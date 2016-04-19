module Worker
  ( benchmark
  , regenerateAndDeploy
  , finalize
  ) where

{-| The 'meat' of the daemon. @work@ calls the @--benchmark@ script for
    new @Benchmark@ items resp. the @--gipeda@ script for new @Regenerate@
    items.
-}

import qualified Assets
import           Control.Monad        (unless, when)
import           Data.Aeson           ((.=))
import qualified Data.Aeson           as Json
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable        (find)
import           Data.List            (elemIndex, stripPrefix)
import           Data.Maybe           (fromMaybe)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import qualified Data.Yaml            as Yaml
import qualified Gipeda
import           GitShell             (SHA)
import           Network.URI          (URI, uriAuthority, uriPath, uriRegName,
                                       uriToString)
import           Repo                 (Repo)
import qualified Repo
import qualified RepoWatcher
import           System.Directory     (copyFile, createDirectoryIfMissing,
                                       doesFileExist, removeFile)
import           System.FilePath      (addTrailingPathSeparator, dropExtension,
                                       dropFileName, takeBaseName, (<.>), (</>))
import           System.IO            (IOMode (WriteMode), hClose, hPutStr,
                                       openFile)
import           System.Process       (cwd, proc, readCreateProcessWithExitCode)


executeIn :: Maybe FilePath -> FilePath -> [String] -> IO String
executeIn cwd executable args = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode (proc executable args) { cwd = cwd } ""
  return stdout


installJsLibs :: FilePath -> FilePath -> IO ()
installJsLibs gipeda project = do
  let
    installScript =
      dropFileName gipeda </> "install-jslibs.sh"

  executeIn (Just project) "sh" [installScript]
  return ()


copyIfNotExists :: FilePath -> FilePath -> FilePath -> IO ()
copyIfNotExists gipeda dst subPath = do
  let
    source =
      dropFileName gipeda </> subPath
    target =
      dst </> subPath

  exists <- doesFileExist target
  unless exists (copyFile source target)


saveSettingsIfNotExists :: FilePath -> Repo -> IO ()
saveSettingsIfNotExists project repo = do
  let
    settingsFile = project </> "settings.yaml"

  settings <- Gipeda.settingsForRepo repo
  exists <- doesFileExist settingsFile
  unless exists (Yaml.encodeFile settingsFile settings)


type SSHSubPathPolicy
  = (Repo -> Bool, Repo -> FilePath)


sshSubPath :: Repo -> FilePath
sshSubPath repo =
  maybe Repo.uniqueName snd (find (\(matches, _) -> matches repo) policies) repo
    where
      policies :: [SSHSubPathPolicy]
      policies =
        [ gitHubPolicy
        , perfHaskellPolicy
        ]

      stripWWW :: String -> String
      stripWWW s =
        fromMaybe s (stripPrefix "www." s)

      matchRegName :: String -> Repo -> Bool
      matchRegName regName =
        maybe False ((== regName) . stripWWW . uriRegName) . uriAuthority . Repo.unRepo

      gitHubPolicy :: SSHSubPathPolicy
      gitHubPolicy =
        -- tail because the path will start a slash
        (matchRegName "github.com", dropExtension . tail . uriPath . Repo.unRepo)

      perfHaskellPolicy :: SSHSubPathPolicy
      perfHaskellPolicy =
        (matchRegName "git.haskell.org", takeBaseName . uriPath . Repo.unRepo)


sshSubPathTestFailures :: [(Repo, String, String)]
sshSubPathTestFailures =
  (map (\(r, e) -> (r, e, sshSubPath r)) . filter isFailure)
    [ (Repo.unsafeFromString "https://github.com/sgraf812/benchmark-test", "sgraf812/benchmark-test")
    , (Repo.unsafeFromString "https://www.github.com/sgraf812/benchmark-test", "sgraf812/benchmark-test")
    , (Repo.unsafeFromString "https://git.haskell.org/sgraf812/benchmark-test", "benchmark-test")
    , (Repo.unsafeFromString "https://bitbucket.org/sgraf812/benchmark-test", "benchmark-test-2921196486978765793")
    ]
    where
      isFailure (repo, expected) =
        sshSubPath repo /= expected


generateMapping :: FilePath -> Set Repo -> IO ()
generateMapping file repos =
  LBS.writeFile file (Json.encode (Json.object (foldMap (\repo -> [Text.pack (sshSubPath repo) .= Repo.uri repo]) repos)))


sshUriPath :: String -> FilePath
sshUriPath sshUri =
  case elemIndex ':' (reverse sshUri) of
    Nothing -> sshUri -- Assume it's a local file path
    Just n -> drop (length sshUri - n) sshUri


rsyncSite :: Set Repo -> Repo -> Maybe String -> IO ()
rsyncSite repos repo = maybe (return ()) $ \remoteDir -> do
  projectDir <- Repo.projectDir repo
  -- we need the trailing path separator, otherwise it will add a site
  -- sub directory.
  -- The rsync-path parameter is used for a little hack that ensures the
  -- remote directory acutally exists on the remote machine.
  -- Otherwise, we couldn't cope with nested sshSubPaths.
  -- -a: archive mode (many different flags), -v verbose, -z compress
  executeIn Nothing "rsync"
    [ "-avz"
    , "--rsync-path=mkdir -p " ++ (sshUriPath remoteDir </> sshSubPath repo) ++ " && rsync"
    , addTrailingPathSeparator (projectDir </> "site")
    , remoteDir </> sshSubPath repo
    ]

  generateMapping "sites.json" repos
  executeIn Nothing "rsync"
    [ "-avz"
    , "sites.json"
    , remoteDir </> "sites.json"
    ]

  BS.writeFile "default_index.html" Assets.defaultIndexHtml
  executeIn Nothing "rsync"
    [ "-avz"
    , "--ignore-existing" -- so that users can provide their own index.html
    , "default_index.html"
    , remoteDir </> "index.html"
    ]

  return ()


finalize :: FilePath -> Maybe String -> Set Repo -> Repo -> SHA -> String -> IO ()
finalize gipeda rsyncPath repos repo commit result = do
  results <- Repo.resultsDir repo
  writeFile (results </> commit <.> "csv") result
  -- Regenerate and deploy gipeda assets as necessary
  (unfinished, _) <- RepoWatcher.commitDiff repo
  when (Set.null unfinished) (regenerateAndDeploy gipeda rsyncPath repos repo)


benchmark :: FilePath -> Repo -> SHA -> IO String
benchmark cloben repo commit = do
  -- Handle a fresh commit by benchmarking
  clone <- Repo.cloneDir repo
  putStrLn ("New commit " ++ Repo.uri repo ++ "@" ++ commit)
  executeIn Nothing cloben [Repo.uri repo, commit]


regenerateAndDeploy :: FilePath -> Maybe String -> Set Repo -> Repo -> IO ()
regenerateAndDeploy gipeda rsyncPath repos repo = do
  project <- Repo.projectDir repo
  putStrLn ("Regenerating " ++ project)
  createDirectoryIfMissing True (project </> "site" </> "js")
  installJsLibs gipeda project
  saveSettingsIfNotExists project repo
  copyIfNotExists gipeda project ("site" </> "index.html")
  copyIfNotExists gipeda project ("site" </> "js" </> "gipeda.js")
  executeIn (Just project) gipeda ["-j"]
  rsyncSite repos repo rsyncPath
  return ()
