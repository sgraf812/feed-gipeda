module Worker
  ( benchmark
  , regenerateAndDeploy
  , finalize
  ) where

{-| The 'meat' of the daemon. @work@ calls the @--benchmark@ script for
    new @Benchmark@ items resp. the @--gipeda@ script for new @Regenerate@
    items.
-}

import           Control.Monad    (unless, when)
import           Data.Foldable    (find)
import           Data.List        (stripPrefix)
import           Data.Maybe       (fromMaybe)
import qualified Data.Set         as Set
import qualified Data.Yaml        as Yaml
import qualified Gipeda
import           GitShell         (SHA)
import           Network.URI      (URI, uriAuthority, uriPath, uriRegName,
                                   uriToString)
import           Repo             (Repo)
import qualified Repo
import qualified RepoWatcher
import           System.Directory (copyFile, createDirectoryIfMissing,
                                   doesFileExist, removeFile)
import           System.FilePath  (addTrailingPathSeparator, dropExtension,
                                   dropFileName, takeBaseName, (<.>), (</>))
import           System.IO        (IOMode (WriteMode), hClose, hPutStr,
                                   openFile)
import           System.Process   (cwd, proc, readCreateProcessWithExitCode)


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



rsyncSite :: Repo -> Maybe String -> IO ()
rsyncSite repo = maybe (return ()) $ \remoteDir -> do
  projectDir <- Repo.projectDir repo
  -- we need the trailing path separator, otherwise it will add a site
  -- sub directory
  executeIn Nothing "rsync"
    [ "-a"
    , addTrailingPathSeparator (projectDir </> "site")
    , remoteDir </> sshSubPath repo
    ]

  return ()



finalize :: FilePath -> Maybe String -> Repo -> SHA -> String -> IO ()
finalize gipeda rsyncPath repo commit result = do
  results <- Repo.resultsDir repo
  writeFile (results </> commit <.> "csv") result
  -- Regenerate and deploy gipeda assets as necessary
  (unfinished, _) <- RepoWatcher.commitDiff repo
  when (Set.null unfinished) (regenerateAndDeploy gipeda rsyncPath repo)


benchmark :: FilePath -> Repo -> SHA -> IO String
benchmark cloben repo commit = do
  -- Handle a fresh commit by benchmarking
  clone <- Repo.cloneDir repo
  results <- Repo.resultsDir repo
  let
    resultFile = results </> commit <.> "csv"

  exists <- doesFileExist resultFile
  createDirectoryIfMissing True results
  when exists $
    putStrLn "Benchmarking a commit for which there already is a file. This is a bug, but nothing critical."
  putStrLn ("New commit " ++ Repo.uri repo ++ "@" ++ commit)
  executeIn Nothing cloben [clone, commit]


regenerateAndDeploy :: FilePath -> Maybe String -> Repo -> IO ()
regenerateAndDeploy gipeda rsyncPath repo = do
  project <- Repo.projectDir repo
  putStrLn ("Regenerating " ++ project)
  createDirectoryIfMissing True (project </> "site" </> "js")
  installJsLibs gipeda project
  saveSettingsIfNotExists project repo
  copyIfNotExists gipeda project ("site" </> "index.html")
  copyIfNotExists gipeda project ("site" </> "js" </> "gipeda.js")
  executeIn (Just project) gipeda ["-j"]
  rsyncSite repo rsyncPath
  return ()
