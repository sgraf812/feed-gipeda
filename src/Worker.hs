module Worker
  ( WorkItem (..)
  , work
  ) where

{-| The 'meat' of the daemon. @work@ calls the @--benchmark@ script for
    new @Benchmark@ items resp. the @--gipeda@ script for new @Regenerate@
    items.
-}

import           Control.Monad    (unless, when)
import           Data.Foldable    (find)
import           Data.List        (stripPrefix)
import           Data.Maybe       (fromMaybe)
import qualified Data.Yaml        as Yaml
import qualified GipedaSettings
import           GitShell         (SHA)
import           Network.URI      (URI, uriAuthority, uriPath, uriRegName,
                                   uriToString)
import           Repo             (Repo)
import qualified Repo
import           System.Directory (copyFile, createDirectoryIfMissing,
                                   doesFileExist, removeFile)
import           System.FilePath  (addTrailingPathSeparator, dropExtension,
                                   dropFileName, takeBaseName, (<.>), (</>))
import           System.IO        (IOMode (WriteMode), hClose, hPutStr,
                                   openFile)
import           System.Process   (cwd, proc, readCreateProcessWithExitCode)


data WorkItem
  = Benchmark FilePath Repo SHA
  | Regenerate FilePath Repo String
  deriving (Eq, Ord, Show)


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

  settings <- GipedaSettings.settingsForRepo repo
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



rsyncSite :: Repo -> String -> IO ()
rsyncSite repo remoteDir = do
  projectDir <- Repo.projectDir repo

  let
    siteDir =
      projectDir </> "site"

    rsync :: String -> IO ()
    rsync remoteDir = do
      executeIn Nothing "rsync"
        -- we need the trailing path separator, otherwise it will add a site
        -- sub directory
        ["-a", addTrailingPathSeparator siteDir, remoteDir </> sshSubPath repo]
      return ()

  case remoteDir of
    [] -> return ()
    _ -> rsync remoteDir


work :: WorkItem -> IO ()
work (Benchmark cloben repo commit) = do
  -- Handle a fresh commit by benchmarking
  clone <- Repo.cloneDir repo
  results <- Repo.resultsDir repo
  let
    resultFile = results </> commit <.> "csv"

  exists <- doesFileExist resultFile
  createDirectoryIfMissing True results
  when exists (putStrLn "Benchmarking a commit for which there already is a file. This is a failure, but nothing critic.")
  putStrLn ("New commit " ++ Repo.uri repo ++ "@" ++ commit)
  executeIn Nothing cloben [clone, commit] >>= writeFile resultFile
work (Regenerate gipeda repo rsyncPath) = do
  -- Regenerate the site by re-running gipeda in the projectDir.
  project <- Repo.projectDir repo
  createDirectoryIfMissing True (project </> "site" </> "js")
  installJsLibs gipeda project
  saveSettingsIfNotExists project repo
  copyIfNotExists gipeda project ("site" </> "index.html")
  copyIfNotExists gipeda project ("site" </> "js" </> "gipeda.js")
  executeIn (Just project) gipeda ["-j"]
  rsyncSite repo rsyncPath
  return ()
