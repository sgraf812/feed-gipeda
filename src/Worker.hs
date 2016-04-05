module Worker
  ( WorkItem (..)
  , work
  ) where

{-| The 'meat' of the daemon. @work@ calls the @--benchmark@ script for
    new @Benchmark@ items resp. the @--gipeda@ script for new @Regenerate@
    items.
-}

import           Control.Exception (mask, onException)
import           Control.Monad     (unless)
import qualified Data.Yaml         as Yaml
import qualified GipedaSettings
import           GitShell          (SHA)
import           Repo              (Repo)
import qualified Repo
import           System.Directory  (copyFile, createDirectoryIfMissing,
                                    doesFileExist, removeFile)
import           System.FilePath   (dropFileName, (<.>), (</>))
import           System.IO         (IOMode (WriteMode), hClose, hPutStr,
                                    openFile)
import           System.Process    (cwd, proc, readCreateProcessWithExitCode)


data WorkItem
  = Benchmark FilePath Repo SHA
  | Regenerate FilePath Repo
  deriving (Eq, Ord, Show)


writeFileDeleteOnException :: FilePath -> IO String -> IO ()
writeFileDeleteOnException path action =
  -- remove the file only when an exception happened, but close the handle
  -- at all costs.
  mask $ \restore -> do
    handle <- openFile path WriteMode
    restore (action >>= hPutStr handle) `onException` (hClose handle >> removeFile path)
    hClose handle


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


work :: WorkItem -> IO ()
work (Benchmark cloben repo commit) = do
  -- Handle a fresh commit by benchmarking
  clone <- Repo.cloneDir repo
  results <- Repo.resultsDir repo
  let
    resultFile = results </> commit <.> "csv"

  exists <- doesFileExist resultFile
  createDirectoryIfMissing True results
  unless exists $
    -- A poor man's mutex. Delete the file only if there occurred an error while
    -- benchmarking (e.g. ctrl-c). Otherwise a re-run would not benchmark
    -- the touched commit.
    writeFileDeleteOnException resultFile $ do
      putStrLn ("New commit " ++ Repo.uri repo ++ "@" ++ commit)
      executeIn Nothing cloben [clone, commit]
work (Regenerate gipeda repo) = do
  -- Regenerate the site by re-running gipeda in the projectDir.
  project <- Repo.projectDir repo
  createDirectoryIfMissing True (project </> "site" </> "js")
  installJsLibs gipeda project
  saveSettingsIfNotExists project repo
  copyIfNotExists gipeda project ("site" </> "index.html")
  copyIfNotExists gipeda project ("site" </> "js" </> "gipeda.js")
  executeIn (Just (dropFileName gipeda)) gipeda ["-C", project, "-j"]
  return ()
