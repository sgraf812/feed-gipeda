{-| Functions for shelling out to @git@ to work with git repositories.
-}


module FeedGipeda.GitShell
  ( isRepositoryRoot
  , fetch
  , allCommits
  , firstCommit
  , remoteRepo
  , showHead
  , sync
  , SHA
  ) where


import           Control.Monad      (void)
import           Data.Char          (isSpace)
import           Data.Functor
import           Data.Maybe         (listToMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           FeedGipeda.Prelude
import           FeedGipeda.Repo    (Repo)
import qualified FeedGipeda.Repo    as Repo
import           System.Directory   (createDirectoryIfMissing)
import           System.Exit        (ExitCode (..))
import           System.Process     (readProcessWithExitCode,
                                     showCommandForUser)


type SHA
  = String


formatGitArgs :: Maybe FilePath -> String -> [String] -> [String]
formatGitArgs local command args =
  case local of
    Nothing -> command : args
    Just l -> ["-C", l, command] ++ args


git :: Maybe FilePath -> String -> [String] -> IO (ExitCode, String, String)
git local command args =
  readProcessWithExitCode "git" (formatGitArgs local command args) ""


gitLoggingErrors :: Maybe FilePath -> String -> [String] -> IO (Maybe String)
gitLoggingErrors repo command args = do
  (exitCode, stdout, stderr) <- git repo command args
  case exitCode of
    ExitSuccess -> return (Just stdout)
    ExitFailure code -> do
      logWarn (showCommandForUser "git" (formatGitArgs repo command args))
      logWarn "stdout:"
      logWarn stdout
      logWarn "stderr:"
      logWarn stderr
      return Nothing


isRepositoryRoot :: FilePath -> IO Bool
isRepositoryRoot path = do
  (_, stdout, _)<- git (Just path) "rev-parse" ["--git-dir"]
  -- testing for ".git" and "." (bare repo) should be good enough.
  (return . maybe False (`elem` [".git", "."]) . listToMaybe . lines) stdout


mirror :: Repo -> FilePath -> IO ()
mirror repo path =
  void (gitLoggingErrors Nothing "clone" ["--mirror", "--quiet", Repo.uri repo, path])


remoteRepo :: FilePath -> IO Repo
remoteRepo path = do
  ret <- gitLoggingErrors (Just path) "ls-remote" ["--get-url", "origin"]
  case ret of
    Nothing -> return (Repo.unsafeFromString "https://error.org/err") -- If this ever happens, all bets are off
    Just stdout -> return (Repo.unsafeFromString (init stdout)) -- strip the \n with init


fetch :: FilePath -> IO ()
fetch path =
  void (gitLoggingErrors (Just path) "fetch" ["--quiet"])


{-| @sync repo@ tries to fetch updates from the remote @repo@ or creates a
    mirror of @repo@ if there isn't already a local clone present.
-}
sync :: Repo -> IO ()
sync repo = do
  path <- Repo.cloneDir repo
  hasClone <- isRepositoryRoot path
  if hasClone
    then fetch path
    else do
      createDirectoryIfMissing True path
      mirror repo path


allCommits :: FilePath -> IO (Set SHA)
allCommits path =
  Set.fromList <$> gitLogImpl path []


firstCommit :: FilePath -> IO (Maybe SHA)
firstCommit path =
  listToMaybe <$> gitLogImpl path ["--reverse"]


showHead :: FilePath -> FilePath -> IO (Maybe String)
showHead repo file = do
  (exitCode, stdout, stderr) <- git (Just repo) "show" ["HEAD:" ++ file]
  case exitCode of
    ExitSuccess -> return (Just stdout)
    ExitFailure _ -> return Nothing


gitLogImpl :: FilePath -> [String] -> IO [SHA]
gitLogImpl path args = do
  ret <- gitLoggingErrors (Just path) "log" ("--format=format:%H" : args)
  case ret of
    Nothing -> return []
    Just stdout -> return (filter (not . null) (lines stdout))
