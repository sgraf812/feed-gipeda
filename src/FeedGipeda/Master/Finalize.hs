{-| The meat of the master node: Calls @gipeda@ and optionally deploys the
    website via @rsync@.

    Upon deployment, repositories are mapped to URLs via specific policies
    verified in @sshSubPathTestFailures@.
-}

module FeedGipeda.Master.Finalize
  ( regenerateAndDeploy
  ) where


import           Control.Logging      as Logging
import           Control.Monad        (unless, void, when)
import           Data.Aeson           ((.=))
import qualified Data.Aeson           as Json
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable        (find)
import           Data.List            (elemIndex, stripPrefix)
import           Data.Maybe           (fromMaybe, isJust, isNothing)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import qualified Data.Yaml            as Yaml
import qualified FeedGipeda.Assets    as Assets
import qualified FeedGipeda.Gipeda    as Gipeda
import           FeedGipeda.GitShell  (SHA)
import qualified FeedGipeda.GitShell  as GitShell
import           FeedGipeda.Prelude
import           FeedGipeda.Repo      (Repo)
import qualified FeedGipeda.Repo      as Repo
import           FeedGipeda.Types     (Deployment (..))
import           Network.URI          (URI, uriAuthority, uriPath, uriRegName,
                                       uriToString)
import           System.Directory     (copyFile, createDirectoryIfMissing,
                                       doesFileExist)
import           System.Exit          (ExitCode (..))
import           System.FilePath      (addTrailingPathSeparator, dropExtension,
                                       dropFileName, takeBaseName, (</>))
import           System.Process       (cwd, proc, readCreateProcessWithExitCode)


executeIn :: Maybe FilePath -> FilePath -> [String] -> IO String
executeIn cwd executable args = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode (proc executable args) { cwd = cwd } ""
  logDebug (takeBaseName executable ++ ": " ++ show exitCode)
  case exitCode of
    ExitFailure _ -> logDebug stderr >> logDebug stdout
    _ -> return ()
  -- That's too much even for debug
  --logDebug "stdout:"
  --mapM_ logDebug (take 20 (lines stdout))
  --logDebug "stderr:"
  --mapM_ logDebug ((take 20 . lines) stderr)
  return stdout


{-| @regenerateAndDeploy gipeda rsyncPath repos repo@ updates the @site/@ sub
    folder by calling @gipeda@ in @repo@s @projectDir@. That also updates
    the @backlog.txt@, which will possibly kick off other benchmarks.

    After the site has been regenerated, the changes are deployed via @rsync@
    to @remoteDir@, if present. The sub directory to which the site is synced
    follow a mapping which should satisfy the tests in @sshSubPathTestFailures@.
-}
regenerateAndDeploy :: FilePath -> Deployment -> Set Repo -> Repo -> IO ()
regenerateAndDeploy gipeda deployment repos repo = do
  project <- Repo.projectDir repo
  logInfo ("Regenerating " ++ Repo.uri repo ++ " (" ++ Repo.uniqueName repo ++ ")")
  clone <- Repo.cloneDir repo
  first <- GitShell.firstCommit clone
  if isJust first
    then do
      saveSettings repo
      executeIn (Just project) gipeda ["-j", "--keep-going", "--always-make"]
      rsyncSite repos repo deployment
      return ()
    else
      logInfo "There were no commits"


saveSettings :: Repo -> IO ()
saveSettings repo = do
  settingsFile <- Repo.settingsFile repo
  settings <- Gipeda.settingsForRepo repo
  Yaml.encodeFile settingsFile settings


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
generateMapping file =
  LBS.writeFile file . Json.encode . Json.object . map sshMapping . Set.toList
    where
      sshMapping repo =
        Text.pack (sshSubPath repo) .= Repo.uri repo


parseSSHUri :: String -> (Maybe String, FilePath)
parseSSHUri sshUri =
  case elemIndex ':' (reverse sshUri) of
    Nothing -> (Nothing, sshUri) -- Assume it's a local file path
    Just n ->
      let
        n' = length sshUri - n
      in
        (Just (take (n' - 1) sshUri), drop n' sshUri)


rsyncSite :: Set Repo -> Repo -> Deployment -> IO ()
rsyncSite repos repo NoDeployment = return ()
rsyncSite repos repo (Deploy remoteDir) = do
  projectDir <- Repo.projectDir repo
  -- we need the trailing path separator, otherwise it will add a site
  -- sub directory.
  -- The rsync-path parameter is used for a little hack that ensures the
  -- remote directory acutally exists on the remote machine.
  -- Otherwise, we couldn't cope with nested sshSubPaths.
  -- -a: archive mode (many different flags), -v verbose, -z compress
  logInfo "rsyncing"
  let (sshPart, filePart) = parseSSHUri remoteDir

  case sshPart of
    Nothing -> createDirectoryIfMissing True (filePart </> sshSubPath repo)
    Just uri ->
      void $ executeIn Nothing "ssh" [uri, "mkdir -p " ++ filePart </> sshSubPath repo]

  executeIn Nothing "rsync"
    [ "-avz"
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
