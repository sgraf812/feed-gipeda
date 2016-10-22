{-| Defines the entry point of the @feed-gipeda@ executable and the command line
    parsing associated with that.
-}

import           Control.Applicative
import           Control.Logging     as Logging
import           Control.Monad       (join)
import           Data.Functor
import           Data.List           (intercalate, last, init)
import           Data.List.Extra     (split)
import           Data.Monoid         ((<>))
import           FeedGipeda
import           Options.Applicative
import           System.Directory    (getAppUserDataDirectory)
import           System.FilePath     ((</>))
import           Text.Read           (readMaybe)


paths :: FilePath -> Parser Paths
paths defaultConfig =
  Paths
    <$> option str
          (long "config"
            <> value defaultConfig
            <> metavar "FILEPATH"
            <> help ("Path to the YAML file containing a list of watched repositories. Will be watched for changes. Defaults to " ++ defaultConfig ++ "."))
    <*> option str
          (long "gipeda"
            <> value "gipeda"
            <> metavar "FILEPATH"
            <> help "Custom path to the gipeda executable")


cmd :: Parser Command
cmd =
  flag'
    Check
    (long "check"
      <> help "Verify that the given config file is well-formed and exit")
  <|>
  Build
    <$> option
          (WatchForChanges . fromIntegral <$> auto)
          (long "watch"
            <> metavar "SECONDS"
            <> value Once
            <> help "Don't quit when done, watch the config file for changes and refetch watched repositories every SECONDS seconds and benchmark new commits.")
    <*> option
          (fromIntegral <$> auto)
          (long "timeout"
            <> metavar "SECONDS"
            <> value (fromIntegral (30*60))
            <> help "Timeout for a benchmark run. A slave is canceled if it needs more than SECONDS seconds. Defaults to 30 minutes.")


deployment :: Parser Deployment
deployment =
  option
    (Deploy <$> str)
    (long "deploy-to"
      <> metavar "SSH_PATH"
      <> value NoDeployment
      <> help "ssh or local path under which to deploy site/ folders with rsync")


slave :: ReadM ProcessRole
slave = do
  s <- str
  case split (== ':') s of
    sport : rest | length rest > 1 -> -- support IPv6
      case (readMaybe sport, readMaybe mport) of
        (Just sp, Just mp) -> return (Slave sp (Endpoint mhost mp))
        (Nothing, _)       -> readerError "Slave port was not integral"
        (_, Nothing)       -> readerError "Master port was not integral"
      where
        mport = last rest
        mhost = intercalate ":" (init rest)
    _ -> readerError "Expected 3 sections separated by a colon"

processRole :: Parser ProcessRole
processRole =
  impl
    <$> optional (option auto
          (long "master"
            <> metavar "PORT"
            <> help "Start in master mode, distributing work items. Identified via the given TCP port number."))
    <*> optional (option slave
          (long "slave"
            <> metavar "PORT:HOST:PORT"
            <> help "Start in slave mode, requesting work items from a master node. Identified via the given local slave port and the name and port of the master node."))
  where
    impl Nothing Nothing = Both 1337 1338
    impl (Just port) Nothing = Master port
    impl Nothing (Just s) = s
    impl (Just master) (Just (Slave slave ep)) =
      if port ep == master && host ep `elem` ["localhost", "127.0.0.1", "::1"]
      then Both master slave
      else error "Contradiction in --master and --slave args!" -- not proud of this


verbosity :: Parser Verbosity
verbosity =
  flag
    NotVerbose
    Verbose
    (long "verbose"
      <> short 'v'
      <> help "Show log messages intended for debugging")


parser :: IO (Parser (IO ()))
parser = do
  defaultConfig <- getAppUserDataDirectory ("feed-gipeda" </> "feed-gipeda.yaml")
  return $
    FeedGipeda.feedGipeda
      <$> paths defaultConfig
      <*> cmd
      <*> deployment
      <*> processRole
      <*> verbosity


main :: IO ()
main = Logging.withStdoutLogging $ do
  p <- parser
  join $ execParser $ info (helper <*> p) $
    fullDesc
      <> header "feed-gipeda - watch git repositories and feed benchmark results to gipeda"
      -- <> progDesc "" -- TODO
