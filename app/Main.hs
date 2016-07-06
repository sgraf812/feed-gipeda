{-| Defines the entry point of the @feed-gipeda@ executable and the command line
    parsing associated with that.
-}

import           Control.Applicative
import           Control.Logging     as Logging
import           Control.Monad       (join)
import           Data.Functor
import           Data.List           (elemIndex)
import           FeedGipeda
import           Options.Applicative
import           System.Directory    (getAppUserDataDirectory)
import           System.FilePath     ((</>))
import           Text.Read           (readMaybe)


endpoint :: ReadM Endpoint
endpoint = do
  s <- str
  case elemIndex ':' s of
    Nothing -> readerError "Expected a colon separator"
    Just idx -> do
      let (host, port') = splitAt idx s
      case readMaybe (drop 1 port') of
        Just port -> return (Endpoint host port)
        Nothing -> readerError "Port was not integral"


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


processRole :: Parser ProcessRole
processRole =
  impl
    <$> optional (option endpoint
          (long "master"
            <> metavar "ENDPOINT"
            <> help "Start in master mode, distributing work items. Identified via the given TCP endpoint (ipadress:portnumber)."))
    <*> optional (option endpoint
          (long "slave"
            <> metavar "ENDPOINT"
            <> help "Start in slave mode, requesting work items from a master node. Identified via the given TCP endpoint (ipadress:portnumber)."))
  where
    impl Nothing Nothing = Both (Endpoint "localhost" 1337) (Endpoint "localhost" 1338)
    impl (Just ep) Nothing = Master ep
    impl Nothing (Just ep) = Slave ep
    impl (Just mep) (Just sep) = Both mep sep


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
