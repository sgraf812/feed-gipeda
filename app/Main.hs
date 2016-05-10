import           Control.Logging     as Logging
import           Control.Monad       (join)
import           Data.List           (elemIndex)
import qualified FeedGipeda
import           Options.Applicative
import           System.Directory    (getAppUserDataDirectory)
import           System.FilePath     ((</>))
import           Text.Read           (readMaybe)


endpoint :: ReadM FeedGipeda.Endpoint
endpoint = do
  s <- str
  case elemIndex ':' s of
    Nothing -> readerError "Expected a colon separator"
    Just idx -> do
      let (host, port') = splitAt idx s
      case readMaybe (drop 1 port') of
        Just port -> return (FeedGipeda.Endpoint host port)
        Nothing -> readerError "Port was not integral"


parser :: IO (Parser (IO ()))
parser = do
  defaultConfig <- getAppUserDataDirectory ("feed-gipeda" </> "feed-gipeda.yaml")
  return $
    FeedGipeda.feedGipeda
      <$> option str
            (long "gipeda"
              <> value "gipeda"
              <> metavar "FILEPATH"
              <> help "Custom path to the gipeda executable")
      <*> option str
            (long "config"
              <> value defaultConfig
              <> metavar "FILEPATH"
              <> help ("Path to the YAML file containing a list of watched repositories. Will be watched for changes. Defaults to " ++ defaultConfig ++ "."))
      <*> switch
            (long "one-shot"
              <> help "Fetch updated repositories only once and exit after all new commits have been handled.")
      <*> option auto
            (long "dt"
              <> metavar "SECONDS"
              <> value (60*60)
              <> help "Fetch interval for all repos in seconds. Default to one hour.")
      <*> switch
            (long "check"
              <> help "Verify that the given config file is well-formed and exit")
      <*> optional (option str
            (long "rsync"
              <> metavar "SSH_PATH"
              <> help "ssh path under which to deploy site/ folders with rsync"))
      <*> optional (option endpoint
            (long "master"
              <> metavar "ENDPOINT"
              <> help "Start in master mode, distributing work items. Identified via the given TCP endpoint (ipadress:portnumber)."))
      <*> optional (option endpoint
            (long "slave"
              <> metavar "ENDPOINT"
              <> help "Start in slave mode, requesting work items from a master node. Identified via the given TCP endpoint (ipadress:portnumber)."))
      <*> switch
            (long "verbose"
              <> short 'v'
              <> help "Show log messages intended for debugging")


main :: IO ()
main = Logging.withStdoutLogging $ do
  p <- parser
  join $ execParser $ info (helper <*> p) $
    fullDesc
      <> header "feed-gipeda - watch git repositories and feed benchmark results to gipeda"
      -- <> progDesc "" -- TODO
