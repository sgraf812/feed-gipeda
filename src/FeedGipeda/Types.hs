{-| The types for configuring @feedGipeda@. Correspond to the different modes
    of operation that are possible, but in a type-safe way.
-}

module FeedGipeda.Types where


import           Data.Time (NominalDiffTime)


-- | Important file paths for the master node.
data Paths
  = Paths
  { configFile :: FilePath
  -- ^ Lists the repositories to clone in YAML markup. See <https://github.com/sgraf812/feed-gipeda#config-file the README>.
  , gipeda     :: FilePath
  -- ^ Path to the gipeda executable if not in @$PATH@.
  } deriving (Show, Eq)


-- | An IP endpoint, or rather some string and some integer delimited by a colon.
data Endpoint
  = Endpoint
  { host :: String
  , port :: Int
  } deriving (Show, Eq)


type Timeout
  = NominalDiffTime


-- | feed-gipeda can either check config file syntax or build the sites.
data Command
  = Check
  | Build BuildMode Timeout
  deriving (Show, Eq)


-- | Whether feed-gipeda should exit when done regenerating
data BuildMode
  = Once
  {-^ Don't watch the config file or repositories for updates, exit immediately
      when there are no more commits to benchmark.
  -}
  | WatchForChanges NominalDiffTime
  -- ^ Don't exit; watch config file and repositories for updates.
  deriving (Show, Eq)


-- | Signifies deployment to a location accessible via SSH.
data Deployment
  = NoDeployment
  | Deploy String
  -- ^ A SSH/local directory where to @rsync@ website changes to.
  deriving (Show, Eq)


-- | Whether the current feed-gipeda process is a master node, slave node, or both.
data ProcessRole
  = Master Int
  -- ^ local port
  | Slave Int Endpoint
  -- ^ local port, remote endpoint of the master node
  | Both Int Int
  -- ^ local port for master, local port for slave
  deriving (Show, Eq)

masterEndpoint :: ProcessRole -> Endpoint
masterEndpoint (Master port) = Endpoint "127.0.0.1" port
masterEndpoint (Both masterPort _) = Endpoint "127.0.0.1" masterPort
masterEndpoint (Slave _ ep) = ep


slaveEndpoint :: ProcessRole -> Maybe Endpoint
slaveEndpoint (Slave port _) = Just (Endpoint "127.0.0.1" port)
slaveEndpoint (Both _ slavePort) = Just (Endpoint "127.0.0.1" slavePort)
slaveEndpoint _ = Nothing


isBoth :: ProcessRole -> Bool
isBoth (Both _ _) = True
isBoth _ = False


data Verbosity
  = Verbose
  | NotVerbose
  deriving (Show, Eq)
