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


-- | feed-gipeda can either check config file syntax or build the sites.
data Command
  = Check
  | Build BuildMode
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
  = Master Endpoint
  | Slave Endpoint
  | Both Endpoint Endpoint
  deriving (Show, Eq)


masterEndpoint :: ProcessRole -> Maybe Endpoint
masterEndpoint (Master ep) = Just ep
masterEndpoint (Both ep _) = Just ep
masterEndpoint _ = Nothing


slaveEndpoint :: ProcessRole -> Maybe Endpoint
slaveEndpoint (Slave ep) = Just ep
slaveEndpoint (Both _ ep) = Just ep
slaveEndpoint _ = Nothing


isBoth :: ProcessRole -> Bool
isBoth (Both _ _) = True
isBoth _ = False


data Verbosity
  = Verbose
  | NotVerbose
  deriving (Show, Eq)
