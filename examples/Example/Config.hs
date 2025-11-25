{-# LANGUAGE QuasiQuotes #-}

module Example.Config where

import Data.Maybe (fromMaybe)
import Effectful
import Effectful.Environment
import Effectful.Exception
import Network.Connection
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPS
import Network.TLS
import Network.URI (parseURI)
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Effect.OAuth2 (Config (..), Token (..))
import Web.Hyperbole.Effect.OAuth2 qualified as OAuth2
import Web.Hyperbole.Effect.GenRandom

data App
data AppConfig = AppConfig
  { endpoint :: Endpoint App
  , manager :: HTTP.Manager
  , oauth :: OAuth2.Config
  }

getAppConfigEnv :: (IOE :> es, Environment :> es, GenRandom :> es) => Eff es AppConfig
getAppConfigEnv = do
  endpoint <- lookupEnvEndpoint "APP_ENDPOINT" -- default to localhost
  manager <- HTTPS.newTlsManagerWith $ HTTPS.mkManagerSettings (TLSSettingsSimple True False False defaultSupported) Nothing
  cfg <- liftIO $ runEff $ runRandom $ runEnvironment $ OAuth2.getConfigEnv
  pure $
    AppConfig
      { endpoint = fromMaybe (Endpoint [uri|http://localhost:3000|]) endpoint
      , manager
      , oauth = cfg
      }

type Key = String

data ConfigError
  = BadEnv Key
  deriving (Show, Exception)

lookupEnvEndpoint :: (Environment :> es) => Key -> Eff es (Maybe (Endpoint a))
lookupEnvEndpoint k = do
  mstr <- lookupEnv k
  pure $ parseEndpoint mstr
 where
  parseEndpoint mstr = do
    input <- mstr
    url <- parseURI input
    pure $ Endpoint url

-- In a real app this would be read from ENV. See OAuth2.initConfigEnv
dummyOAuthConfig :: OAuth2.Config
dummyOAuthConfig =
  Config
    { clientId = Token "dummy client id"
    , clientSecret = Token "dummy client secret"
    , authorize = Endpoint [uri|https://oauth-mock.mock.beeceptor.com/oauth/authorize|]
    , token = Endpoint [uri|https://oauth-mock.mock.beeceptor.com/oauth/token/github|]
    }
