module Server
    ( runServer
    ) where

import           Data.Proxy                           (Proxy (..))
import           Network.HTTP.Types.Method
import           Network.Wai
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant

import           Data.Event

runServer :: IO ()
runServer = Warp.run 8000 $ logStdoutDev $ cors app
  where
    app = serve nodeAPI server
    cors = modifyResponse (mapResponseHeaders applyCors)
    applyCors headers =
        ("Access-Control-Allow-Origin",   "*") :
        ("Access-Control-Allow-Methods" , "GET,POST,PUT,DELETE,OPTIONS") :
        ("Access-Control-Allow-Headers",  "Content-Type,Access-Control-Allow-Headers,Authorization,X-Requested-With") :
        headers

server :: Server API
server =
    options
    :<|> createTaxableEvents

options :: String -> Handler NoContent
options _str = pure NoContent

createTaxableEvents :: [Event] -> Handler [TaxableEvent]
createTaxableEvents events = pure (findTaxableEvents events)

type Option = Verb 'OPTIONS 204

type API
    =     Capture "x" String :> Option '[JSON] NoContent
    :<|>  "taxevents" :> ReqBody  '[JSON] [Event] :> Post '[JSON] [TaxableEvent]
    -- =     "taxevents" :> Option '[JSON] NoContent
    -- :<|> "block" :> ReqBody '[JSON] Block :> Post '[JSON] ()
    -- :<|> "transaction" :> ReqBody '[JSON] Transaction :> Post '[JSON] ()
    -- :<|> "transaction-pool" :> Get '[JSON] [Transaction]

nodeAPI :: Proxy API
nodeAPI = Proxy
