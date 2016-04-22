{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

import Control.Lens
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Data.Aeson
import Control.Monad.Trans.Except

type UserAPI = "voting" :> (PluralityPut :<|> PluralityGet) -- :<|> ApprovalGet :<|> ApprovalPut)
type GetElection = Capture "election-name" String :> Get '[JSON] String 
type PostElection = Capture "election-name" String :> (Post '[JSON] String)
type PluralityGet = "plurality" :> GetElection
type PluralityPut = "plurality" :> PostElection
{-type ApprovalGet = "approval" :> ElectionName
type Range = "range" :> ElectionName
type Schulze = "schulze" :> ElectionName -}

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = votePlurality :<|> return undefined -- users

app :: Application
app = serve userAPI server

main :: IO ()
main =  run 8080 app

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

data PluralityJSON = Plurality String

votePlurality :: String -> ExceptT ServantErr IO String
votePlurality electionName = undefined

countPlurality :: String -> ExceptT ServantErr IO String
countPlurality electionName = undefined
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
 
data ApprovalJSON = Approval [String]

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

data RangeVote = RangeVote {
  name :: String,
  rating :: Int }
data RangeJSON = Range [RangeVote]


