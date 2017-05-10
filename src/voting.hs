{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens hiding ((<.>))
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Data.UUID
import Data.UUID.V4
import Data.List 
import Data.Ratio
import Data.Ord
import Data.Monoid 
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Aeson
import GHC.Generics
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Database.Cassandra.CQL hiding (Server)

type UserAPI = "voting" :> (PluralityPut :<|> PluralityGet 
                        :<|> ApprovalPut :<|> ApprovalGet
                        :<|> RangePut :<|> RangeGet)

type GetElection = Capture "election-name" T.Text :> Get '[JSON] T.Text 
type PostElection vote = Capture "election-name" T.Text :> ReqBody '[JSON] vote :> (Post '[JSON] T.Text)

type PluralityGet = "plurality" :> GetElection
type PluralityPut = "plurality" :> PostElection T.Text

type ApprovalGet = "approval" :> GetElection 
type ApprovalPut = "approval" :> PostElection ApprovalJSON

type RangeGet = "range" :> GetElection 
type RangePut = "range" :> PostElection RangeJSON

--type Schulze = "schulze" :> ElectionName

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = votePlurality :<|> countPlurality 
    :<|> voteApproval  :<|> countApproval 
    :<|> voteRange     :<|> countRange

app :: Application
app = serve userAPI server

main :: IO ()
main =  run 8080 app


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

localHost = [("localhost", "9042")]

localPool = newPool localHost (Keyspace "voting") Nothing

runCas' :: MonadIO m => Cas a -> m a 
runCas' act = liftIO $ do
  pool <- localPool
  runCas pool act

writeCas query values = runCas' $ executeWrite QUORUM query values

readCas query values = runCas' $ executeRows ALL query values

createTable schema = runCas' $ executeSchema ALL schema ()

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

pluralityCreate :: Query Schema () ()
pluralityCreate = "CREATE TABLE voting.plurality (voteId uuid, election text, candidate text, PRIMARY KEY (election, voteId));"

votePlurality :: T.Text -> T.Text -> ExceptT ServantErr IO T.Text
votePlurality electionName vote = do
  uuid <- liftIO $ nextRandom
  writeCas pluralityWrite (uuid, electionName, vote)
  return "Ok"

pluralityWrite :: Query Write (UUID, T.Text, T.Text) ()
pluralityWrite = "insert into plurality (voteId, election, candidate) values (?,?,?)"

countPlurality :: T.Text -> ExceptT ServantErr IO T.Text
countPlurality electionName = do
  votes <- readCas pluralityRead electionName
  return $ runPlurality votes

pluralityRead :: Query Rows T.Text T.Text
pluralityRead = "select candidate from plurality where election=?"

runPlurality :: [T.Text] -> T.Text
runPlurality = head . maximumBy (comparing length) . group . sort


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
 
data ApprovalJSON = Approval { votes :: [T.Text] } deriving (Generic, Show)

instance ToJSON ApprovalJSON where
instance FromJSON ApprovalJSON where

approvalCreate :: Query Schema () ()
approvalCreate = "CREATE TABLE voting.approval (voteId uuid, election text, candidates list<text>, PRIMARY KEY (election, voteId));"

voteApproval :: T.Text -> ApprovalJSON -> ExceptT ServantErr IO T.Text
voteApproval electionName vote = do
  uuid <- liftIO $ nextRandom
  writeCas approvalWrite (uuid, electionName, votes vote)
  return "Ok"

approvalWrite :: Query Write (UUID, T.Text, [T.Text]) ()
approvalWrite = "insert into approval (voteId, election, candidates) values (?,?,?)"

countApproval :: T.Text -> ExceptT ServantErr IO T.Text
countApproval electionName = do
  votes <- readCas approvalRead electionName
  return $ runApproval votes

approvalRead :: Query Rows T.Text [T.Text]
approvalRead = "select candidates from approval where election=?"

runApproval :: [[T.Text]] -> T.Text
runApproval = head . maximumBy (comparing length) . group . sort . concat

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

data RangeVote = RangeVote {
  name :: T.Text,
  rating :: Int } deriving (Generic, Show)
instance ToJSON RangeVote where
instance FromJSON RangeVote where

data RangeJSON = RangeJSON [RangeVote] deriving (Generic, Show)
instance ToJSON RangeJSON where
instance FromJSON RangeJSON where

rangeJSONToMap (RangeJSON votes) = M.fromAscList . map toTup $ votes
  where
    toTup (RangeVote name rating) = (name, rating)

rangeCreate :: Query Schema () ()
rangeCreate = "CREATE TABLE voting.range (voteId uuid, election text, ratings map<text,int>, PRIMARY KEY (election, voteId));"

voteRange :: T.Text -> RangeJSON -> ExceptT ServantErr IO T.Text
voteRange electionName ratings = do
  uuid <- liftIO nextRandom
  writeCas rangeWrite (uuid, electionName, rangeJSONToMap ratings)
  return "OK"

rangeWrite :: Query Write (UUID, T.Text, M.Map T.Text Int) ()
rangeWrite = "insert into range (roteId, election, ratings) values (?, ?, ?)"

countRange :: T.Text -> ExceptT ServantErr IO T.Text
countRange electionName = do
  votes <- readCas rangeRead electionName
  return $ runRange votes

rangeRead :: Query Rows T.Text (M.Map T.Text Int)
rangeRead = "select ratings from range where election=?"

-- One ambiguity in range voting is how to deal with abstentions, so an unknown candidate with
-- perfect scores from a small minority of voters doesn't win.
-- Here, treat an abstention as 1/10th of a zero vote 
runRange :: [M.Map T.Text Int] -> T.Text
runRange ratings = fst . maximumBy (comparing $ score . snd) $ M.toAscList combinedRatings
  where
    
    totalVotes = fromIntegral $ length ratings
    ratingsWithCount = map (M.map $ (, Sum 1) . Sum) ratings
    combinedRatings = foldl' (M.unionWith (<>)) M.empty ratingsWithCount
    score (Sum total, Sum numVotes) = fromIntegral total / (fromIntegral numVotes +  (totalVotes - fromIntegral numVotes / 10))

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

data SchulzeJSON = Schulze { schulzeVotes :: [T.Text] } deriving (Generic, Show)

instance ToJSON SchulzeJSON where
instance FromJSON SchulzeJSON where

schulzeCreate :: Query Schema () ()
schulzeCreate = "CREATE TABLE voting.schulze (voteId uuid, election text, rankings map<text, int>, PRIMARY KEY (election, voteId));"
{-
schulzeJSONToMap = undefined

voteSchulze :: T.Text -> SchulzeJSON -> ExceptT ServantErr IO T.Text
voteSchulze electionName rankings = do
  uuid <- liftIO $ nextRandom
  writeCas schulzeWrite (uuid, electionName, schulzeJSONToMap rankings)
  return "Ok"

schulzeWrite :: Query Write (UUID, T.Text, M.Map T.Text Int) ()
schulzeWrite = "insert into schulze (voteId, election, rankings) values (?,?,?)"

countSchulze :: T.Text -> ExceptT ServantErr IO T.Text
countSchulze electionName = do
  votes <- readCas schulzeRead electionName
  return $ runSchulze votes

schulzeRead :: Query Rows T.Text [T.Text]
schulzeRead = "select rankings from schulze where election=?"

runSchulze :: [M.Map T.Text Int] -> T.Text
runSchulze = undefined
-}


data WidestPath = Width Int
                | Unreachable deriving (Eq, Show, Ord)

instance Semiring WidestPath where
  zero = Width 0
  
  Width x <+> Width y = Width $ max x y
  Unreachable <+> x = x
  x <+> Unreachable = x

  one = Unreachable

  Width x <.> Width y = Width $ min x y
  Unreachable <.> x = x
  x <.> Unreachable = x

  closure a = one <+> a
  

-- Use trick from the functional pearl "fun with semirings"
-- to calculate all-pairs-widest-path
-- reimplemented because this doesn't appear to be in a library anywhere.
infixl 9 <.>
infixl 8 <+>
class Semiring a where
  (<+>) :: a -> a -> a
  (<.>) :: a -> a -> a
  closure :: a -> a
  zero :: a
  one :: a

data Matrix a = Scalar a
              | Matrix [[a]] deriving (Eq, Show) 

-- take 4 blocks and join them into a larger matrix
mjoin ( Matrix a, Matrix b
      , Matrix c, Matrix d
      ) = Matrix $ (zipWith (++) a b) ++ (zipWith (++) c d)

msplit (Matrix (row:rows)) = ( Matrix [[a]], Matrix [b]
                             , Matrix c    , Matrix d
                             )
  where (a:b) = row
        (c,d) = unzip $ map (\(x:xs) -> ([x], xs)) rows
  
instance Semiring a => Semiring (Matrix a) where
  zero = Scalar zero
  one = Scalar one
  
  Scalar a <+> Scalar b = Scalar $ a <+> b
  Matrix a <+> Matrix b = Matrix $ (zipWith . zipWith) (<+>) a b
  Scalar a <+> m = m <+> Scalar a
  Matrix [[a]] <+> Scalar b = Matrix [[a <+> b]]
  m <+> s = mjoin ( a <+> s, b
                  , c      , d <+> s
                  )
    where (a,b,
           c,d) = msplit m

  Scalar a <.> Scalar b = Scalar $ a <.> b
  Scalar a <.> Matrix bs = Matrix $ (map . map) (a <.>) bs
  Matrix bs <.> Scalar a = Matrix $ (map . map) (<.> a) bs
  Matrix a <.> Matrix b = 
    Matrix [[ foldl1 (<+>) (zipWith (<.>) row col) | col <- transpose b] | row <- a]

  closure (Matrix [[x]]) = Matrix [[closure x]]
  closure m = mjoin (a' <+> b' <.> d' <.> c', b' <.> d'
                    ,d' <.> c'              , d'              
                    )
    where
      (a, b,
       c, d) = msplit m
      a' = closure a
      b' = a' <.> b
      c' = c <.> a'
      d' = closure (d <+> c' <.> b)
        
closure' indent (Matrix [[x]]) = return $ Matrix [[closure x]] 
closure' indent m = 
  do 
     print $ indent ++ "a:" 
     display indent a
     print $ indent ++ "b:"
     display indent b
     print $ indent ++ "c:" 
     display indent c
     print $ indent ++ "d:"
     display indent d
     print "calling closure' a"
     a' <- closure' ("\t" ++ indent) a
     let b' = a' <.> b
     let c' = c <.> a'
     print "calling closure' d"
     d' <- closure' ("    " ++ indent) $ d <+> (c' <.> b)
     let first = a' <+> (b' <.> d' <.> c')
     let top =  b' <.> d' 
     let front = d' <.> c'
     let rest =  d'

     print $ indent ++ "a':" 
     display indent a'
     print $ indent ++ "b':"
     display indent b'
     print $ indent ++ "c':" 
     display indent c'
     print $ indent ++ "d':"
     display indent d'
     print $ indent ++ "first:"
     display indent first
     print $ indent ++ "top:"
     display indent top
     print $ indent ++ "front:"
     display indent front
     print $ indent ++ "rest:"
     display indent rest
     --print $ "top:" 
     --display top
     let res =  mjoin (first, top, front, rest)
     --print $ "a':" ++ show a'
     --print $ "b':" ++ show b'
     --print $ "c':" ++ show c'
     --print $ "d':" ++ show d'
     --print $ "res:" ++ show res
     --print "\n\n"
     return $ res
  where
    (a, b,
     c, d) = msplit m

example = Matrix 
  [[ Unreachable, Width 20   , Width 26   , Width 30   , Width 22]
  ,[ Width 25   , Unreachable, Width 16   , Width 33   , Width 18]
  ,[ Width 19   , Width 29   , Unreachable, Width 17   , Width 24]
  ,[ Width 15   , Width 12   , Width 28   , Unreachable, Width 14]
  ,[ Width 23   , Width 27   , Width 21   , Width 31   , Unreachable]
  ]

display indent (Matrix m) = mapM_ (print . (indent ++) . show) m

square x = x <.> x
cube x = x <.> square x
fourth x = x <.> cube x
fifth x = x <.> fourth x
