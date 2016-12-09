module ReferenceGame where

import Control.Monad.Random (MonadRandom)
import Data.Distribution
import Data.List (nub)
import Data.Map ((!))
import Text.PrettyPrint.Boxes
import Text.Printf (printf)

newtype Feature = Feature String deriving (Eq, Ord)
data Object = Object String [Feature] deriving (Eq, Ord)
newtype Agent = Agent [((Object, Feature), Int)]

instance Show Object where
  show (Object name _) = name

instance Show Feature where
  show (Feature name) = name

pprint :: Probability -> String
pprint p = printf "%.2f" (realToFrac p :: Double)

instance Show Agent where
  show (Agent ev) = render table
    where dist = toMap $ fromList ev
          nameColumn = vcat right (text "":map (text . show) features)
          table = hsep 4 top $ nameColumn:(map colFor objects)
          colFor :: Object -> Box
          colFor o = vcat right $ text (show o):(map (\f -> text $ pprint $ dist ! (o, f)) features)

objectsFrom :: [(String, [String])] -> [Object]
objectsFrom = map (\(name, fs) -> Object name (map Feature fs))

objects :: [Object]
objects = objectsFrom [
  ("a", ["hat", "glasses"]),
  ("b", ["hat"]),
  ("c", ["glasses", "scarf"])
  ]

features :: [Feature]
features = nub $ concatMap (\(Object _ fs) -> fs) objects

startingEvidence :: [((Object, Feature), Int)]
startingEvidence = do
  Object name fs <- objects
  feature <- features
  let count = (if elem feature fs then quot 60 (length fs) else 0) + 1
  return ((Object name fs, feature), count)

startingAgent :: Agent
startingAgent = Agent startingEvidence

equal1 :: Eq a => a -> (a, b) -> Bool
equal1 a' (a, _) = a == a'

equal2 :: Eq b => b -> (a, b) -> Bool
equal2 b' (_, b) = b == b'

speaker :: Agent -> Distribution (Object, Feature)
speaker (Agent ev) = (uniform objects) `andThen` (\o -> assuming (equal1 o) (fromList ev))

listener :: Agent -> Distribution (Object, Feature)
listener (Agent ev) = (uniform features) `andThen` (\f -> assuming (equal2 f) (fromList ev))

object :: MonadRandom m => m Object
object = getSample (fromDistribution $ uniform objects)

speak :: MonadRandom m => Object -> Agent -> m Feature
speak o ag = fmap snd $ getSample (fromDistribution dist)
  where dist = assuming (equal1 o) (speaker ag)

listen :: MonadRandom m => Feature -> Agent -> m Object
listen f ag = fmap fst $ getSample (fromDistribution dist)
  where dist = assuming (equal2 f) (listener ag)

learn :: (Object, Feature) -> Agent -> Agent
learn msg (Agent ev) = Agent (map learn' ev)
  where learn' (msg', count) = (msg', if msg == msg' then count + 1 else count)
