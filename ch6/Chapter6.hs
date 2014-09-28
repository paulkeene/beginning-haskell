{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Chapter6 where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.List
import qualified Data.Map as M


class Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (c - a) * (c - a) + (d - b) * (d - b)
  centroid lst = let (u, v) = foldr (\(a, b) (c, d) -> (a + c, b + d))
                                (0.0, 0.0) lst
                     n = fromIntegral $ length lst
                 in (u / n, v / n)

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

clusterAssignmentPhase :: (Vector v, Vectorizable e v, Ord v) => [v]
                                                              -> [e]
                                                              -> M.Map v [e]
clusterAssignmentPhase centroids = foldr assignCluster initialMap
  where
    assignCluster e = M.adjust (++ [e]) $ closestCentroid e
    closestCentroid e' = minimumBy f centroids
      where
        f c1 c2 = compare (distance v c1) (distance v c2)
        v = toVector e'
    initialMap = M.fromList $ zip centroids (repeat [])

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids t = foldr (\(c, c') a -> distance c c' + a) 0 centroids < t

kmeans :: (Vector v, Vectorizable e v, Ord v)
            => (Int -> [e] -> [v]) -- initialization f
            -> Int                 -- # of centroids
            -> [e]                 -- data
            -> Double              -- threshold
            -> [v]                 -- final centroids
kmeans f c es = kmeans' initialCentroids es
  where
    initialCentroids = f c es

kmeans' :: (Vector v, Vectorizable e v, Ord v) => [v] -> [e] -> Double -> [v]
kmeans' centroids es t
  | shouldStop oldNewCentroids t = newCentroids
  | otherwise = kmeans' newCentroids es t
  where
    clusters = clusterAssignmentPhase centroids es
    oldNewCentroids = newCentroidPhase clusters
    newCentroids = map snd oldNewCentroids

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n)
                         : initializeSimple (n - 1) v


-- Exercise 6-1
kmeansWithCount :: (Vector v, Vectorizable e v, Ord v)
                     => (Int -> [e] -> [v]) -- initialization f
                     -> Int                 -- # of centroids
                     -> [e]                 -- data
                     -> Double              -- threshold
                     -> ([v], Int)          -- final centroids, # iterations
kmeansWithCount f c es t = kmeansWithCount' initialCentroids es t 0
  where
    initialCentroids = f c es

kmeansWithCount' :: (Vector v, Vectorizable e v, Ord v)
                      => [v]
                      -> [e]
                      -> Double
                      -> Int
                      -> ([v], Int)
kmeansWithCount' vs es t c
  | shouldStop oldNewCentroids t = (newCentroids, c + 1)
  | otherwise = kmeansWithCount' newCentroids es t (c + 1)
  where
    clusters = clusterAssignmentPhase vs es
    oldNewCentroids = newCentroidPhase clusters
    newCentroids = map snd oldNewCentroids

testData :: [(Double, Double)]
testData = [(1.0,1.0),(1.0,2.0),(4.0,4.0),(4.0,5.0)]

testResult :: ([(Double, Double)], Int)
testResult = kmeansWithCount initializeSimple 2 testData 0.001


-- Exercise 6-2
data TimeTravelCapability = Past | Future | Both
                          deriving Show
data TimeMachine = TimeMachine { _manufacturer :: String
                               , _model :: Integer
                               , _name :: String
                               , _timeTravelCapability :: TimeTravelCapability
                               , _price :: Float }
                 deriving Show

makeLenses ''TimeMachine

updatePrice :: [TimeMachine] -> Float -> [TimeMachine]
updatePrice ts percent = map (\t -> t & price *~ (1.0 + percent / 100.0)) ts


-- Exercise 6-3

data KMeansState e v = KMeansState { _centroids :: [v]
                                   , _points :: [e]
                                   , _err :: Double
                                   , _threshold :: Double
                                   , _steps :: Int }
                       deriving Show

makeLenses ''KMeansState

initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState (i n pts) pts (1.0 / 0.0) t 0

kMeans :: (Vector v, Vectorizable e v, Ord v) => (Int -> [e] -> [v]) -> Int -> [e]
                                          -> Double -> [v]
kMeans i n pts t = view centroids $ kMeans' (initializeState i n pts t)

kMeans' :: (Vector v, Vectorizable e v, Ord v) => KMeansState e v -> KMeansState e v
kMeans' state =
  let assignments = clusterAssignments state
      state1 = state & centroids.traversed
                     %~ (\c -> centroid $ fmap toVector $ M.findWithDefault []
                               c assignments)
      state2 = state1 & err .~ sum (zipWith distance (state ^. centroids)
                                    (state1 ^. centroids))
      state3 = state2 & steps +~ 1
  in if state3 ^. err < state3 ^. threshold then state3 else kMeans' state3

clusterAssignments :: (Vector v, Vectorizable e v, Ord v) => KMeansState e v ->
                                                             M.Map v [e]
clusterAssignments state = foldr assignCluster initialMap (state ^. points)
  where
    assignCluster e = M.adjust (++ [e]) $ closestCentroid e
    closestCentroid e' = minimumBy f (state ^. centroids)
      where
        f c1 c2 = compare (distance v c1) (distance v c2)
        v = toVector e'
    initialMap = M.fromList $ zip (state ^. centroids) (repeat [])
