module Chapter4 where

import qualified Data.Map as M


-- Exercise 4-2

myInsert :: Ord k => k -> a -> M.Map k a -> M.Map k a
myInsert k a = M.alter (\_ -> Just a) k

myDelete :: Ord k => k -> M.Map k a -> M.Map k a
myDelete = M.alter const Nothing

myAdjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
myAdjust f = M.alter (fmap f)
