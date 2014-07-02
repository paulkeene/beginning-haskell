module Chapter2 where

import Data.List (foldl')
import qualified Data.Map as Map


-- Section 2-1 --

-- 1.
list1_1 :: String
list1_1 = 'a':'b':'c':[]

list1_2 :: [String]
list1_2 = ('a':'b':'c':[]):('d':'e':[]):[]

-- 2.
whatKindOfEmpty :: [[a]] -> String
whatKindOfEmpty [] = "Empty"
whatKindOfEmpty ([]:xs) = "First Element Empty"
whatKindOfEmpty _ = "Not Empty"

-- 3.
onlyOneElement :: [a] -> Bool
onlyOneElement (_:[]) = True
onlyOneElement _ = False

-- 4.
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:[]) = xs
myConcat (x:r@(y:xs)) = x ++ myConcat r


-- Exercises for Section 2-2 and 2-3 were just fluff --


-- Section 2-4 --

-- 1.
data Gender = Male | Female | Unknown
            deriving (Show, Eq, Ord)
data Person = Person String String Gender
            deriving Show
data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

-- 2.
testPerson :: Client
testPerson = Individual (Person "Paul" "Keene" Male) False

-- 3.
data TimeTravelCapability = Past | Future | Both
                          deriving Show
data TimeMachine = TimeMachine String Integer String TimeTravelCapability Float
                 deriving Show


-- Section 2-5 --
-- 1.
genderCounts :: [Client] -> Map.Map Gender Int
genderCounts cs = genderCounts' cs defaultMap
  where
    genderCounts' :: [Client] -> Map.Map Gender Int -> Map.Map Gender Int
    genderCounts' [] counts = counts
    genderCounts' (c:cs) counts = genderCounts' cs $ Map.adjust (+ 1)
                                  (clientGender c) counts
    defaultMap :: Map.Map Gender Int
    defaultMap = Map.insert Unknown 0 $ Map.insert Female 0 $
                 Map.insert Male 0 Map.empty

clientGender :: Client -> Gender
clientGender (GovOrg _) = Unknown
clientGender (Company _ _ (Person _ _ g) _) = g
clientGender (Individual (Person _ _ g) _) = g

-- 2.
applyDiscount :: Float -> [TimeMachine] -> [TimeMachine]
applyDiscount discount = map discountTimeMachine
  where
    discountTimeMachine :: TimeMachine -> TimeMachine
    discountTimeMachine (TimeMachine m o n t p) = TimeMachine m o n t p'
      where
        p' = p * (1 - discount)


-- Section 2-6 --
-- 1.
ackermann :: Int -> Int -> Int
ackermann m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = ackermann (m - 1) 1
  | m > 0 && n > 0 = ackermann (m - 1) $ ackermann m (n - 1)

-- 2.
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = foldl' f ([], [])
  where
    f :: ([a], [b]) -> (a, b) -> ([a], [b])
    f (as, bs) (a', b') = (as ++ [a'], bs ++ [b'])


-- Section 2-7 --
-- 1.
data TimeMachineR = TimeMachineR { manufacturer :: String
                                 , model :: Int
                                 , name :: String
                                 , timeTravelCapability :: TimeTravelCapability
                                 , price :: Float }
                  deriving Show

applyDiscountR :: Float -> [TimeMachineR] -> [TimeMachineR]
applyDiscountR discount = map discountTimeMachine
  where
    discountTimeMachine :: TimeMachineR -> TimeMachineR
    discountTimeMachine t = t { price = price' }
      where
        price' = price t * (1 - discount)
