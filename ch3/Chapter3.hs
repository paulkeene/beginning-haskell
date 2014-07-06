{-# LANGUAGE LambdaCase #-}
module Chapter3 where
import Data.List (find)

-- Section 3-1 --

-- 1.
swapTriple :: (a, b, c) -> (b, c, a)
swapTriple (x, y, z) = (y, z, x)

duplicate :: a -> (a, a)
duplicate x = (x, x)

nothing :: a -> Maybe b
nothing _ = Nothing

index :: [a] -> [(Int, a)]
index [] = []
index [x] = [(0, x)]
index (x:xs) = let indexed@((n, _):_) = index xs
               in (n + 1, x):indexed

maybeA :: [a] -> Char
maybeA [] = 'a'


-- Section 3-2 --

-- 1.
filterOnes :: [Int] -> [Int]
filterOnes = filter (== 1)

filterANumber :: Int -> [Int] -> [Int]
filterANumber x = filter (== x)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

data Gender = Male | Female | Unknown
            deriving (Show, Eq, Ord)
data Person = Person String String Gender
            deriving Show
data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs = filter isGovOrg
    where
      isGovOrg :: (Client -> Bool)
      isGovOrg (GovOrg _) = True
      isGovOrg _ = False

filterGovOrgs' :: [Client] -> [Client]
filterGovOrgs' = filter (\case (GovOrg _) -> True
                               _ -> False)


-- List functions --

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFiler f (a:as)
  | f a = a : myFilter f as
  | otherwise = myFilter f as

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f = foldr g []
  where
    g a b
      | f a = a:b
      | otherwise = b

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (a:as) = f a : myMap f as

myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldr g []
  where
    g a b = f a : b

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ a [] = a
myFoldl f a (b:bs) = myFoldl f (f a b) bs


myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' f a bs = foldr (flip f) a (reverse bs)

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (a:as) = f a $ myFoldr f b as

-- Observation: with foldl we are applying the aggregate function to the
-- accumulator and the first element of the list and then passing that value
-- for the recursive call of foldl. With foldr we apply the aggregate function
-- to the return value of the recursive call to foldr.

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a xs = case find (== a) xs of Just a -> True
                                     Nothing -> False


-- Section 3-3 --

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (x:xs) = x * product xs

myProduct' :: [Int] -> Int
myProduct' = foldr (*) 1

clientName :: Client -> String
clientName (GovOrg n) = n
clientName (Company n _ _ _) = n
clientName (Individual (Person n _ _) _) = n

minClient :: Client -> Client -> Client
minClient c1 c2
  | lengthC1 <= lengthC2 = c1
  | otherwise = c2
  where
    lengthC1 = length $ clientName c1
    lengthC2 = length $ clientName c2

minimumClient :: [Client] -> Client
minimumClient (c:[]) = c
minimumClient (c:cs) = minClient c $ minimumClient cs

minimumClient' :: [Client] -> Client
minimumClient' = foldr1 minClient

myAll :: [Bool] -> Bool
myAll [] = True
myAll (x:xs) = x && myAll xs

minimumBy :: Ord b => (a -> b) -> [a] -> a
minimumBy _ (x:[]) = x
minimumBy f (x:xs)
  | f x <= f minRemainder = x
  | otherwise = minRemainder
  where
    minRemainder = minimumBy f xs

minimumBy' :: Ord b => (a -> b) -> [a] -> a
minimumBy' g = foldr1 f
  where
-- For some reason adding the type signature to this scoped function results in 
-- a compiler error. Enabling ScopedTypeVariables didn't help.
--    f :: a -> a -> a
    f a1 a2
      | g a1 <= g a2 = a1
      | otherwise = a2
