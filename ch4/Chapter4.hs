{-# LANGUAGE InstanceSigs #-}
module Chapter4 where

import           Data.Function as F (on)
import qualified Data.Map as M
import           Data.Monoid ((<>), mconcat)
import qualified Data.Set as S


-- Exercise 4-2

myInsert :: Ord k => k -> a -> M.Map k a -> M.Map k a
myInsert k a = M.alter (\_ -> Just a) k

myDelete :: Ord k => k -> M.Map k a -> M.Map k a
myDelete = M.alter $ const Nothing

myAdjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
myAdjust f = M.alter (fmap f)


-- Exercise 4-3

data Gender = Male | Female | Unknown
            deriving (Show, Eq, Ord)
data Person = Person String String Gender
            deriving (Show, Ord)
data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String,
                          person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving Show

data ClientKind = IndividualKind | CompanyKind | GovOrgKind
                deriving (Show, Ord, Eq)

getClientKind :: Client a -> ClientKind
getClientKind (GovOrg {}) = GovOrgKind
getClientKind (Company {}) = CompanyKind
getClientKind (Individual {}) = IndividualKind

classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients = foldr f $ M.fromList [(GovOrgKind, S.empty),
                                        (CompanyKind, S.empty),
                                        (IndividualKind, S.empty)]
  where
    f c = M.adjust (S.insert c) $ getClientKind c

classifyClients' :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients' cs = M.fromList [(GovOrgKind, S.fromList govOrgs),
                                  (CompanyKind, S.fromList companies),
                                  (IndividualKind, S.fromList individuals)]
  where
    f k x = getClientKind x == k
    govOrgs = filter (f GovOrgKind) cs
    companies = filter (f CompanyKind) cs
    individuals = filter (f IndividualKind) cs 


-- Exercise 4-4

class Priceable p where
  price :: p -> Double

data TravelGuide = TravelGuide { title :: String, authors :: [String],
                                 cost :: Double } deriving Show
data MaintenanceTools = MaintenanceTools String Double deriving Show

instance Priceable TravelGuide where
  price TravelGuide { cost = c } = c

instance Priceable MaintenanceTools where
  price (MaintenanceTools _ p) = p

totalPrice :: Priceable p => [p] -> Double
totalPrice = foldr ((+) . price) 0.0


-- Exercise 4-5

instance Eq Person where
  (Person f1 l1 g1) == (Person f2 l2 g2) = (f1 == f2) && (l1 == l2) &&
                                           (g1 == g2)

instance Eq i => Eq (Client i) where
  (GovOrg id1 name1) == (GovOrg id2 name2) = (id1 == id2) && (name1 == name2)
  (Company id1 name1 p1 d1) == (Company id2 name2 p2 d2) = isEqual
    where
      isEqual = (id1 == id2) && (name1 == name2) && (p1 == p2) && (d1 == d2)
  (Individual id1 p1) == (Individual id2 p2) = (id1 == id2) && (p1 == p2)
  _ == _ = False


-- Exercise 4-6

getClientName :: Client i -> String
getClientName GovOrg { clientName = name } = name
getClientName Company { clientName = name } = name
getClientName Individual { person = (Person first _ _) } = first

instance Ord i => Ord (Client i) where
  compare c1 c2 = nameOrd <> kindOrd <> finalOrd c1 c2
    where
      nameOrd = (compare `on` getClientName) c1 c2
      kindOrd = (compare `on` getClientKind) c1 c2
      finalOrd c1'@(Individual {}) c2'@(Individual {}) = personOrd <> idOrd
        where
          personOrd = (compare `on` person) c1' c2'
          idOrd = (compare `on` clientId) c1' c2'
      finalOrd c1'@(Company {}) c2'@(Company {}) = personOrd <> dutyOrd <> idOrd
        where
          personOrd = (compare `on` person) c1' c2'
          idOrd = (compare `on` clientId) c1' c2'
          dutyOrd = (compare `on` duty) c1' c2'
      finalOrd c1'@(GovOrg {}) c2'@(GovOrg {}) = (compare `on` clientId) c1' c2'


-- Exercise 4-7

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  | Leaf
                  deriving Show

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert v Leaf = Node v Leaf Leaf
treeInsert v n@(Node v' l r) = case compare v v' of
                                 EQ -> n
                                 LT -> Node v' (treeInsert v l) r
                                 GT -> Node v' l (treeInsert v r)

treeConcat :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeConcat Leaf t2 = t2
treeConcat t1 Leaf = t1
treeConcat t1 n@(Node v l r) = treeConcat (treeConcat (treeInsert v t1) l) r


-- Exercise 4-8

newtype MyMaybe a = MyMaybe (Maybe a) deriving Show

instance Functor MyMaybe where
  fmap _ (MyMaybe Nothing) = MyMaybe Nothing
  fmap f (MyMaybe (Just x)) = MyMaybe . Just $ f x

-- Unfortunately we can't make BinaryTree an instance of Functor in its current
-- state. Although BinaryTree does not have a constraint on its type (since in
-- the Haskell community the convention is to never do this) it really should
-- only hold Ord-erable data. We have no way to enforce that the transformation
-- function passed to fmap will output a type that is orderable, which means
-- that we can't use any of the functions we defined for BinaryTree since
-- they all specify the Ord class constraint. Basically the author needs to
-- fix this example / exercise.

--instance Functor BinaryTree where
--  fmap :: Ord b => (a -> b) -> BinaryTree a -> BinaryTree b
--  fmap _ Leaf = Leaf
--  fmap f n@(Node v l r) = treeConcat (fmap f r) $ treeInsert (f v) (fmap f l)
