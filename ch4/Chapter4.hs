module Chapter4 where

import qualified Data.Map as M
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
              deriving (Show, Ord)

data ClientKind = GovOrgKind | CompanyKind | IndividualKind
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

data TravelGuide = TravelGuide String Double deriving Show
data MaintenanceTools = MaintenanceTools String Double deriving Show

instance Priceable TravelGuide where
  price (TravelGuide _ p) = p

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
