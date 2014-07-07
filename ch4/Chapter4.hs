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
            deriving (Show, Ord, Eq)
data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String,
                          person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Ord, Eq)

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
