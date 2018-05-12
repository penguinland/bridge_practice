module Dealer(
  Dealer
, newDeal
, addDefn
, addReq
, addNewReq
, eval
) where

import qualified Data.Map.Strict as Map
import System.Process(readProcess)

data Dealer = Dealer (Map.Map String String) [String]

newDeal :: Dealer
newDeal = Dealer Map.empty []

addDefn :: String -> String -> Dealer -> Dealer
addDefn name defn (Dealer m l) =
  case Map.lookup name m of
    Nothing    -> Dealer (Map.insert name defn m) l
    Just defn' -> if defn == defn' then Dealer m l
                                   else error $ "2 defintions for " ++ name

addReq :: String -> Dealer -> Dealer
addReq expr (Dealer m l) = Dealer m (expr:l)

addNewReq :: String -> String -> Dealer -> Dealer
addNewReq name defn = addReq name . addDefn name defn

eval :: Dealer -> IO String
eval = undefined
