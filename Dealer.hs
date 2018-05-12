module Dealer(
  Dealer
, newDeal
, addDefn
, addReq
, addNewReq
, eval
, Deal
) where

import Data.List.Utils(join, split, wholeMap, fixedWidth)
import qualified Data.Map.Strict as Map
import System.Process(readProcess)

import Output(Showable, toLatex)
import qualified Structures as S
import qualified Terminology as T


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

toProgram :: Dealer -> String
toProgram (Dealer defns conds) = join "\n" $
    ["generate 1000000", "produce 1", ""] ++
    (Map.foldMapWithKey (\k v -> ["  " ++ k ++ " = " ++ v]) defns)
    ++ ["", "condition",
        join " && " conds,
        "action", "    printall"]

--                                           North  East   South  West
data Deal = Deal T.Direction T.Vulnerability S.Hand S.Hand S.Hand S.Hand

instance Showable Deal where
    toLatex (Deal d v n e s w) =
        "  \\deal{" ++ toLatex d ++ "}{" ++ toLatex v ++ "{%\n" ++
        (join "" . map format $ [n, e, s, w])
      where
          format h = "    {" ++ toLatex h ++ "}\n"


eval :: T.Direction -> T.Vulnerability -> Dealer -> IO (Maybe String)
eval dir vul deal = let
    result = readProcess "dealer" ["-s", "0"] (toProgram deal)
  in
    result >>= return . Just
