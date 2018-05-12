module Dealer(
  Dealer
, newDeal
, addDefn
, addReq
, addNewReq
, eval
, Deal
) where

import Data.List(transpose)
import Data.List.Utils(join, split, wholeMap, fixedWidth)
import Data.String.Utils(strip)
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
        "  \\deal{" ++ toLatex d ++ "}{" ++ toLatex v ++ "}%\n" ++
        (join "" . map format $ [n, e, s, w])
      where
          format h = "    {" ++ toLatex h ++ "}\n"


eval :: T.Direction -> T.Vulnerability -> Dealer -> Int -> IO (Maybe Deal)
eval dir vul deal seed = let
    result :: IO String
    result = readProcess "dealer" ["-s", show seed] (toProgram deal)

    toSuits :: [String] -> Maybe [[String]]
    toSuits (_:s:h:d:c:_:_:_) = Just $ map splitSuit [s, h, d, c]
    toSuits problem       = error $ join "\n" problem

    splitSuit :: String -> [String]
    splitSuit = map strip . wholeMap (fixedWidth (repeat 20))

    toHand :: [String] -> Maybe S.Hand
    toHand (s:h:d:c:[]) = Just $ S.Hand s h d c
    toHand  _           = Nothing

    toDeal :: [[String]] -> Maybe Deal
    toDeal suits = let
        maybeHands = sequence . map toHand . transpose $ suits
      in
        case maybeHands of
            Just (n:e:s:w:[]) -> Just $ Deal dir vul n e s w
            Just _            -> Nothing
            Nothing           -> Nothing
  in
    do
        output <- result
        let lines = split "\n" output
        return $ toSuits lines >>= toDeal
