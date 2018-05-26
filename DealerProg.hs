module DealerProg(
  DealerProg
, newDeal
, addDefn
, addReq
, addNewReq
, forbidNewReq
, invert
, eval
, toProgram -- TODO: remove when done debugging
, Deal
) where

import Data.List(transpose)
import Data.List.Utils(join, split, wholeMap, fixedWidth)
import Data.String.Utils(strip)
import qualified Data.Map.Strict as Map
import GHC.IO.Exception(ExitCode(ExitSuccess))
import System.Process(readProcessWithExitCode)

import Output(Showable, toLatex)
import qualified Structures as S
import qualified Terminology as T


data DealerProg = DealerProg (Map.Map String String) [String]

newDeal :: DealerProg
newDeal = DealerProg Map.empty []

instance Monoid DealerProg where
    mempty = newDeal
    mappend (DealerProg defnsA reqsA) (DealerProg defnsB reqsB) =
        DealerProg (Map.unionWithKey noDupes defnsA defnsB) (reqsB ++ reqsA)
      where
        noDupes k a b = if a == b then a else error $ "2 definitons for " ++ k

addDefn :: String -> String -> DealerProg -> DealerProg
addDefn name defn (DealerProg m l) =
  case Map.lookup name m of
    Nothing    -> DealerProg (Map.insert name defn m) l
    Just defn' -> if defn == defn' then DealerProg m l
                                   else error $ "2 defintions for " ++ name

addReq :: String -> DealerProg -> DealerProg
addReq expr (DealerProg m l) = DealerProg m (expr:l)

addNewReq :: String -> String -> DealerProg -> DealerProg
addNewReq name defn = addReq name . addDefn name defn

forbidNewReq :: String -> String -> DealerProg -> DealerProg
forbidNewReq name defn = addReq ("!" ++ name) . addDefn name defn

invert :: DealerProg -> DealerProg
invert (DealerProg defns reqs) =
    DealerProg defns ["!(" ++ join " && " reqs ++ ")"]

toProgram :: DealerProg -> String
toProgram (DealerProg defns conds) = join "\n" $
    ["generate 1000000", "produce 1", ""] ++
    (Map.foldMapWithKey (\k v -> ["    " ++ k ++ " = " ++ v]) defns)
    ++ ["", "condition",
        "    " ++ (join " && " . reverse $ conds),
        "action", "    printall"]

--                                           North  East   South  West
data Deal = Deal T.Direction T.Vulnerability S.Hand S.Hand S.Hand S.Hand

instance Showable Deal where
    toLatex (Deal d v n e s w) =
        "  \\deal{" ++ toLatex d ++ "}{" ++ toLatex v ++ "}%\n" ++
        (noPercent . join "" . map format $ [n, e, s, w])
      where
          format h = "    {" ++ toLatex h ++ "}%\n"
          -- The very last hand at the end of the deal shouldn't end in a %
          noPercent (a:b:[]) = [b]
          noPercent (a:z) = a:(noPercent z)


eval :: T.Direction -> T.Vulnerability -> DealerProg -> Int -> IO (Maybe Deal)
eval dir vul deal seed = let
    result :: IO (Maybe String)
    result = do
        let prog = toProgram deal
        (exitCode, stdout, stderr) <- readProcessWithExitCode
            "dealer" ["-s", show seed] prog
        if exitCode == ExitSuccess
        then return $ Just stdout
        else putStrLn "Error!" >> putStrLn stderr >> putStrLn prog >> return Nothing

    toSuits :: [String] -> Maybe [[String]]
    -- The dealer output, when everything went right, contains the following:
    -- 1.  Deal number (always 1 for us)
    -- 2.  spades
    -- 3.  hearts
    -- 4.  diamonds
    -- 5.  clubs
    -- 6.  blank line
    -- 7.  number of hands generated
    -- 8.  number of hands produced
    -- 9.  initial random seed
    -- 10. time spent computing
    -- 11. blank line
    toSuits (_:s:h:d:c:_:_:_:_:_:_:[]) = Just $ map splitSuit [s, h, d, c]
    -- If that didn't work, we have totally misunderstood something.
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
            Just _            -> error $ "Unexpected suits!" ++ show seed
            Nothing           -> Nothing
  in
    do
        output <- result  -- output is a Maybe String
        return $ output >>= (toSuits . split "\n") >>= toDeal
