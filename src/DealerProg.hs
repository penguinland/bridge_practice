module DealerProg(
  DealerProg
, addDefn
, addReq   -- TODO: does this need to be public?
, addNewReq
, invert
, eval
, toProgram -- TODO: remove when done debugging
) where

import Data.List(transpose)
import Data.List.Utils(join, split, wholeMap, fixedWidth)
import qualified Data.Map.Strict as Map
import Data.String.Utils(strip)
import GHC.IO.Exception(ExitCode(ExitSuccess))
import System.Process(readProcessWithExitCode)

import qualified Structures as S
import qualified Terminology as T


-- TODO: Make these (String, Direction) and [String]. This is difficult at the
-- moment because `invert` prematurely casts some of these to string, meaning
-- the separate direction is lost within inversions before it is lost elsewhere,
-- and thus rotating the auction doesn't work properly. Better might be to make
-- a free monad with an interpreter at the end which casts it to a dealer
-- program?
type CondName = String
type CondDefn = String


data DealerProg = DealerProg (Map.Map CondName CondDefn) [CondName]


instance Semigroup DealerProg where
    (DealerProg defnsA reqsA) <> (DealerProg defnsB reqsB) =
        DealerProg (Map.unionWithKey noDupes defnsA defnsB) (reqsB ++ reqsA)
      where
        noDupes k a b | a == b    = a
                      | otherwise = error $ "2 definitons for " ++ k

instance Monoid DealerProg where
    mempty = DealerProg Map.empty []


addDefn :: CondName -> CondDefn -> DealerProg -> DealerProg
addDefn name defn (DealerProg m l) =
  case Map.lookup name m of
    Nothing    -> DealerProg (Map.insert name defn m) l
    Just defn' -> if defn == defn' then DealerProg m l
                                   else error $ "2 defintions for " ++ name


addReq :: CondName -> DealerProg -> DealerProg
addReq expr (DealerProg m l) = DealerProg m (expr:l)


addNewReq :: CondName -> CondDefn -> DealerProg -> DealerProg
addNewReq name defn = addReq name . addDefn name defn


invert :: DealerProg -> DealerProg
invert (DealerProg defns reqs) =
    DealerProg defns ["!(" ++ join " && " reqs ++ ")"]


toProgram :: DealerProg -> String
toProgram (DealerProg defns conds) = join "\n" $
    -- This used to generate 1 million boards, but some situations are extremely
    -- rare (e.g., North opens a Precision 1D with 5 hearts and South responds
    -- 2N: occurs less than twice in a million deals), and if you run the dealer
    -- program many times, it'll fail at least once. So, this was bumped up to
    -- 10 million so that it should really only fail if the constraints are
    -- impossible, rather than just a really rare situation. If 10 million still
    -- isn't enough, consider removing those situations from practice entirely,
    -- since they'll probably never come up.
    ["generate 10000000", "produce 1", ""] ++
    Map.foldMapWithKey formatDefinition defns
    ++ ["", "condition",
        "    " ++ (join " && " . map conditionToString . reverse $ conds),
        "action", "    printall"]
  where
    conditionToString :: CondName -> String
    conditionToString = id
    formatDefinition name defn =
        ["    " ++ conditionToString name ++ " = " ++ defn]


eval :: T.Direction -> T.Vulnerability -> DealerProg -> Int -> IO (Maybe S.Deal)
eval dir vul deal seed = let
    prog = toProgram deal

    result :: IO (Maybe String)
    result = do
        (exitCode, stdout, stderr) <- readProcessWithExitCode
            "dealer" ["-s", show seed] prog
        if exitCode == ExitSuccess
        then return $ Just stdout
        else do putStrLn "Error!"
                putStrLn stderr
                putStrLn prog
                return Nothing

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
    toSuits output@(_:s:h:d:c:_)
     | length output == 11 = Just $ map splitSuit [s, h, d, c]
    -- If that didn't work, we have totally misunderstood something.
    toSuits problem     = error $ join "\n"
        ("Unexpected output from dealer invocation:":prog:"Output was:":problem)

    splitSuit :: String -> [String]
    splitSuit = map strip . wholeMap (fixedWidth (repeat 20))

    toHand :: [String] -> Maybe S.Hand
    toHand [s, h, d, c] = Just $ S.Hand s h d c
    toHand  _           = Nothing

    toDeal :: [[String]] -> Maybe S.Deal
    toDeal suits = let
        maybeHands = mapM toHand . transpose $ suits
      in
        case maybeHands of
            Just [n, e, s, w] -> Just $ S.Deal dir vul n e s w
            Just _            -> error $ "Unexpected suits!" ++ show seed
            Nothing           -> Nothing
  in
    do
        -- Uncomment the next line during debugging.
        --putStrLn prog
        output <- result  -- output is a Maybe String
        -- Note: the do notation takes care of the IO monad, and the binds on
        -- this next line take care of the Maybe monad.
        return $ output >>= (toSuits . split "\n") >>= toDeal
