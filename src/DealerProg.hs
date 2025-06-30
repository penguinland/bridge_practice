module DealerProg(
  DealerProg
, addDefn
, addNewReq
, invert
, nameAll
, eval
, toProgram  -- Useful when debugging
) where

import Data.List(transpose)
import Data.List.Utils(join, split, wholeMap, fixedWidth)
import Data.String.Utils(strip)
import GHC.IO.Exception(ExitCode(ExitSuccess))
import System.Process(readProcessWithExitCode)

import DealerDefs(CondName, CondDefn, DealerDefs, addDefinition, toProgDefs)
import qualified Structures as S
import qualified Terminology as T


-- Contains all definitions and then required conditions
data DealerProg = DealerProg DealerDefs [CondName]


instance Semigroup DealerProg where
    (DealerProg defnsA reqsA) <> (DealerProg defnsB reqsB) =
        DealerProg (defnsA <> defnsB) (reqsB ++ reqsA)


instance Monoid DealerProg where
    mempty = DealerProg mempty []


addDefn :: CondName -> CondDefn -> DealerProg -> DealerProg
addDefn name defn (DealerProg m l) =
  DealerProg (addDefinition name defn m) l


addNewReq :: CondName -> CondDefn -> DealerProg -> DealerProg
addNewReq name defn (DealerProg m l) =
  DealerProg (addDefinition name defn m) (name:l)


invert :: DealerProg -> DealerProg
invert (DealerProg defns reqs) =
    DealerProg defns ["!(" ++ join " && " reqs ++ ")"]


nameAll :: CondName -> DealerProg -> DealerProg
nameAll name (DealerProg defns reqs) =
    if (length reqs > 0)
    then addNewReq name (join " && " . reverse $ reqs) (DealerProg defns mempty)
    else DealerProg defns reqs  -- Unchanged


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
    toProgDefs defns
    ++ ["", "condition",
        "    " ++ (join " && " . reverse $ conds),
        "action", "    printall"]


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
