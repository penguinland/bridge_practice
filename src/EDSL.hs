module EDSL (
  forbid
, forbidAll
, alternatives
, impliesThat
, forEach
, makeCall
, makeAlertableCall
, makePass
, pointRange
, balancedHand
, flatHand
, suitLength
, minSuitLength
, maxSuitLength
, hasTopN
, hasControl
, soundHolding
, longerThan
, shorterThan
, equalLength
, atLeastAsLong
, atMostAsLong
, loserCount
, minLoserCount
, maxLoserCount
) where

import Control.Monad.Trans.State.Strict(execState, get, put, modify)
import Data.Bifunctor(first)
import Data.List.Utils(join)

import Action(Action, newAuction, constrain)
import DealerProg(invert)
import Output(Showable, toCommentary)
import Structures(addCall, currentBidder)
import qualified Terminology as T


forbid :: Action -> Action
forbid action = do
    (bidding, dealerProg) <- get
    let freshAuction = newAuction . currentBidder $ bidding
        (_, dealerToInvert) = execState action freshAuction
    put (bidding, dealerProg `mappend` invert dealerToInvert)


forbidAll :: [Action] -> Action
forbidAll = mapM_ forbid


alternatives :: [Action] -> Action
-- We use deMorgan's laws. (A || B || C) becomes !(!A && !B && !C)
alternatives = forbid . forbidAll


impliesThat :: Action -> Action -> Action
impliesThat ifPart thenPart = alternatives [thenPart, forbid ifPart]


forEach :: [a] -> (a -> Action) -> Action
forEach = flip mapM_


makeCall :: T.Call -> Action
makeCall call = modify $ first (addCall $ T.CompleteCall call Nothing)

makeAlertableCall :: Showable a => T.Call -> a -> Action
makeAlertableCall call alert =
    modify $ first (addCall $ T.CompleteCall call (Just . toCommentary $ alert))


makePass :: Action
makePass = makeCall T.Pass


balancedHand :: Action
balancedHand =
    constrain "balanced" ["shape(", ", any 4333 + any 5332 + any 4432)"]


flatHand :: Action
flatHand = constrain "flat" ["shape(", ", any 4333)"]


pointRange :: Int -> Int -> Action
pointRange minHcp maxHcp =
    constrain (join "_" ["range", show minHcp, show maxHcp])
              ["hcp(", ") >= " ++ show minHcp ++ " && " ++
               "hcp(", ") <= " ++ show maxHcp]


suitLengthOp :: String -> String -> T.Suit -> Int -> Action
suitLengthOp op suffix suit len =
    constrain (join "_" [show suit, suffix, show len])
              [show suit ++ "(", ") " ++ op ++ " " ++ show len]

suitLength :: T.Suit -> Int -> Action
suitLength = suitLengthOp "==" "eq"

minSuitLength :: T.Suit -> Int -> Action
minSuitLength = suitLengthOp ">=" "ge"

maxSuitLength :: T.Suit -> Int -> Action
maxSuitLength = suitLengthOp "<=" "le"


hasTopN :: T.Suit -> Int -> Int -> Action
hasTopN suit range minCount = do
    constrain (join "_" [show suit, show minCount, "of", "top", show range])
              ["top" ++ show range ++ "(", ", " ++ show suit ++
               ") >= " ++ show minCount]


-- Shows the ace/king/singleton/void
hasControl :: T.Suit -> Action
hasControl suit = alternatives [hasTopN suit 2 1, maxSuitLength suit 1]


-- A sound pre-empt has 2 of the top 3 or 3 of the top 5 cards in the suit.
soundHolding :: T.Suit -> Action
soundHolding suit = alternatives [hasTopN suit 3 2, hasTopN suit 5 3]


-- unexported helper
_compareSuitLength :: String -> String -> T.Suit -> T.Suit -> Action
_compareSuitLength name op suitA suitB = let
    fullName = join "_" [show suitA, name, show suitB, "length"]
  in
    constrain fullName [show suitA ++ "(", ") " ++ op ++ " " ++
                        show suitB ++ "(", ")"]

longerThan :: T.Suit -> T.Suit -> Action
longerThan = _compareSuitLength "longer" ">"

shorterThan :: T.Suit -> T.Suit -> Action
shorterThan = _compareSuitLength "shorter" "<"

equalLength :: T.Suit -> T.Suit -> Action
equalLength = _compareSuitLength "equal" "=="

atLeastAsLong :: T.Suit -> T.Suit -> Action
atLeastAsLong = _compareSuitLength "ge" ">="

atMostAsLong :: T.Suit -> T.Suit -> Action
atMostAsLong = _compareSuitLength "le" "<="


-- unexported helper
loserComparison :: String -> String -> Int -> Action
loserComparison name op count = let
    fullName = join "_" ["losers", name, show count]
  in
    constrain fullName ["loser(", ") " ++ op ++ " " ++ show count]

loserCount :: Int -> Action
loserCount = loserComparison "equal" "=="

minLoserCount :: Int -> Action
minLoserCount = loserComparison "at_least" ">="

maxLoserCount :: Int -> Action
maxLoserCount = loserComparison "at_most" "<="


-- TODO: hasCard
