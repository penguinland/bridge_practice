module Bids.StandardModernPrecision.BasicBids(
    lessThanInvitational
  , invitational
  , oppsPass
  -- Opening bids
  , b1C
  , b1D
  , b1M
  , b1N
  , b2C
  , b2D
  , b2N
  , b3N
  , setOpener
) where

import Control.Monad.Trans.State.Strict(get)

import Action(Action, constrain)
import CommonBids(cannotPreempt, cannotPreempt2H)
import EDSL(forbid, pointRange, suitLength, minSuitLength, hasTopN,
            balancedHand, makeCall, makeAlertableCall, makePass, alternatives,
            minLoserCount, maxLoserCount, forEach, forbidAll, longerThan,
            atLeastAsLong, nameAction)
import Output(Punct(..), (.+))
import Structures(currentBidder)
import qualified Terminology as T


lessThanInvitational :: Action
lessThanInvitational = nameAction "smp_less_than_invitational" $ do
    pointRange 0 10
    minLoserCount 9


invitational :: Action
invitational = nameAction "smp_invitational" $ do
    pointRange 11 13
    minLoserCount 7  -- TODO: Is this right? Maybe it should be exactly 8 losers
    maxLoserCount 8


oppsPass :: Action
oppsPass = nameAction "smp_pass" $ do
    cannotPreempt
    makePass


smpBalancedHand_ :: Action
smpBalancedHand_ = nameAction "smp_balanced" $ do
    alternatives [balancedHand, smpSemibalanced]
  where
    smpSemibalanced = do  -- 4-2 in the majors, 5-2 in the minors
        alternatives [ suitLength T.Hearts 2 >> suitLength T.Spades 4
                     , suitLength T.Hearts 4 >> suitLength T.Spades 2
                     ]
        alternatives [ suitLength T.Clubs 2 >> suitLength T.Diamonds 5
                     , suitLength T.Clubs 5 >> suitLength T.Diamonds 2
                     ]


------------------
-- Opening bids --
------------------
b1N :: Action
b1N = nameAction "smp_b1N" $ do
    pointRange 14 16
    smpBalancedHand_
    makeAlertableCall (T.Bid 1 T.Notrump) ("14" .+ NDash .+ "16 HCP")


b2N :: Action
b2N = nameAction "smp_b2N" $ do
    pointRange 19 20  -- A modification from Part 1
    smpBalancedHand_
    makeAlertableCall (T.Bid 2 T.Notrump) ("19" .+ NDash .+ "20 HCP")


b3N :: Action
b3N = nameAction "smp_b3N" $ do
    alternatives . map (\suit -> minSuitLength suit 7 >> hasTopN suit 5 4) $
        T.minorSuits
    makeAlertableCall (T.Bid 3 T.Notrump) "Gambling: long running minor"


b1C :: Action
b1C = nameAction "smp_b1C" $ do
    pointRange 16 40
    forbidAll [b1N, b2N]
    makeAlertableCall (T.Bid 1 T.Clubs) "16+ HCP, any shape"


b1M :: T.Suit -> Action
b1M suit = nameAction ("smp_b1" ++ T.suitLetter suit) $ do
    _canOpen
    forbidAll [b1C, b1N, b2N]
    minSuitLength suit 5
    -- If you're a maximum with a 6-card minor and 5-card major, open the minor.
    forEach T.minorSuits (\minor -> forbid (
        pointRange 14 15 >> minSuitLength minor 6 >> suitLength suit 5))
    -- TODO: would you reverse with 5-5 in the majors and a maximum?
    -- TODO: these should use `when`, but it doesn't seem to exist yet. Maybe
    -- upgrade base?
    if suit == T.Hearts then T.Hearts `longerThan` T.Spades else return ()
    if suit == T.Spades then T.Spades `atLeastAsLong` T.Hearts else return ()
    makeCall $ T.Bid 1 suit


b2C :: Action
b2C = nameAction "smp_b2C" $ do
    _canOpen
    forbid b1C
    forEach T.majorSuits (forbid . b1M)
    minSuitLength T.Clubs 6
    makeAlertableCall (T.Bid 2 T.Clubs) ("10" .+ NDash .+ "15 HCP, 6+ clubs")


b2D :: Action
b2D = nameAction "smp_b2D" $ do
    _canOpen
    forbid b1C
    constrain "two_diamond_opener" ["shape(", ", 4414 + 4405 + 4315 + 3415)"]
    makeAlertableCall (T.Bid 2 T.Diamonds) "4414, 4315, 3415, or 4405 shape"


b1D :: Action
b1D = nameAction "smp_b1D" $ do
    _canOpen
    forbidAll [b1C, b1N, b1M T.Hearts, b1M T.Spades, b2C, b2D, b2N]
    -- The next line is commented out because if it can be violated, we're gonna
    -- have a bad day. Make sure that it's never violated in the results even if
    -- it's not explicitly required.
    --minSuitLength T.Diamonds 2
    makeAlertableCall (T.Bid 1 T.Diamonds) "Could be as short as 2 diamonds"


-- A replacement for CommonBids.setOpener. Note that a player with a weak hand
-- and 6 diamonds might still pass, because they can't open a weak 2D. So, use
-- cannotPreempt2H instead of the more general version.
setOpener :: T.Direction -> Action
setOpener opener = do
    (bidding, _) <- get
    if opener == currentBidder bidding
    then _canOpen
    else forbid _canOpen >> cannotPreempt2H >> makePass >> setOpener opener


-- unexported helper
_canOpen :: Action
_canOpen = nameAction "smp_opening" $ alternatives [ pointRange 11 40
                                                   , do forbid smpBalancedHand_
                                                        pointRange 10 40
                                                        maxLoserCount 7
                                                   ]
