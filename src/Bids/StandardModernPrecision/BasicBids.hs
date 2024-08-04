module Bids.StandardModernPrecision.BasicBids(
    lessThanInvitational
  , invitational
  , canOpen
  , firstSeatOpener
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
  -- syntactic sugar
  , smpWrapN
  , smpWrapS
  , setOpener
) where

import Control.Monad.Trans.State.Strict(State, get)
import System.Random(StdGen)

import Action(Action, constrain)
import CommonBids(cannotPreempt)
import EDSL(forbid, pointRange, suitLength, minSuitLength, hasTopN,
            balancedHand, makeCall, makeAlertableCall, makePass, alternatives,
            minLoserCount, maxLoserCount, forEach, forbidAll)
import Output(Punct(..), (.+))
import Situation(Situation, (<~))
import Structures(currentBidder)
import qualified Terminology as T
import Topic(wrap, Situations)


lessThanInvitational :: Action
lessThanInvitational = do
    pointRange 0 10
    minLoserCount 9


invitational :: Action
invitational = do
    pointRange 11 13
    minLoserCount 7  -- TODO: Is this right? Maybe it should be exactly 8 losers
    maxLoserCount 8


-- Because we're not using the Rule of 20 and its ilk, we're going to skip the
-- auctions that start with other folks passing for now, and maybe come back to
-- those later.
canOpen :: Action
canOpen = alternatives [ pointRange 11 40
                               , pointRange 10 40 >> maxLoserCount 7
                               ]
firstSeatOpener :: Action
firstSeatOpener = canOpen

oppsPass :: Action
oppsPass = do
    cannotPreempt
    makePass


------------------
-- Opening bids --
------------------
b1N :: Action
b1N = do
    pointRange 14 16
    balancedHand
    makeAlertableCall (T.Bid 1 T.Notrump) ("14" .+ NDash .+ "16 HCP")


b2N :: Action
b2N = do
    pointRange 19 20  -- A modification from Part 1
    balancedHand
    makeAlertableCall (T.Bid 2 T.Notrump) ("19" .+ NDash .+ "20 HCP")


b3N :: Action
b3N = do
    alternatives . map (\suit -> minSuitLength suit 7 >> hasTopN suit 5 4) $
        T.minorSuits
    makeAlertableCall (T.Bid 3 T.Notrump) "Gambling: long running minor"


b1C :: Action
b1C = do
    pointRange 16 40
    forbidAll [b1N, b2N]
    makeAlertableCall (T.Bid 1 T.Clubs) "16+ HCP, any shape"


b1M :: T.Suit -> Action
b1M suit = do
    canOpen
    forbidAll [b1C, b1N, b2N]
    minSuitLength suit 5
    -- If you're a maximum with a 6-card minor and 5-card major, open the minor.
    forEach T.minorSuits (\minor -> forbid (
        pointRange 14 15 >> minSuitLength minor 6 >> suitLength suit 5))
    makeCall $ T.Bid 1 suit


b2C :: Action
b2C = do
    canOpen
    forbid b1C
    forEach T.majorSuits (forbid . b1M)
    minSuitLength T.Clubs 6
    makeAlertableCall (T.Bid 2 T.Clubs) ("10" .+ NDash .+ "15 HCP, 6+ clubs")


b2D :: Action
b2D = do
    canOpen
    forbid b1C
    constrain "two_diamond_opener" ["shape(", ", 4414 + 4405 + 4315 + 3415)"]
    makeAlertableCall (T.Bid 2 T.Diamonds) "4414, 4315, 3415, or 4405 shape"


b1D :: Action
b1D = do
    canOpen
    forbidAll [b1C, b1N, b1M T.Hearts, b1M T.Spades, b2C, b2D, b2N]
    -- The next line is commented out because if it can be violated, we're gonna
    -- have a bad day. Make sure that it's never violated in the results even if
    -- it's not explicitly required.
    --minSuitLength T.Diamonds 2
    makeAlertableCall (T.Bid 1 T.Diamonds) "Could be as short as 2 diamonds"


-------------------------------
-- Situation syntactic sugar --
-------------------------------
-- Always make opener be in first seat, until we figure out how to open in other
-- seats.
-- TODO: change this to let other folks be dealer, too
smpWrapS :: State StdGen (T.Vulnerability -> T.Direction -> Situation) ->
            Situations
smpWrapS sit = wrap $ sit <~ T.allVulnerabilities <~ [T.South]

smpWrapN :: State StdGen (T.Vulnerability -> T.Direction -> Situation) ->
            Situations
smpWrapN sit = wrap $ sit <~ T.allVulnerabilities <~ [T.North]


-- A replacement for CommonBids.setOpener
setOpener :: T.Direction -> Action
setOpener opener = do
    (bidding, _) <- get
    if opener == currentBidder bidding
    then canOpen
    else forbid canOpen >> cannotPreempt >> makePass >> setOpener opener
