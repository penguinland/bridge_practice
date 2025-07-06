module Bids.TransfersOver1MX(
    b1H    -- Re-exported from TakeoutDoubles
  , b1HoX  -- Re-exported from TakeoutDoubles
  , b1HoXXX
  , b1HoX1S
  , b1HoX1N
  , b1HoX1N2C
  , b1HoX1N2CP
  , b1HoX1N2C2H
  , b1HoX2C
  , b1HoX2C2D
  , b1HoX2C2DP
  , b1HoX2C2D2H
  , b1HoX2D
  , b1HoX2D2H
  , b1HoX2H
  , b1S    -- Re-exported from TakeoutDoubles
  , b1SoX  -- Re-exported from TakeoutDoubles
  , b1SoXXX
  , b1SoX1N
  , b1SoX1N2C
  , b1SoX1N2CP
  , b1SoX1N2C2S
  , b1SoX2C
  , b1SoX2C2D
  , b1SoX2C2DP
  , b1SoX2C2D2S
  , b1SoX2D
  , b1SoX2D2H
  , b1SoX2D2HP
  , b1SoX2D2H2S
  , b1SoX2H
  , b1SoX2H2S
  , b1SoX2S
) where

import Action(Action)
import Bids.TakeoutDoubles(b1H, b1HoX, b1S, b1SoX)
import EDSL(suitLength, minSuitLength, maxSuitLength, pointRange, soundHolding,
            nameAction, alternatives, makeCall, makeAlertableCall, forEach,
            loserCount, minLoserCount, maxLoserCount, flatHand, forbid,
            strongerThan)
import qualified Terminology as T


transferBid_ :: T.Suit -> T.Suit -> Action
transferBid_ openerSuit ourSuit = let
    leadDirectingLimitRaise = do
        minSuitLength ourSuit 4
        soundHolding ourSuit
        suitLength openerSuit 3
        forEach (T.otherSuits ourSuit) (ourSuit `strongerThan`)
        pointRange 11 40
        forbid flatHand  -- You might just redouble with 4333 shape
    signoffNoFit = do
        maxSuitLength openerSuit 2
        minSuitLength ourSuit 6
        soundHolding ourSuit
        pointRange 6 9
  in
    alternatives [leadDirectingLimitRaise, signoffNoFit]


completeTransfer_ :: T.Suit -> Action
completeTransfer_ suit = do
    minSuitLength suit 2
    forEach T.allSuits (`maxSuitLength` 6)
    makeCall $ T.Bid 2 suit


signoff_ :: T.Suit -> Action
signoff_ openerSuit = do
    maxSuitLength openerSuit 2
    makeCall T.Pass


invite_ :: T.Suit -> Action
invite_ openerSuit = do
    suitLength openerSuit 3
    pointRange 10 12
    loserCount 8
    makeCall $ T.Bid 2 openerSuit


b1HoXXX :: Action
b1HoXXX = nameAction "b1HoXXX" $ do
    maxSuitLength T.Hearts 2
    pointRange 10 40
    forEach T.allSuits (`maxSuitLength` 4)
    -- You'd probably bid a 4-card major at the 1 level if you could.
    maxSuitLength T.Spades 3
    makeCall T.Redouble


b1HoX1S :: Action
b1HoX1S = nameAction "b1HoX1S" $ do
    minSuitLength T.Spades 4
    pointRange 8 40
    makeCall $ T.Bid 1 T.Spades


b1HoX1N :: Action
b1HoX1N = nameAction "b1HoX1N" $ do
    transferBid_ T.Hearts T.Clubs
    makeAlertableCall (T.Bid 1 T.Notrump) "transfer to clubs"


b1HoX1N2C :: Action
b1HoX1N2C = nameAction "b1HoX1N2C" $ completeTransfer_ T.Clubs


b1HoX1N2CP :: Action
b1HoX1N2CP = nameAction "b1HoX1N2CP" $ signoff_ T.Hearts


b1HoX1N2C2H :: Action
b1HoX1N2C2H = nameAction "b1HoX1N2C2H" $ invite_ T.Hearts


b1HoX2C :: Action
b1HoX2C = nameAction "b1HoX2C" $ do
    transferBid_ T.Hearts T.Diamonds
    makeAlertableCall (T.Bid 2 T.Clubs) "transfer to diamonds"


b1HoX2C2D :: Action
b1HoX2C2D = nameAction "b1HoX2C2D" $ completeTransfer_ T.Diamonds


b1HoX2C2DP :: Action
b1HoX2C2DP = nameAction "b1HoX2C2DP" $ signoff_ T.Hearts


b1HoX2C2D2H :: Action
b1HoX2C2D2H = nameAction "b1HoX2C2D2H" $ invite_ T.Hearts


b1HoX2D :: Action
b1HoX2D = nameAction "b1HoX2D" $ do
    suitLength T.Hearts 3
    pointRange 8 10
    minLoserCount 8
    makeAlertableCall (T.Bid 2 T.Diamonds) "constructive raise in hearts"


b1HoX2D2H :: Action
b1HoX2D2H = nameAction "b1HoX2D2H" $ do
    pointRange 0 16  -- If you're much stronger, you might invite
    maxLoserCount 8
    completeTransfer_ T.Hearts


b1HoX2H :: Action
b1HoX2H = nameAction "b1HoX2H" $ do
    suitLength T.Hearts 3
    pointRange 5 7
    forEach T.allSuits (`maxSuitLength` 5)
    -- If you had 4 spades, you might be tempted to make a free bid of 1S,
    -- hoping for a 4-4 spade fit in addition to the 5-3 heart fit, and knowing
    -- you can still sign off in 2H if you don't find it.
    maxSuitLength T.Spades 3
    makeAlertableCall (T.Bid 2 T.Hearts) "weakest possible heart raise"


b1SoXXX :: Action
b1SoXXX = nameAction "b1SoXXX" $ do
    maxSuitLength T.Spades 2
    pointRange 10 40
    forEach T.allSuits (`maxSuitLength` 4)
    makeCall T.Redouble


b1SoX1N :: Action
b1SoX1N = nameAction "b1SoX1N" $ do
    transferBid_ T.Spades T.Clubs
    makeAlertableCall (T.Bid 1 T.Notrump) "transfer to clubs"


b1SoX1N2C :: Action
b1SoX1N2C = nameAction "b1SoX1N2C" $ completeTransfer_ T.Clubs


b1SoX1N2CP :: Action
b1SoX1N2CP = nameAction "b1SoX1N2CP" $ signoff_ T.Spades


b1SoX1N2C2S :: Action
b1SoX1N2C2S = nameAction "b1SoX1N2C2S" $ invite_ T.Spades


b1SoX2C :: Action
b1SoX2C = nameAction "b1SoX2C" $ do
    transferBid_ T.Spades T.Diamonds
    makeAlertableCall (T.Bid 2 T.Clubs) "transfer to diamonds"


b1SoX2C2D :: Action
b1SoX2C2D = nameAction "b1SoX2C2D" $ completeTransfer_ T.Diamonds


b1SoX2C2DP :: Action
b1SoX2C2DP = nameAction "b1SoX2C2DP" $ signoff_ T.Spades


b1SoX2C2D2S :: Action
b1SoX2C2D2S = nameAction "b1SoX2C2D2S" $ invite_ T.Spades


b1SoX2D :: Action
b1SoX2D = nameAction "b1SoX2D" $ do
    transferBid_ T.Spades T.Hearts
    makeAlertableCall (T.Bid 2 T.Diamonds) "transfer to hearts"


b1SoX2D2H :: Action
b1SoX2D2H = nameAction "b1SoX2D2H" $ completeTransfer_ T.Hearts


b1SoX2D2HP :: Action
b1SoX2D2HP = nameAction "b1SoX2D2HP" $ signoff_ T.Spades


b1SoX2D2H2S :: Action
b1SoX2D2H2S = nameAction "b1SoX2D2H2S" $ invite_ T.Spades


b1SoX2H :: Action
b1SoX2H = nameAction "b1SoX2H" $ do
    suitLength T.Spades 3
    pointRange 8 9
    minLoserCount 8
    makeAlertableCall (T.Bid 2 T.Hearts) "constructive raise in spades"


b1SoX2H2S :: Action
b1SoX2H2S = nameAction "b1SoX2H2S" $ do
    pointRange 0 16  -- If you're much stronger, you might invite
    maxLoserCount 8
    completeTransfer_ T.Spades


b1SoX2S :: Action
b1SoX2S = nameAction "b1SoX2S" $ do
    suitLength T.Spades 3
    pointRange 5 7
    forEach T.allSuits (`maxSuitLength` 5)
    makeAlertableCall (T.Bid 2 T.Spades) "weakest possible spade raise"
