module Bids.TransfersOver1MX(
    b1H    -- Re-exported from TakeoutDoubles
  , b1HoX  -- Re-exported from TakeoutDoubles
  --, b1HoXXX
  , b1HoX1S
  , b1HoX1N
  , b1HoX1N2C
  , b1HoX2C
  , b1HoX2C2D
  , b1HoX2D
  , b1HoX2D2H
  , b1HoX2H
  , b1S    -- Re-exported from TakeoutDoubles
  , b1SoX  -- Re-exported from TakeoutDoubles
  --, b1SoXXX
  , b1SoX1N
  , b1SoX1N2C
  , b1SoX2C
  , b1SoX2C2D
  , b1SoX2D
  , b1SoX2D2H
  , b1SoX2H
  , b1SoX2H2S
  , b1SoX2S
) where

import Action(Action)
import Bids.TakeoutDoubles(b1H, b1HoX, b1S, b1SoX)
import EDSL(suitLength, minSuitLength, maxSuitLength, pointRange, soundHolding,
            nameAction, alternatives, makeCall, makeAlertableCall, forEach)
import qualified Terminology as T


transferBid_ :: T.Suit -> T.Suit -> Action
transferBid_ openerSuit ourSuit = let
    leadDirectingLimitRaise = do
        soundHolding ourSuit
        suitLength openerSuit 3
        pointRange 11 40
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


b1HoX2C :: Action
b1HoX2C = nameAction "b1HoX2C" $ do
    transferBid_ T.Hearts T.Diamonds
    makeAlertableCall (T.Bid 1 T.Clubs) "transfer to diamonds"


b1HoX2C2D :: Action
b1HoX2C2D = nameAction "b1HoX2C2D" $ completeTransfer_ T.Diamonds


b1HoX2D :: Action
b1HoX2D = nameAction "b1HoX2D" $ do
    suitLength T.Hearts 3
    pointRange 8 10
    makeAlertableCall (T.Bid 2 T.Diamonds) "constructive raise in hearts"


b1HoX2D2H :: Action
b1HoX2D2H = nameAction "b1HoX2D2H" $ completeTransfer_ T.Hearts


b1HoX2H :: Action
b1HoX2H = nameAction "b1HoX2H" $ do
    suitLength T.Hearts 3
    pointRange 5 7
    makeAlertableCall (T.Bid 2 T.Hearts) "weakest possible heart raise"


b1SoX1N :: Action
b1SoX1N = nameAction "b1SoX1N" $ do
    transferBid_ T.Spades T.Clubs
    makeAlertableCall (T.Bid 1 T.Notrump) "transfer to clubs"


b1SoX1N2C :: Action
b1SoX1N2C = nameAction "b1SoX1N2C" $ completeTransfer_ T.Clubs


b1SoX2C :: Action
b1SoX2C = nameAction "b1SoX2C" $ do
    transferBid_ T.Spades T.Diamonds
    makeAlertableCall (T.Bid 1 T.Clubs) "transfer to diamonds"


b1SoX2C2D :: Action
b1SoX2C2D = nameAction "b1SoX2C2D" $ completeTransfer_ T.Diamonds


b1SoX2D :: Action
b1SoX2D = nameAction "b1SoX2D" $ do
    transferBid_ T.Spades T.Hearts
    makeAlertableCall (T.Bid 1 T.Diamonds) "transfer to hearts"


b1SoX2D2H :: Action
b1SoX2D2H = nameAction "b1SoX2D2H" $ completeTransfer_ T.Hearts


b1SoX2H :: Action
b1SoX2H = nameAction "b1SoX2H" $ do
    suitLength T.Spades 3
    pointRange 8 10
    makeAlertableCall (T.Bid 2 T.Hearts) "constructive raise in spades"


b1SoX2H2S :: Action
b1SoX2H2S = nameAction "b1SoX2H2S" $ completeTransfer_ T.Spades


b1SoX2S :: Action
b1SoX2S = nameAction "b1SoX2S" $ do
    suitLength T.Spades 3
    pointRange 5 7
    makeAlertableCall (T.Bid 2 T.Spades) "weakest possible spade raise"
