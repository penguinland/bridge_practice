module Bids.StandardModernPrecision.Transfers1D1MLin(
    b1D    -- re-exported from BasicBids
  , b1Do1H
  , b1Do1HX
  , b1Do1S
  , b1Do1SX
) where

import Action(Action)
import Bids.StandardModernPrecision.BasicBids(b1D)
import qualified EDSL as E
import Output(Punct(..), (.+))
import qualified Terminology as T


b1Do1M :: T.Suit -> Action
b1Do1M suit = nameAction ("smp_b1Do1" ++ T.suitLetter suit) $ do
    E.pointRange 8 17  -- With a stronger hand, make a power double
    E.minSuitLength suit 5
    -- With a weak hand and long suit, bid pre-emptively instead
    E.forbid (E.pointRange 5 10 >> E.minSuitLength suit 6)
    E.makeCall $ T.Bid 1 suit

b1Do1H :: Action
b1Do1H = b1Do1M T.Hearts
b1Do1S :: Action
b1Do1S = b1Do1M T.Spades


b1Do1HX :: Action
b1Do1HX = nameAction "smp_b1Do1HX" $ do
    E.pointRange 6 40
    E.minSuitLength T.Spades 4
    E.forEach T.allSuits (T.Spades `E.atLeastAsLong`)
    E.forbid E.balancedHand
    E.makeAlertableCall T.Double "4+ spades, not a negative double!"


b1Do1SX :: Action
b1Do1SX = nameAction "smp_b1Do1HX" $ do
    E.pointRange 6 40
    E.suitLength T.Hearts 4
    E.makeCall T.Double  -- Not alertable: just a negative double

