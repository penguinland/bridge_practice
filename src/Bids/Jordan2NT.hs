module Bids.Jordan2NT(
    b1HoX2N
  , b1SoX2N
) where


import Action(Action)
import qualified Bids.Jacoby2NT as J
import qualified EDSL as E
import qualified Terminology as T


b1HoX2N :: Action
b1HoX2N = E.nameAction "jdn_b1HoX2N" $ do
    E.forbidAll [J.b1H3S, J.b1H4C, J.b1H4D]
    E.minSuitLength T.Hearts 4
    E.pointRange 10 40
    E.maxLoserCount 8
    E.makeAlertableCall (T.Bid 2 T.Notrump) "Limit+ with 4+ hearts"


b1SoX2N :: Action
b1SoX2N = E.nameAction "jdn_b1SoX2N" $ do
    E.forbidAll [J.b1S4C, J.b1S4D, J.b1S4H]
    E.minSuitLength T.Spades 4
    E.pointRange 10 40
    E.maxLoserCount 8
    E.makeAlertableCall (T.Bid 2 T.Notrump) "Limit+ with 4+ spades"
