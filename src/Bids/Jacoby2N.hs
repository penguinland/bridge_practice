module Bids.Jacoby2N(
    b1H  -- Re-exported from StandardOpenings
  , b1H2N
  , b1H3S
  , b1H4C
  , b1H4D
  , b1S  -- Re-exported from StandardOpenings
  , b1S2N
  , b1S4C
  , b1S4D
  , b1S4H
) where


import Action(Action)
import Bids.StandardOpenings(b1H, b1S)
import EDSL(pointRange, suitLength, minSuitLength, maxSuitLength, makeCall,
            flatHand, minLoserCount, forEach)
import qualified Terminology as T


-- First start with splinter bids; prefer them over J2NT.
splinter_ :: T.Suit -> T.Suit -> T.Call -> Action
splinter_ trump shortness bid = do
    minSuitLength trump 4
    maxSuitLength shortness 1
    pointRange 10 13
    makeAlertableCall bid ("splinter: 4+ " .+ show trump .+ ", " .+
                           (init $ show shortness) .+ " shortness")

b1H3S :: Action
b1H3S = splinter_ T.Hearts T.Spades (T.Bid 3 T.Spades)

b1H4C :: Action
b1H4C = splinter_ T.Hearts T.Clubs (T.Bid 4 T.Clubs)

b1H4D :: Action
b1H4D = splinter_ T.Hearts T.Diamonds (T.Bid 4 T.Diamonds)

b1S4C :: Action
b1S4C = splinter_ T.Spades T.Clubs (T.Bid 4 T.Clubs)

b1S4D :: Action
b1S4D = splinter_ T.Spades T.Diamonds (T.Bid 4 T.Diamonds)

b1S4H :: Action
b1S4H = splinter_ T.Spades T.Hearts (T.Bid 4 T.Hearts)


-- Define J2NT itself
b1H2N :: Action
b1H2N = do
    forbidAll [b1H3S, b1H4C, b1H4D]
    minSuitLength T.Hearts 4
    pointRange 12 40
    maxLoserCount 7
    makeAlertableCall (T.Bid 2 T.Notrump) "Jacoby: GF with 4+ hearts"

b1S2N :: Action
b1S2N = do
    forbidAll [b1S4C, b1S4D, b1S4H]
    minSuitLength T.Spades 4
    pointRange 12 40
    maxLoserCount 7
    makeAlertableCall (T.Bid 2 T.Notrump) "Jacoby: GF with 4+ spades"
