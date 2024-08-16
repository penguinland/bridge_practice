module Bids.Jacoby2N(
    b1H  -- Re-exported from StandardOpenings
  , b1H2N
  , b1H2N3C
  , b1H2N3D
  , b1H2N3H
  , b1H2N3S
  , b1H2N3N
  , b1H2N4C
  , b1H2N4D
  , b1H2N4H
  , b1H3S
  , b1H4C
  , b1H4D
  , b1S  -- Re-exported from StandardOpenings
  , b1S2N
  , b1S2N3C
  , b1S2N3D
  , b1S2N3H
  , b1S2N3S
  , b1S2N3N
  , b1S2N4C
  , b1S2N4D
  , b1S2N4H
  , b1S2N4S
  , b1S4C
  , b1S4D
  , b1S4H
) where


import Action(Action)
import Bids.StandardOpenings(b1H, b1S)
import EDSL(minSuitLength, maxSuitLength, makeCall, makeAlertableCall,
            pointRange, soundHolding, maxLoserCount, forbidAll)
import Output((.+))
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


-- Prefer for opener to rebid a 5-card side suit.
openerSideSuit_ :: T.Suit -> Action
openerSideSuit_ sideSuit = do
    minSuitLength sideSuit 5
    soundHolding sideSuit  -- Don't do this with a bad 5-card suit
    makeCall $ T.Bid 4 sideSuit

b1H2N4C :: Action
b1H2N4C = openerSideSuit_ T.Clubs

b1H2N4D :: Action
b1H2N4D = openerSideSuit_ T.Diamonds

-- If opener is 5-5 in the majors and opened a heart, they must either have
-- longer hearts or were planning to reverse.
-- TODO: what should we do about that? Is there ever a time when you "reverse"
-- to 4S?

b1S2N4C :: Action
b1S2N4C = openerSideSuit_ T.Clubs

b1S2N4D :: Action
b1S2N4D = openerSideSuit_ T.Diamonds

b1S2N4H :: Action
b1S2N4H = openerSideSuit_ T.Hearts


-- Show shortness if you can't show length.
openerShortness_ :: T.Suit -> [Action] -> Action
openerShortness_ shortSuit preferredBids = do
    forbidAll preferredBids
    maxSuitLength shortSuit 1
    makeAlertableCall (T.Bid 3 shortSuit) ("shortness in " .+ show shortSuit)

b1H2N3C :: Action
b1H2N3C = openerShortness_ T.Clubs [b1H2N4C, b1H2N4D]

b1H2N3D :: Action
b1H2N3D = openerShortness_ T.Diamonds [b1H2N4C, b1H2N4D]

b1H2N3S :: Action
b1H2N3S = openerShortness_ T.Spades [b1H2N4C, b1H2N4D]

b1S2N3C :: Action
b1S2N3C = openerShortness_ T.Clubs [b1S2N4C, b1S2N4D, b1S2N4H]

b1S2N3D :: Action
b1S2N3D = openerShortness_ T.Diamonds [b1S2N4C, b1S2N4D, b1S2N4H]

b1S2N3H :: Action
b1S2N3H = openerShortness_ T.Hearts [b1S2N4C, b1S2N4D, b1S2N4H]


-- If you can't show length or shortness, show strength
b1H2N4H :: Action
b1H2N4H = do
    forbidAll [b1H2N4C, b1H2N4D, b1H2N3C, b1H2N3D, b1H2N3S]
    pointRange 0 13
    makeCall $ T.Bid 4 T.Hearts

b1H2N3N :: Action
b1H2N3N = do
    forbidAll [b1H2N4C, b1H2N4D, b1H2N3C, b1H2N3D, b1H2N3S]
    pointRange 14 15
    makeCall $ T.Bid 3 T.Notrump

b1H2N3H :: Action
b1H2N3H = do
    forbidAll [b1H2N4C, b1H2N4D, b1H2N3C, b1H2N3D, b1H2N3S]
    pointRange 16 40
    makeCall $ T.Bid 3 T.Hearts


b1S2N4S :: Action
b1S2N4S = do
    forbidAll [b1S2N4C, b1S2N4D, b1S2N4H, b1S2N3C, b1S2N3D, b1S2N3H]
    pointRange 0 13
    makeCall $ T.Bid 4 T.Spades

b1S2N3N :: Action
b1S2N3N = do
    forbidAll [b1S2N4C, b1S2N4D, b1S2N4H, b1S2N3C, b1S2N3D, b1S2N3H]
    pointRange 14 15
    makeCall $ T.Bid 3 T.Notrump

b1S2N3S :: Action
b1S2N3S = do
    forbidAll [b1S2N4C, b1S2N4D, b1S2N4H, b1S2N3C, b1S2N3D, b1S2N3H]
    pointRange 16 40
    makeCall $ T.Bid 3 T.Spades
