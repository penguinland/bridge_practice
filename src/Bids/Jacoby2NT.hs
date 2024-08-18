module Bids.Jacoby2NT(
    b1H  -- Re-exported from StandardOpenings
  , b1H2N
  , b1H2N3C
  , b1H2N3D
  , b1H2N3H
  , b1H2N3H3S
  , b1H2N3H4C
  , b1H2N3H4D
  , b1H2N3H4H
  , b1H2N3S
  , b1H2N3N
  , b1H2N3N4C
  , b1H2N3N4D
  , b1H2N3N4H
  , b1H2N4C
  , b1H2N4D
  , b1H2N4H
  , b1H2N4HP
  , b1H2N4H4N
  , b1H3S
  , b1H4C
  , b1H4D
  , b1S  -- Re-exported from StandardOpenings
  , b1S2N
  , b1S2N3C
  , b1S2N3D
  , b1S2N3H
  , b1S2N3S
  , b1S2N3S4C
  , b1S2N3S4D
  , b1S2N3S4H
  , b1S2N3S4S
  , b1S2N3N
  , b1S2N3N4C
  , b1S2N3N4D
  , b1S2N3N4H
  , b1S2N3N4S
  , b1S2N4C
  , b1S2N4D
  , b1S2N4H
  , b1S2N4S
  , b1S2N4SP
  , b1S2N4S4N
  , b1S4C
  , b1S4D
  , b1S4H
) where


import Action(Action)
import Bids.StandardOpenings(b1H, b1S)
import EDSL(minSuitLength, maxSuitLength, makeCall, makeAlertableCall,
            pointRange, soundHolding, minLoserCount, maxLoserCount, forbidAll,
            shorterThan, atMostAsLong, forEach, hasControl, forbid)
import Output((.+))
import qualified Terminology as T


-- First start with splinter bids; prefer them over J2NT. If you have multiple
-- singletons, bid the cheapest one.
splinter_ :: T.Suit -> T.Suit -> T.Call -> Action
splinter_ trump shortness bid = do
    minSuitLength trump 4
    maxSuitLength shortness 1
    pointRange 10 13
    makeAlertableCall bid ("splinter: 4+ " .+ show trump .+ ", " .+
                           (init $ show shortness) .+ " shortness")

b1H3S :: Action
b1H3S = do
    T.Spades `atMostAsLong` T.Clubs
    T.Spades `atMostAsLong` T.Diamonds
    splinter_ T.Hearts T.Spades (T.Bid 3 T.Spades)

b1H4C :: Action
b1H4C = do
    T.Clubs `shorterThan` T.Spades
    T.Clubs `atMostAsLong` T.Diamonds
    splinter_ T.Hearts T.Clubs (T.Bid 4 T.Clubs)

b1H4D :: Action
b1H4D = do
    T.Diamonds `shorterThan` T.Spades
    T.Diamonds `shorterThan` T.Clubs
    splinter_ T.Hearts T.Diamonds (T.Bid 4 T.Diamonds)

b1S4C :: Action
b1S4C = do
    T.Clubs `atMostAsLong` T.Diamonds
    T.Clubs `atMostAsLong` T.Hearts
    splinter_ T.Spades T.Clubs (T.Bid 4 T.Clubs)

b1S4D :: Action
b1S4D = do
    T.Diamonds `shorterThan` T.Clubs
    T.Diamonds `atMostAsLong` T.Hearts
    splinter_ T.Spades T.Diamonds (T.Bid 4 T.Diamonds)

b1S4H :: Action
b1S4H = do
    T.Hearts `shorterThan` T.Clubs
    T.Hearts `shorterThan` T.Diamonds
    splinter_ T.Spades T.Hearts (T.Bid 4 T.Hearts)


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

sideSuitRebidsH_ :: [Action]
sideSuitRebidsH_ = [b1H2N4C, b1H2N4D]

b1S2N4C :: Action
b1S2N4C = openerSideSuit_ T.Clubs

b1S2N4D :: Action
b1S2N4D = openerSideSuit_ T.Diamonds

b1S2N4H :: Action
b1S2N4H = openerSideSuit_ T.Hearts

sideSuitRebidsS_ :: [Action]
sideSuitRebidsS_ = [b1S2N4C, b1S2N4D, b1S2N4H]


-- Show shortness if you can't show length. If you have two singletons (or two
-- voids), bid the lower one.
openerShortness_ :: T.Suit -> [Action] -> Action
openerShortness_ shortSuit preferredBids = do
    forbidAll preferredBids
    maxSuitLength shortSuit 1
    makeAlertableCall (T.Bid 3 shortSuit) ("shortness in " .+ show shortSuit)

b1H2N3C :: Action
b1H2N3C = do
    T.Clubs `atMostAsLong` T.Diamonds
    T.Clubs `atMostAsLong` T.Spades
    openerShortness_ T.Clubs sideSuitRebidsH_

b1H2N3D :: Action
b1H2N3D = do
    T.Diamonds `shorterThan` T.Clubs
    T.Diamonds `atMostAsLong` T.Spades
    openerShortness_ T.Diamonds sideSuitRebidsH_

b1H2N3S :: Action
b1H2N3S = do
    T.Spades `shorterThan` T.Clubs
    T.Spades `shorterThan` T.Diamonds
    openerShortness_ T.Spades sideSuitRebidsH_

b1S2N3C :: Action
b1S2N3C = do
    T.Clubs `atMostAsLong` T.Diamonds
    T.Clubs `atMostAsLong` T.Hearts
    openerShortness_ T.Clubs sideSuitRebidsS_

b1S2N3D :: Action
b1S2N3D = do
    T.Diamonds `shorterThan` T.Clubs
    T.Diamonds `atMostAsLong` T.Hearts
    openerShortness_ T.Diamonds sideSuitRebidsS_

b1S2N3H :: Action
b1S2N3H = do
    T.Hearts `shorterThan` T.Clubs
    T.Hearts `shorterThan` T.Diamonds
    openerShortness_ T.Hearts sideSuitRebidsS_

shapelyRebidsH_ :: [Action]
shapelyRebidsH_ = [b1H2N4C, b1H2N4D, b1H2N3C, b1H2N3D, b1H2N3S]

shapelyRebidsS_ :: [Action]
shapelyRebidsS_ = [b1S2N4C, b1S2N4D, b1S2N4H, b1S2N3C, b1S2N3D, b1S2N3H]


-- If you can't show length or shortness, show strength

b1H2N4H :: Action
b1H2N4H = do
    forbidAll shapelyRebidsH_
    pointRange 0 13
    makeCall $ T.Bid 4 T.Hearts


b1H2N3N :: Action
b1H2N3N = do
    forbidAll shapelyRebidsH_
    pointRange 14 15
    makeCall $ T.Bid 3 T.Notrump


b1H2N3H :: Action
b1H2N3H = do
    forbidAll shapelyRebidsH_
    pointRange 16 40
    makeCall $ T.Bid 3 T.Hearts


b1S2N4S :: Action
b1S2N4S = do
    forbidAll shapelyRebidsS_
    pointRange 0 13
    makeCall $ T.Bid 4 T.Spades


b1S2N3N :: Action
b1S2N3N = do
    forbidAll shapelyRebidsS_
    pointRange 14 15
    makeCall $ T.Bid 3 T.Notrump


b1S2N3S :: Action
b1S2N3S = do
    forbidAll shapelyRebidsS_
    pointRange 16 40
    makeCall $ T.Bid 3 T.Spades


-- Signoffs by responder

b1H2N4HP :: Action
b1H2N4HP = do
    minLoserCount 6
    makeCall T.Pass


b1S2N4SP :: Action
b1S2N4SP = do
    minLoserCount 6
    makeCall T.Pass


b1H2N3N4H :: Action
b1H2N3N4H = do
    minLoserCount 7
    makeCall $ T.Bid 4 T.Hearts


b1S2N3N4S :: Action
b1S2N3N4S = do
    minLoserCount 7
    makeCall $ T.Bid 4 T.Spades

-- Slam investigations

b1H2N4H4N :: Action
b1H2N4H4N = do
    forbid b1H2N4HP
    -- Don't bid Blackwood with a void
    forEach T.allSuits (`minSuitLength` 1)
    makeCall $ T.Bid 4 T.Notrump


b1S2N4S4N :: Action
b1S2N4S4N = do
    forbid b1S2N4SP
    -- Don't bid Blackwood with a void
    forEach T.allSuits (`minSuitLength` 1)
    makeCall $ T.Bid 4 T.Notrump


-- Control bids over 3N

b1H2N3N4C :: Action
b1H2N3N4C = do
    forbid b1H2N3N4H
    hasControl T.Clubs
    makeCall $ T.Bid 4 T.Clubs


b1H2N3N4D :: Action
b1H2N3N4D = do
    forbid b1H2N3N4H
    forbid $ hasControl T.Clubs
    hasControl T.Diamonds
    makeCall $ T.Bid 4 T.Diamonds


-- TODO: if hearts are trump, you have slam interest after opener's 3N rebid,
-- but you don't have control in either minor, should you show a spade
-- control, or go straight to asking for keycards? I'm unsure, so skipping that
-- entirely for now...


b1S2N3N4C :: Action
b1S2N3N4C = do
    forbid b1S2N3N4S
    hasControl T.Clubs
    makeCall $ T.Bid 4 T.Clubs


b1S2N3N4D :: Action
b1S2N3N4D = do
    forbid b1S2N3N4S
    forbid $ hasControl T.Clubs
    hasControl T.Diamonds
    makeCall $ T.Bid 4 T.Diamonds


b1S2N3N4H :: Action
b1S2N3N4H = do
    forbid b1S2N3N4S
    forbid $ hasControl T.Clubs
    forbid $ hasControl T.Diamonds
    hasControl T.Hearts
    makeCall $ T.Bid 4 T.Hearts


-- Control bids over 3M

b1H2N3H3S :: Action
b1H2N3H3S = do
    hasControl T.Spades
    makeCall $ T.Bid 3 T.Spades


b1H2N3H4C :: Action
b1H2N3H4C = do
    forbid b1H2N3H3S
    hasControl T.Clubs
    makeCall $ T.Bid 4 T.Clubs


b1H2N3H4D :: Action
b1H2N3H4D = do
    forbid b1H2N3H3S
    forbid b1H2N3H4C
    hasControl T.Diamonds
    makeCall $ T.Bid 4 T.Diamonds


b1H2N3H4H :: Action
b1H2N3H4H = do
    forbid b1H2N3H3S
    forbid b1H2N3H4C
    forbid b1H2N3H4H
    makeCall $ T.Bid 4 T.Hearts


b1S2N3S4C :: Action
b1S2N3S4C = do
    hasControl T.Clubs
    makeCall $ T.Bid 4 T.Clubs


b1S2N3S4D :: Action
b1S2N3S4D = do
    forbid b1S2N3S4C
    hasControl T.Diamonds
    makeCall $ T.Bid 4 T.Diamonds


b1S2N3S4H :: Action
b1S2N3S4H = do
    forbid b1S2N3S4C
    forbid b1S2N3S4D
    hasControl T.Hearts
    makeCall $ T.Bid 4 T.Hearts


b1S2N3S4S :: Action
b1S2N3S4S = do
    forbid b1S2N3S4C
    forbid b1S2N3S4D
    forbid b1S2N3S4H
    makeCall $ T.Bid 4 T.Spades
