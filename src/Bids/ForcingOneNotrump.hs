module Bids.ForcingOneNotrump(
    b1H  -- Re-exported from StandardOpenings
  , b1H1N
  , b1H1N2C
  , b1H1N2D
  , b1H1N2H
  , b1H1N2S
  , b1H1N3C
  , b1H1N3D
  , b1H1N3H
  , b1S  -- Re-exported from StandardOpenings
  , b1S1N
  , b1S1N2C
  , b1S1N2D
  , b1S1N2H
  , b1S1N2S
  , b1S1N3C
  , b1S1N3D
  , b1S1N3H
  , b1S1N3S
  , b1M1N2N
) where


import Auction(Action, withholdBid)
import EDSL(pointRange, suitLength, minSuitLength, maxSuitLength, impliesThat,
            makeCall, makeAlertableCall, forbid, balancedHand, longerThan,
            atLeastAsLong, alternatives)
import qualified Bids.MajorSuitRaises as M
import StandardOpenings(b1H, b1S)
import qualified Terminology as T


-- The bad 12-point hands are also not game forcing, but coding that up is hard
notGameForcing :: Action
notGameForcing = pointRange 6 11


b1H1N :: Action
b1H1N = do
    notGameForcing
    forbid M.b1H2H
    forbid M.b1H3H
    maxSuitLength T.Spades 3
    -- If you've got a 10+ card heart fit, you should raise partner's suit
    -- immediately (likely by jumping to 4H, to show that you're not game
    -- forcing but should be in game anyway).
    maxSuitLength T.Hearts 4
    -- Are weak jump shifts or intermediate jump shifts more standard? I'm
    -- unsure; we need to practice neither one for now.
    -- TODO: remove this when adding support for jump shifts.
    mapM_ (`maxSuitLength` 6) T.minorSuits
    makeAlertableCall (T.Bid 1 T.Notrump) "forcing"


b1S1N :: Action
b1S1N = do
    notGameForcing
    forbid M.b1S2S
    forbid M.b1S3S
    -- TODO: implement splinters, and forbid them in here.
    maxSuitLength T.Spades 4  -- With 5+ spades, raise partner's suit instead
    suitLength T.Spades 4 `impliesThat` pointRange 6 9
    -- TODO: remove this when we support jump shifts.
    mapM_ (`maxSuitLength` 6) T.minorSuits
    makeAlertableCall (T.Bid 1 T.Notrump) "forcing"


b1M1N2N :: Action
b1M1N2N = do
    balancedHand
    pointRange 18 19
    makeCall $ T.Bid 2 T.Notrump


jumpShift :: T.Suit -> T.Suit -> Action
jumpShift firstSuit secondSuit = do
    pointRange 18 21
    minSuitLength secondSuit 4
    -- 3-suited hands might require nuance, so ensure this is a 2-suiter.
    mapM_ (`maxSuitLength` 3) .
        filter (\s -> s /= firstSuit && s /= secondSuit) $ T.allSuits
    -- If you've got 6 cards in a minor and 5 in a major, what would you open?
    -- I'm unsure; avoid that scenario.
    maxSuitLength secondSuit 5
    makeCall $ T.Bid 3 secondSuit

b1H1N3C :: Action
b1H1N3C = jumpShift T.Hearts T.Clubs

b1H1N3D :: Action
b1H1N3D = jumpShift T.Hearts T.Diamonds

b1S1N3C :: Action
b1S1N3C = jumpShift T.Spades T.Clubs

b1S1N3D :: Action
b1S1N3D = jumpShift T.Spades T.Diamonds

b1S1N3H :: Action
b1S1N3H = do
    -- If you're 5-5 in the majors, I'd start with 1H and reverse to 2S.
    suitLength T.Hearts 4
    jumpShift T.Spades T.Hearts

b1H1N2S :: Action
b1H1N2S = do
    -- This is just like a jump shift, but we bid at the 2 level, not 3
    withholdBid $ jumpShift T.Hearts T.Spades
    makeCall $ T.Bid 2 T.Spades


jumpRebid :: T.Suit -> [Action] -> Action
jumpRebid major jumpShifts = do
    pointRange 16 18
    minSuitLength major 6
    maxSuitLength major 7  -- With 8+ cards in the suit, just jump to game.
    mapM_ forbid jumpShifts
    makeCall $ T.Bid 3 major

b1H1N3H :: Action
b1H1N3H = jumpRebid T.Hearts [b1H1N3C, b1H1N3D, b1H1N2S]

b1S1N3S :: Action
b1S1N3S = jumpRebid T.Spades [b1S1N3C, b1S1N3D, b1S1N3H]


rebid :: T.Suit -> [Action] -> Action
rebid major strongBids = do
    mapM_ forbid strongBids
    minSuitLength major 6
    -- It's possible some rare, strong hands have slipped through the cracks of
    -- the strong bids. Make sure they don't show up here!
    pointRange 0 15
    -- If we're 6-5 in two suits, we'd rebid the second one.
    -- TODO: if we're 6-4, do we rebid the major or not? Does it matter if the
    -- second suit is also a major?
    mapM_ (`maxSuitLength` 4) . filter (/= major) $ T.allSuits
    -- TODO: If you're 6-4 with a minor, would you rebid spades or bid your
    -- minor? Avoid these possibilities, as I don't think there is consensus on
    -- the topic.
    maxSuitLength T.Clubs 3
    maxSuitLength T.Diamonds 3
    makeCall $ T.Bid 2 major

b1H1N2H :: Action
b1H1N2H = rebid T.Hearts [b1H1N3C, b1H1N3D, b1H1N3H, b1H1N2S, b1M1N2N]

b1S1N2S :: Action
b1S1N2S = do
    -- If you're 6-4 in the majors, I'd be seriously tempted to show the second
    -- major instead of rebidding the first. Avoid this possibility.
    maxSuitLength T.Hearts 3
    rebid T.Spades [b1S1N3C, b1S1N3D, b1S1N3H, b1S1N3S, b1M1N2N]


b1S1N2H :: Action
b1S1N2H = do
    -- Even if we've got 6+ spades, prefer to show the second major to give
    -- partner options.
    minSuitLength T.Hearts 4
    pointRange 0 16
    T.Spades `atLeastAsLong` T.Hearts
    T.Hearts `longerThan` T.Diamonds
    T.Hearts `longerThan` T.Clubs
    makeCall $ T.Bid 2 T.Hearts


-- TODO: if you've got a 5-card major and a 6-card minor, what do you open?


b1S1N2D :: Action
b1S1N2D = do
    minSuitLength T.Diamonds 3  -- might be 5332
    -- With 6-5, bid the second suit. With 6-4 (or 6-3!), rebid the major first.
    alternatives [minSuitLength T.Diamonds 5, maxSuitLength T.Spades 5]
    pointRange 0 16
    T.Spades `atLeastAsLong` T.Diamonds
    T.Diamonds `atLeastAsLong` T.Hearts
    T.Diamonds `longerThan` T.Clubs
    makeCall $ T.Bid 2 T.Diamonds


b1S1N2C :: Action
b1S1N2C = do
    minSuitLength T.Clubs 3  -- might be 5332
    -- With 6-5, bid the second suit. With 6-4 (or 6-3!), rebid the major first.
    alternatives [minSuitLength T.Clubs 5, maxSuitLength T.Spades 5]
    pointRange 0 16
    T.Spades `atLeastAsLong` T.Clubs
    T.Clubs `atLeastAsLong` T.Hearts
    T.Clubs `atLeastAsLong` T.Diamonds
    makeCall $ T.Bid 2 T.Clubs


b1H1N2D :: Action
b1H1N2D = do
    minSuitLength T.Diamonds 3
    -- With 6-5, bid the second suit. With 6-4 (or 6-3!), rebid the major first.
    alternatives [minSuitLength T.Diamonds 5, maxSuitLength T.Hearts 5]
    pointRange 0 16
    T.Hearts `atLeastAsLong` T.Diamonds
    T.Diamonds `longerThan` T.Clubs
    makeCall $ T.Bid 2 T.Diamonds


b1H1N2C :: Action
b1H1N2C = do
    minSuitLength T.Clubs 3
    -- With 6-5, bid the second suit. With 6-4 (or 6-3!), rebid the major first.
    alternatives [minSuitLength T.Clubs 5, maxSuitLength T.Hearts 5]
    pointRange 0 16
    T.Hearts `atLeastAsLong` T.Clubs
    T.Clubs `atLeastAsLong` T.Diamonds
    makeCall $ T.Bid 2 T.Clubs
