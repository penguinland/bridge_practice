module Bids.ForcingOneNotrump(
    b1H  -- Re-exported from StandardOpenings
  , b1H1N
  , b1H1N2S
  , b1H1N3C
  , b1H1N3D
  , b1H1N3H
  , b1S  -- Re-exported from StandardOpenings
  , b1S1N
  , b1S1N3C
  , b1S1N3D
  , b1S1N3H
  , b1S1N3S
  , b1M1N2N
) where


import Auction(pointRange, suitLength, minSuitLength, maxSuitLength, Action,
               makeCall, makeAlertableCall, forbid, balancedHand, withholdBid,
               impliesThat)
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
    maxSuitLength T.Spades 4  -- With 5+ spades, raise partner's suit instead
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


jumpRebid :: T.Suit -> Action
jumpRebid major = do
    pointRange 17 40
    minSuitLength major 6
    -- With extra strength and extra length, just jump straight to 4.
    minSuitLength major 7 `impliesThat` pointRange 17 19
    -- If we were 6-4 in two suits and strong enough to jump, we should probably
    -- bid our second suit instead.
    mapM_ (`maxSuitLength` 3) . filter (/= major) $ T.allSuits
    makeCall $ T.Bid 3 major

b1H1N3H :: Action
b1H1N3H = jumpRebid T.Hearts

b1S1N3S :: Action
b1S1N3S = jumpRebid T.Spades
