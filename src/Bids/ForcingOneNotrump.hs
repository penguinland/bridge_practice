module Bids.ForcingOneNotrump(
    b1H  -- Re-exported from StandardOpenings
  , b1H1N
  , b1H1N2S
  , b1H1N3C
  , b1H1N3D
  , b1S  -- Re-exported from StandardOpenings
  , b1S1N
  , b1S1N3C
  , b1S1N3D
  , b1S1N3H
  , b1M1N2N
) where


import Auction(pointRange, suitLength, minSuitLength, maxSuitLength, Action,
               makeCall, makeAlertableCall, forbid, balancedHand)
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
    makeAlertableCall (T.Bid 1 T.Notrump) "forcing"


b1S1N :: Action
b1S1N = do
    notGameForcing
    forbid M.b1S2S
    forbid M.b1S3S
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
    -- I'm unsure; avoid that situation.
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
b1H1N2S = jumpShift T.Hearts T.Spades
