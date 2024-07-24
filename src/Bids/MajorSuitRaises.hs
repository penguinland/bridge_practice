module Bids.MajorSuitRaises(
    b1H  -- Re-exported from StandardOpenings
  , b1H2H
  , b1H3H
  , b1H3N
  , b1S  -- Re-exported from StandardOpenings
  , b1S2S
  , b1S3S
  , b1S3N
) where


import Auction(pointRange, minSuitLength, maxSuitLength, suitLength, Action,
               makeCall, flatHand, minLoserCount)
import StandardOpenings(b1H, b1S)
import qualified Terminology as T


basicRaise :: T.Suit -> Action
basicRaise suit = do
    minSuitLength suit 3
    maxSuitLength suit 4 -- With 5+, you might jump straight to game, per LoTT
    pointRange 6 9
    -- If you've got a freak distribution, you might instead want to either
    -- upgrade your hand or bid your other suit on the way to supporting
    -- partner. Ensure we don't have freak distribution.
    mapM_ (`maxSuitLength` 5) T.allSuits
    makeCall $ T.Bid 2 suit

b1H2H :: Action
b1H2H = basicRaise T.Hearts

b1S2S :: Action
b1S2S = basicRaise T.Spades


limitRaise :: T.Suit -> Action
limitRaise suit = do
    -- Make these always 4-card raises, in anticipation of 2/1 using a forcing
    -- 1N followed by a jump raise for 3-card raises. Also don't do 5-card
    -- raises, because a lot of the time those should just blast game.
    suitLength suit 4
    -- Ensure that there are no singletons or voids, with which you might
    -- instead want to splinter.
    mapM_ (`minSuitLength` 2) T.allSuits
    pointRange 10 12
    minLoserCount 8  -- With at most 7 losers, you should be game forcing.
    makeCall $ T.Bid 3 suit

b1H3H :: Action
b1H3H = limitRaise T.Hearts

b1S3S :: Action
b1S3S = limitRaise T.Spades


blast3N :: T.Suit -> Action
blast3N suit = do
    pointRange 13 15
    suitLength suit 3
    flatHand
    makeCall $ T.Bid 3 T.Notrump

b1H3N :: Action
b1H3N = blast3N T.Hearts

b1S3N :: Action
b1S3N = blast3N T.Spades
