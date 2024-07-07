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
               makeCall, constrain)
import StandardOpenings(b1H, b1S)
import qualified Terminology as T


basicRaise :: T.Suit -> Action
basicRaise suit = do
    minSuitLength suit 3
    maxSuitLength suit 4 -- With 5+, you might jump straight to game, per LoTT
    pointRange 6 9
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
    pointRange 10 12
    makeCall $ T.Bid 3 suit

b1H3H :: Action
b1H3H = limitRaise T.Hearts

b1S3S :: Action
b1S3S = limitRaise T.Spades


blast3N :: T.Suit -> Action
blast3N suit = do
    pointRange 13 15
    suitLength suit 3
    constrain "fourtriple3" ["shape(", ", any 4333)"]
    makeCall $ T.Bid 3 T.Notrump

b1H3N :: Action
b1H3N = blast3N T.Hearts

b1S3N :: Action
b1S3N = blast3N T.Spades
