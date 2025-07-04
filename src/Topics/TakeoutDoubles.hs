module Topics.TakeoutDoubles(topic) where

import qualified Bids.TakeoutDoubles as B
import CommonBids(setOpener)
import EDSL(forbid)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapDlr, Situations, makeTopic)


makeTox :: Situations
makeTox = let
    sit (opener, double, openerSuit) = let
        action = do
            setOpener T.East
            _ <- opener
            forbid $ B.powerDouble openerSuit
        explanation =
            "The opponents have opened the bidding, and we've got around " .+
            "opening strength and support for every unbid suit. Make a " .+
            "takeout double. Partner will take it out by bidding their " .+
            "favorite suit, and you'll play there."
      in situation "tox" action double explanation
  in
    wrapDlr $ return sit <~ [ (B.b1C, B.b1CoX, T.Clubs)
                            , (B.b1D, B.b1DoX, T.Diamonds)
                            , (B.b1H, B.b1HoX, T.Hearts)
                            , (B.b1S, B.b1SoX, T.Spades)
                            ]


-- make a power double
-- bid a suit in response to partner's takeout double
-- bid 1N in response to partner's takeout double


topic :: Topic
topic = makeTopic "takeout doubles of 1-level suit openings" "tox" situations
  where
    situations = wrap [ makeTox
                      ]
