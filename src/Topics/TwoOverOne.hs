module Topics.TwoOverOne(topic) where

import qualified Bids.TwoOverOne as TOO
import CommonBids(setOpener, noInterference)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapNW, Situations, makeTopic)



twoOverOne :: Situations
twoOverOne = let
    sit (openingSuit, openingBid, response) = let
        action = do
            setOpener T.North
            _ <- openingBid
            noInterference openingSuit
        explanation =
            "Partner opened the bidding. We've got game-forcing strength, " .+
            "so start by making a 2/1 bid. Once partner knows not to pass " .+
            "too low, we can explore further to find the right fit."
        in situation "2o1" action response explanation
  in
    wrapNW $ return sit <~ [ (T.Diamonds, TOO.b1D, TOO.b1D2C)
                           , (T.Hearts,   TOO.b1H, TOO.b1H2C)
                           , (T.Hearts,   TOO.b1H, TOO.b1H2D)
                           , (T.Spades,   TOO.b1S, TOO.b1S2C)
                           , (T.Spades,   TOO.b1S, TOO.b1S2D)
                           , (T.Spades,   TOO.b1S, TOO.b1S2H)
                           ]


-- TODO:
-- responder raises opener's major with 3-card support
-- opener rebids after responder's 2/1 bid
-- responder is too weak so bids 1N Forcing


topic :: Topic
topic = makeTopic "2/1 game forcing" "2o1" situations
  where
    situations = wrap [ twoOverOne
                      , twoOverOne
                      ]
