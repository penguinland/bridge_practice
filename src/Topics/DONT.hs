module Topics.DONT(topic) where

import Action(Action)
import qualified Bids.DONT as B
import CommonBids(setOpener)
import EDSL(pointRange, minSuitLength, maxSuitLength, makePass, forEach)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapVulDlr, stdWrap, Situations, makeTopic)


responderCannotBid :: Action
responderCannotBid = do
    pointRange 0 7                           -- No jumping to 3N, etc.
    forEach T.majorSuits (`maxSuitLength` 4) -- No transfers
    minSuitLength T.Clubs 2                  -- Avoid Garbage Stayman
    makePass


double :: Situations
double = let
    sit = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "We've got a single-suited hand. Double to show this. Partner " .+
            "will relay to " .+ T.Bid 2 T.Clubs .+ ", which we can pass or " .+
            "correct to whatever our suit is."
        in situation "dbl" action B.b1NoX explanation
  in
    stdWrap sit


poc2C :: Situations
poc2C = let
    sit = let
        action = do
            setOpener T.West
            B.b1N
            B.b1NoX
            responderCannotBid
        explanation =
            "Partner has shown a single-suited hand. Bid " .+ B.b1NoX2C .+
            "to ask partner what their suit is: they will pass with clubs " .+
            "and bid correct to their suit otherwise."
        in situation "poc2C" action B.b1NoX2C explanation
  in
    -- It is tremendously unlikely that West would open in pass-out seat and
    -- North will want to come in over that with a single-suited hand: North
    -- should probably have opened a weak two. So, ensure they're an unpassed
    -- hand.
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.South, T.East]


twoSuited :: Situations
twoSuited = let
    sit bid = let
        action = do
            setOpener T.East
            B.b1N
        explanation =
            "We've got a two-suited hand. Bid our lower suit: partner " .+
            "can pass if we've got a fit, or else they'll bid the " .+
            "next suit up, which we can pass or correct to our second suit."
        in situation "2suit" action bid explanation
  in
    wrapVulDlr $ return sit <~ [B.b1No2C, B.b1No2D, B.b1No2H]


topic :: Topic
topic = makeTopic "DONT over strong notrump" "MW1N" situations
  where
    situations = wrap [ double
                      , twoSuited
                      , poc2C
                      ]
