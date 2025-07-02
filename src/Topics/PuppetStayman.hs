module Topics.PuppetStayman(topic) where

import qualified Bids.PuppetStayman as P
import CommonBids(setOpener)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, stdWrap, wrapDlr, Situations, makeTopic)


-- Initial 3C
-- Initial 3C, even if 3-2 in majors
-- 5-card major rebids
-- 4-card major rebids
-- no major rebids
-- 5-card major raises
-- Texas transfers over 3N
-- smolen-like re-responses
-- opener sets the contract after smol


threeClubs :: Situations
threeClubs = let
    sit = let
        action = do
            setOpener T.North
            P.b2N
            P.noInterference
        explanation =
            "Partner has opened " .+ T.Bid 2 T.Notrump .+ ". We've got " .+
            "game-forcing strength, and it's possible we've got a " .+
            "major-suit fit. Initiate Puppet Stayman."
        in situation "3C" action P.b2N3C explanation
  in
    stdWrap sit


fiveCardMajor :: Situations
fiveCardMajor = let
    sit major = let
        action = do
            setOpener T.South
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", and partner bid " .+
            "Puppet Stayman. We've got a 5-card major, so bid it naturally. " .+
            "Partner will place the contract from there."
        in situation "5M" action major explanation
  in
    wrapDlr $ return sit <~ [P.b2N3C3H, P.b2N3C3S]


topic :: Topic
topic = makeTopic ("Puppet Stayman over " .+ T.Bid 2 T.Notrump) "pup" situations
  where
    situations = wrap [ threeClubs
                      , fiveCardMajor
                      ]
