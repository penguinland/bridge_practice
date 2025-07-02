module Topics.PuppetStayman(topic) where

import qualified Bids.PuppetStayman as P
import CommonBids(setOpener)
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, stdWrap, wrapDlr, Situations, makeTopic)


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
            "major-suit fit. Initiate puppet Stayman."
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
            "puppet Stayman. We've got a 5-card major, so bid it naturally. " .+
            "Partner will place the contract from there."
        in situation "5M" action major explanation
  in
    wrapDlr $ return sit <~ [P.b2N3C3H, P.b2N3C3S]


fourCardMajor :: Situations
fourCardMajor = let
    sit = let
        action = do
            setOpener T.South
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", and partner bid " .+
            "puppet Stayman. We don't have a 5-card major, but do have a " .+
            "4-card major. Bid " .+ P.b2N3C3D .+ " to show this."
        in situation "4M" action P.b2N3C3D explanation
  in
    stdWrap sit


noMajor :: Situations
noMajor = let
    sit = let
        action = do
            setOpener T.South
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", and partner bid " .+
            "puppet Stayman. We don't even have a 4-card major: go " .+
            "straight to " .+ P.b2N3C3N .+ ". This often ends the auction, " .+
            "though partner might still make a Texas transfer if they're " .+
            "6" .+ NDash .+ "4 in the majors."
        in situation "NM" action P.b2N3C3N explanation
  in
    stdWrap sit


-- Initial 3C, even if 3-2 in majors
-- 5-card major raises
-- Texas transfers over 3N
-- smolen-like re-responses
-- opener sets the contract after smol


topic :: Topic
topic = makeTopic ("puppet Stayman over " .+ T.Bid 2 T.Notrump) "pup" situations
  where
    situations = wrap [ threeClubs
                      , wrap [fiveCardMajor, fourCardMajor, noMajor]
                      , noMajor
                      ]
