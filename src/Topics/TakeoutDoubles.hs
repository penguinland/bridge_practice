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


makePowerX :: Situations
makePowerX = let
    sit (opener, openerSuit) = let
        action = do
            setOpener T.East
            opener
        explanation =
            "The opponents have opened the bidding, and we've got a super " .+
            "strong hand. To convey this extra strength, start with a " .+
            "power double. Partner will think it's a takeout double, but " .+
            "instead of supporting their suit, we will bid our own suit " .+
            "(or notrump) later, and then partner will realize how powerful " .+
            "our hand is."
      in situation "powx" action (B.powerDouble openerSuit) explanation
  in
    wrap $ return sit <~ [ (B.b1C, T.Clubs)
                         , (B.b1D, T.Diamonds)
                         , (B.b1H, T.Hearts)
                         , (B.b1S, T.Spades)
                         ]
                      -- To make a power double, you must be an unpassed hand.
                      <~ [T.West, T.North, T.East]
                      <~ T.allVulnerabilities


advanceSuit :: Situations
advanceSuit = let
    sit (opener, openerSuit, overcall, advance) = let
        action = do
            setOpener T.West
            _ <- opener
            _ <- overcall
            B.responderPasses openerSuit
        explanation =
            "The opponents have opened the bidding, partner has made " .+
            "a takeout double, and the next player has passed. We need " .+
            "to take the double out by bidding our favorite suit. Do this " .+
            "even with 0 high card points! Partner has support for us, " .+
            "and it beats letting the opponents play doubled at the 1 level."
      in situation "advS" action advance explanation
  in
    wrapDlr $ return sit <~ [ (B.b1C, T.Clubs,    B.b1CoX, B.b1CoXo1D)
                            , (B.b1C, T.Clubs,    B.b1CoX, B.b1CoXo1H)
                            , (B.b1C, T.Clubs,    B.b1CoX, B.b1CoXo1S)
                            , (B.b1D, T.Diamonds, B.b1DoX, B.b1DoXo1H)
                            , (B.b1D, T.Diamonds, B.b1DoX, B.b1DoXo1S)
                            , (B.b1D, T.Diamonds, B.b1DoX, B.b1DoXo2C)
                            , (B.b1H, T.Hearts,   B.b1HoX, B.b1HoXo1S)
                            , (B.b1H, T.Hearts,   B.b1HoX, B.b1HoXo2C)
                            , (B.b1H, T.Hearts,   B.b1HoX, B.b1HoXo2D)
                            , (B.b1S, T.Spades,   B.b1SoX, B.b1SoXo2C)
                            , (B.b1S, T.Spades,   B.b1SoX, B.b1SoXo2D)
                            , (B.b1S, T.Spades,   B.b1SoX, B.b1SoXo2H)
                            ]


-- bid 1N in response to partner's takeout double
-- convert the takeout double to penalty


topic :: Topic
topic = makeTopic "takeout doubles of 1-level suit openings" "tox" situations
  where
    situations = wrap [ wrap [makeTox, makeTox, makeTox, makeTox, makePowerX]
                      , advanceSuit
                      ]
