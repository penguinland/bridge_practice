module Topics.Smolen(topic) where

import qualified Bids.OneNotrump as B
import CommonBids(setOpener)
import EDSL(alternatives)
import Output(Punct(..), (.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, stdWrap, stdWrapSE, wrap, wrapDlr, Situations, makeTopic)


stayman :: Situations
stayman = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            alternatives [B.b1N2C2D3H, B.b1N2C2D3S]
        explanation =
            "Partner opened a strong " .+ B.b1N .+ ". With 5" .+ NDash .+
            "4 in the majors and game-forcing strength, start by bidding " .+
            "Stayman. If opener has a major, we're in good shape. If they " .+
            "don't, we can bid Smolen afterwards to look for a 5" .+ NDash .+
            "3 fit, and still stop in " .+ T.Bid 3 T.Notrump .+ " if we " .+
            "don't find one."
      in situation "stay" action B.b1N2C explanation
  in
    stdWrap sit


smolen :: Situations
smolen = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            B.b1N2C
            B.noInterference
            B.b1N2C2D
            B.noInterference
        explanation =
            "Partner opened a strong " .+ B.b1N .+ ", we bid Stayman, and " .+
            "partner denied a 4-card major. They might still have a 3-card " .+
            "major, though. Jump in our shorter major to show our 5" .+ NDash .+
            "4 shape: partner then has a choice of games, depending on " .+
            "whether we have a major-suit fit."
      in situation "smol" action bid explanation
  in
    wrapDlr $ return sit <~ [B.b1N2C2D3H, B.b1N2C2D3S]


haveFitH :: Situations
haveFitH = let
    sit = let
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            B.b1N2C
            B.noInterference
            B.b1N2C2D
            B.noInterference
            B.b1N2C2D3S
            B.noInterference
        explanation =
            "We opened a strong " .+ B.b1N .+ ", partner bid Stayman, and " .+
            "we denied a 4-card major. Partner then bid Smolen, showing " .+
            "4 spades and 5 hearts. We've got a major-suit fit: bid it."
      in situation "fit" action B.b1N2C2D3S4H explanation
  in
    stdWrap sit


haveFitS :: Situations
haveFitS = let
    sit = let
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            B.b1N2C
            B.noInterference
            B.b1N2C2D
            B.noInterference
            B.b1N2C2D3H
            B.noInterference
        explanation =
            "We opened a strong " .+ B.b1N .+ ", partner bid Stayman, and " .+
            "we denied a 4-card major. Partner then bid Smolen, showing " .+
            "4 hearts and 5 spades. We've got a major-suit fit: bid it. " .+
            "Partner will probably just raise to game, but might start " .+
            "control bidding with slam interest."
      in situation "fit" action B.b1N2C2D3H3S explanation
  in
    -- For the comment about slam interest to make sense, North must be an
    -- unpassed hand.
    stdWrapSE sit


noFit :: Situations
noFit = let
    sit (smol, signoff) = let
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            B.b1N2C
            B.noInterference
            B.b1N2C2D
            B.noInterference
            _ <- smol
            B.noInterference
        explanation =
            "We opened a strong " .+ B.b1N .+ ", partner bid Stayman, and " .+
            "we denied a 4-card major. Partner then bid Smolen, showing " .+
            "4 hearts and 5 spades. We still don't have a major-suit fit: " .+
            "sign off in " .+ signoff .+ "."
      in situation "nof" action signoff explanation
  in
    wrapDlr $ return sit <~ [ (B.b1N2C2D3H, B.b1N2C2D3H3N)
                            , (B.b1N2C2D3S, B.b1N2C2D3S3N)
                            ]


topic :: Topic
topic = makeTopic "Smolen" "leb1N" situations
  where
    situations = wrap [ stayman
                      , smolen
                      , wrap [haveFitH, haveFitS]
                      , noFit
                      ]
