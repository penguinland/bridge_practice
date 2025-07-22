module Topics.MuppetStayman(topic) where

import qualified Bids.MuppetStayman as M
import CommonBids(setOpener)
import EDSL(suitLength, maxSuitLength, forEach)
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, stdWrap, wrapNW, wrapSE, wrapDlr, Situations,
             makeTopic)


threeClubs :: Situations
threeClubs = let
    sit = let
        action = do
            setOpener T.North
            M.b2N
            M.noInterference
        explanation =
            "Partner has opened " .+ T.Bid 2 T.Notrump .+ ". We've got " .+
            "game-forcing strength, and it's possible we've got a " .+
            "major-suit fit. Initiate Muppet Stayman."
        in situation "3C" action M.b2N3C explanation
  in
    stdWrap sit


threeClubsShortMajors :: Situations
threeClubsShortMajors = let
    sit maybeMajor = let
        action = do
            setOpener T.North
            case maybeMajor of
                Nothing    -> return ()
                Just major -> suitLength major 5
            M.b2N
            M.noInterference
            forEach T.majorSuits (`maxSuitLength`3)
        explanation =
            "Partner has opened " .+ T.Bid 2 T.Notrump .+ ". We've got " .+
            "game-forcing strength, and it's possible we've got a " .+
            "major-suit fit. Initiate Muppet Stayman. Do this even if you " .+
            "only have a 3-card major: we might have a 5" .+ NDash .+ "3 fit!"
        in situation "3Cnm" action M.b2N3C explanation
  in
    -- Make this more memorable: half the time, opener *does* have a 5-card
    -- major! We might not have a fit, but it'll hopefully jog users' memories.
    wrapDlr $ return sit <~ [Nothing, Nothing, Just T.Spades, Just T.Hearts]


fiveHearts :: Situations
fiveHearts = let
    sit = let
        action = do
            setOpener T.South
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", and partner bid " .+
            "Muppet Stayman. We've got a 5-card heart suit, so bid an " .+
            "artificial " .+ M.b2N3C3N .+ ". Partner can pass with no fit, " .+
            "or transfer to hearts with one."
        in situation "5H" action M.b2N3C3N explanation
  in
    stdWrap sit


fiveHeartsFit :: Situations
fiveHeartsFit = let
    sit = let
        action = do
            setOpener T.North
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3N
            M.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", and we bid " .+
            "Muppet Stayman. Partner then showed a 5-card heart suit by " .+
            "bidding an artificial " .+ T.Bid 3 T.Notrump .+ ". We have a " .+
            "heart fit: transfer into it. After the transfer, we can pass " .+
            "or investigate slam if relevant."
        in situation "5HT" action M.b2N3C3N4D explanation
  in
    stdWrap sit


fiveHeartsFitSignoff :: Situations
fiveHeartsFitSignoff = let
    sit = let
        action = do
            setOpener T.South
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3N
            M.noInterference
            M.b2N3C3N4D
            M.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", and partner bid " .+
            "Muppet Stayman. Partner then showed a 5-card heart suit by " .+
            "bidding an artificial " .+ T.Bid 3 T.Notrump .+ ". Partner has " .+
            "now transferred into hearts to show that we have a fit! " .+
            "Complete the transfer. Partner will likely pass, but might " .+
            "investigate slam afterwards."
        in situation "5HSO" action M.b2N3C3N4D4H explanation
  in
    stdWrap sit


fiveHeartsNoFit :: Situations
fiveHeartsNoFit = let
    sit = let
        action = do
            setOpener T.North
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3N
            M.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", and we bid " .+
            "Muppet Stayman. Partner then showed a 5-card heart suit by " .+
            "bidding an artificial " .+ T.Bid 3 T.Notrump .+ ", but we " .+
            "don't have a major-suit fit. Pass."
        in situation "5HP" action M.b2N3C3NP explanation
  in
    stdWrap sit


fiveSpades :: Situations
fiveSpades = let
    sit = let
        action = do
            setOpener T.South
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", and partner bid " .+
            "Muppet Stayman. We've got a 5-card spade suit, so bid it " .+
            "naturally. Partner will place the contract from there."
        in situation "5S" action M.b2N3C3S explanation
  in
    stdWrap sit


fourCardMajor :: Situations
fourCardMajor = let
    sit = let
        action = do
            setOpener T.South
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", and partner bid " .+
            "Muppet Stayman. We don't have a 5-card major, but do have a " .+
            "4-card major. Bid " .+ M.b2N3C3D .+ " to show this, just like " .+
            "in puppet Stayman."
        in situation "4M" action M.b2N3C3D explanation
  in
    stdWrap sit


noMajor :: Situations
noMajor = let
    sit = let
        action = do
            setOpener T.South
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", and partner bid " .+
            "Muppet Stayman. We don't even have a 4-card major: show this " .+
            "by bidding an artificial " .+ M.b2N3C3H .+ ". Partner will " .+
            "likely bid " .+ M.b2N3C3H3S .+ " to right-side a notrump " .+
            "contract, but might bid " .+ M.b2N3C3H3N .+ " to show 5 " .+
            "spades (pass or correct to " .+ T.Bid 4 T.Spades .+ "). They " .+
            "might also make a Texas transfer with a 6-card major."
        in situation "NM" action M.b2N3C3H explanation
  in
    stdWrap sit


texasTransfer :: Situations
texasTransfer = let
    sit bid = let
        action = do
            setOpener T.North
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3N
            M.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", and we bid Muppet " .+
            "Stayman. Partner bid " .+ T.Bid 3 T.Hearts .+ " to show they " .+
            "don't have a 4-card major, but we actually have a 6-card suit. " .+
            "Transfer into our fit. We can then either pass in the correct " .+
            "game or investigate slam, as necessary."
        in situation "TexI" action bid explanation
  in
    -- South will never be a passed hand: if they could have opened, they would
    -- have bid either 1M or 2M. So, West or North must have dealt.
    wrapNW $ return sit <~ [M.b2N3C3H4D, M.b2N3C3H4H]


texasTransferCompleted :: Situations
texasTransferCompleted = let
    sit (start, finish) = let
        action = do
            setOpener T.South
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3H
            M.noInterference
            _ <- start
            M.noInterference
        explanation =
            "After we've shown no majors over Muppet Stayman, partner has " .+
            "transferred into their 6-card suit. Complete the transfer. " .+
            "Partner is likely to pass next, but might investigate slam " .+
            "with extra strength."
        in situation "TexC" action finish explanation
  in
    -- North will never be a passed hand: if they could have opened, they would
    -- have bid either 1M or 2M.
    wrapSE $ return sit <~ [ (M.b2N3C3H4D, M.b2N3C3H4D4H)
                           , (M.b2N3C3H4H, M.b2N3C3H4H4S)
                           ]


fiveCardSpadeRaise :: Situations
fiveCardSpadeRaise = let
    sit = let
        action = do
            setOpener T.North
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3S
            M.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", we bid Muppet " .+
            "Stayman, and partner has shown 5 spades. We've got a fit with " .+
            "them, and no interest in slam. Sign off in game."
        in situation "5SG" action M.b2N3C3S4S explanation
  in
    stdWrap sit


wrongFiveCardMajor :: Situations
wrongFiveCardMajor = let
    sit (major, raise) = let
        action = do
            setOpener T.North
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            _ <- major
            M.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", we bid Muppet " .+
            "Stayman, and partner has shown a 5-card major. Alas, we don't " .+
            "have a fit. Sign off in " .+ T.Bid 3 T.Notrump .+ "."
        in situation "5MN" action raise explanation
  in
    wrapDlr $ return sit <~ [ (M.b2N3C3N, M.b2N3C3NP)
                            , (M.b2N3C3S, M.b2N3C3S3N)
                            ]


fiveSpadesSlam :: Situations
fiveSpadesSlam = let
    sit = let
        action = do
            setOpener T.North
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3S
            M.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", we bid Muppet " .+
            "Stayman, and partner has shown a 5-card spade suit. We've " .+
            "found our fit, and have slam interest. Bid the other major to " .+
            "show this! Partner now knows to investigate slam with us."
        in situation "5SS" action M.b2N3C3S4H explanation
  in
    stdWrap sit


smol :: Situations
smol = let
    sit bid = let
        action = do
            setOpener T.North
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3D
            M.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", we bid Muppet " .+
            "Stayman, and partner has shown at least one 4-card major. " .+
            "We have a 4-card major of our own, and show this by bidding " .+
            "the other major. Partner can retreat to " .+ T.Bid 3 T.Notrump .+
            " if we don't have a fit, or bid our major if we do. This " .+
            "right-sides the contract if we have a fit."
        in situation "Smol" action bid explanation
  in
    wrapDlr $ return sit <~ [M.b2N3C3D3H, M.b2N3C3D3S]


bothMajors :: Situations
bothMajors = let
    sit = let
        action = do
            setOpener T.North
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3D
            M.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", we bid Muppet " .+
            "Stayman, and partner has shown at least one 4-card major. " .+
            "We have both majors, so we definitely have a fit with partner. " .+
            "Bid " .+ T.Bid 4 T.Diamonds .+ " to prompt partner to bid " .+
            "their major. You can then either pass or investigate slam, as " .+
            "relevant."
        in situation "44M" action M.b2N3C3D4D explanation
  in
    stdWrap sit


smolFitH :: Situations
smolFitH = let
    sit = let
        action = do
            setOpener T.South
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3D
            M.noInterference
            M.b2N3C3D3S
            M.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", partner bid Muppet " .+
            "Stayman, and we showed at least one 4-card major. Partner " .+
            "then bid a Smolen-like " .+ T.Bid 3 T.Spades .+ ", showing " .+
            "the other major, hearts. Bid game in our known 8-card fit!" .+
            "Partner will likely pass, but might investigate slam afterwards."
        in situation "SFH" action M.b2N3C3D3S4H explanation
  in
    stdWrap sit


smolFitS :: Situations
smolFitS = let
    sit = let
        action = do
            setOpener T.South
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3D
            M.noInterference
            M.b2N3C3D3H
            M.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", partner bid Muppet " .+
            "Stayman, and we showed at least one 4-card major. Partner " .+
            "then bid a Smolen-like " .+ T.Bid 3 T.Hearts .+ ", showing " .+
            "the other major, spades. Bid naturally to show our 8-card " .+
            "fit! Partner will likely sign off by raising to game, but " .+
            "might investigate slam by control bidding."
        in situation "SFS" action M.b2N3C3D3H3S explanation
  in
    stdWrap sit


smolNoFit :: Situations
smolNoFit = let
    sit (major, signoff) = let
        action = do
            setOpener T.South
            M.b2N
            M.noInterference
            M.b2N3C
            M.noInterference
            M.b2N3C3D
            M.noInterference
            _ <- major
            M.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", partner bid Muppet " .+
            "Stayman, and we showed we had at least one 4-card major. " .+
            "Partner has shown a single 4-card major by making a " .+
            "Smolen-like bid in the other major. We don't have a fit: " .+
            "sign off in " .+ signoff .+ "."
        in situation "SNF" action signoff explanation
  in
    wrapDlr $ return sit <~ [ (M.b2N3C3D3H, M.b2N3C3D3H3N)
                            , (M.b2N3C3D3S, M.b2N3C3D3S3N)
                            ]


-- TODO:
-- Transfer to notrump after no 4-card major
-- 3N PoC after no 4-card major with spades
-- sign off after 3N PoC


topic :: Topic
topic = makeTopic ("Muppet Stayman over " .+ T.Bid 2 T.Notrump) "mup" situations
  where
    situations = wrap [ wrap [threeClubs, threeClubs, threeClubs,
                              threeClubsShortMajors]
                      , wrap [fiveHearts, fiveSpades, fourCardMajor, noMajor]
                      , wrap [texasTransfer, texasTransferCompleted]
                      , wrap [ fiveCardSpadeRaise
                             , wrongFiveCardMajor
                             , fiveSpadesSlam
                             ]
                      , wrap [smol, bothMajors]
                      , wrap [smolFitH, smolFitS, smolNoFit, smolNoFit]
                      , wrap [ fiveHeartsNoFit
                             , fiveHeartsFit
                             , fiveHeartsFitSignoff
                             ]
                      ]
