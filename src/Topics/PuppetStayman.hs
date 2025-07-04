module Topics.PuppetStayman(topic) where

import qualified Bids.PuppetStayman as P
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
            P.b2N
            P.noInterference
        explanation =
            "Partner has opened " .+ T.Bid 2 T.Notrump .+ ". We've got " .+
            "game-forcing strength, and it's possible we've got a " .+
            "major-suit fit. Initiate puppet Stayman."
        in situation "3C" action P.b2N3C explanation
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
            P.b2N
            P.noInterference
            forEach T.majorSuits (`maxSuitLength`3)
        explanation =
            "Partner has opened " .+ T.Bid 2 T.Notrump .+ ". We've got " .+
            "game-forcing strength, and it's possible we've got a " .+
            "major-suit fit. Initiate puppet Stayman. Do this even if you " .+
            "only have a 3-card major: we might have a 5" .+ NDash .+ "3 fit!"
        in situation "3Cnm" action P.b2N3C explanation
  in
    -- Make this more memorable: half the time, opener *does* have a 5-card
    -- major! We might not have a fit, but it'll hopefully jog users' memories.
    wrapDlr $ return sit <~ [Nothing, Nothing, Just T.Spades, Just T.Hearts]


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
            "straight to " .+ P.b2N3C3N .+ ". This usually ends the " .+
            "auction, though partner might still make a Texas transfer if " .+
            "they're 6" .+ NDash .+ "4 in the majors, or jump to " .+
            T.Bid 6 T.Notrump .+ " with extra strength."
        in situation "NM" action P.b2N3C3N explanation
  in
    stdWrap sit


texasTransfer :: Situations
texasTransfer = let
    sit bid = let
        action = do
            setOpener T.North
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
            P.b2N3C3N
            P.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", and we bid puppet " .+
            "Stayman. Partner tried to sign off in " .+ T.Bid 3 T.Notrump .+
            " because they don't have a 4-card major, but we're actually " .+
            "6" .+ NDash .+ "4 in the majors. Transfer into our fit. We can " .+
            "then either pass in the correct game or investigate slam, as " .+
            "necessary."
        in situation "TexI" action bid explanation
  in
    -- South will never be a passed hand: if they could have opened, they woud
    -- have bid either 1M or 2M. So, West or North must have dealt.
    wrapNW $ return sit <~ [P.b2N3C3N4D, P.b2N3C3N4H]


texasTransferCompleted :: Situations
texasTransferCompleted = let
    sit (start, finish) = let
        action = do
            setOpener T.South
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
            P.b2N3C3N
            P.noInterference
            _ <- start
            P.noInterference
        explanation =
            "After we've shown no majors over puppet Stayman, partner has " .+
            "transferred into their 6-card suit. Complete the transfer. " .+
            "Partner is likely to pass next, but might investigate slam " .+
            "with extra strength."
        in situation "TexC" action finish explanation
  in
    -- North will never be a passed hand: if they could have opened, they woud
    -- have bid either 1M or 2M.
    wrapSE $ return sit <~ [ (P.b2N3C3N4D, P.b2N3C3N4D4H)
                           , (P.b2N3C3N4H, P.b2N3C3N4H4S)
                           ]


fiveCardMajorRaise :: Situations
fiveCardMajorRaise = let
    sit (major, raise) = let
        action = do
            setOpener T.North
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
            _ <- major
            P.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", we bid puppet " .+
            "Stayman, and partner has shown a 5-card major. We've got a " .+
            "fit with them, and no interest in slam. Sign off in game."
        in situation "5MG" action raise explanation
  in
    wrapDlr $ return sit <~ [ (P.b2N3C3H, P.b2N3C3H4H)
                            , (P.b2N3C3S, P.b2N3C3S4S)
                            ]


wrongFiveCardMajor :: Situations
wrongFiveCardMajor = let
    sit (major, raise) = let
        action = do
            setOpener T.North
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
            _ <- major
            P.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", we bid puppet " .+
            "Stayman, and partner has shown a 5-card major. Alas, we don't " .+
            "have a fit. Sign off in " .+ T.Bid 3 T.Notrump .+ "."
        in situation "5MN" action raise explanation
  in
    wrapDlr $ return sit <~ [ (P.b2N3C3H, P.b2N3C3H3N)
                            , (P.b2N3C3S, P.b2N3C3S3N)
                            ]


fiveCardMajorSlam :: Situations
fiveCardMajorSlam = let
    sit (major, raise) = let
        action = do
            setOpener T.North
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
            _ <- major
            P.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", we bid puppet " .+
            "Stayman, and partner has shown a 5-card major. We've found " .+
            "our fit, and have slam interest. Bid the other major to show " .+
            "this! Partner now knows to investigate slam with us."
        in situation "5MS" action raise explanation
  in
    wrapDlr $ return sit <~ [ (P.b2N3C3H, P.b2N3C3H3S)
                            , (P.b2N3C3S, P.b2N3C3S4H)
                            ]


smol :: Situations
smol = let
    sit bid = let
        action = do
            setOpener T.North
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
            P.b2N3C3D
            P.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", we bid puppet " .+
            "Stayman, and partner has shown at least one 4-card major. " .+
            "We have a 4-card major of our own, and show this by bidding " .+
            "the other major. Partner can retreat to " .+ T.Bid 3 T.Notrump .+
            " if we don't have a fit, or bid our major if we do. This " .+
            "right-sides the contract if we have a fit."
        in situation "Smol" action bid explanation
  in
    wrapDlr $ return sit <~ [P.b2N3C3D3H, P.b2N3C3D3S]


bothMajors :: Situations
bothMajors = let
    sit = let
        action = do
            setOpener T.North
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
            P.b2N3C3D
            P.noInterference
        explanation =
            "Partner opened " .+ T.Bid 2 T.Notrump .+ ", we bid puppet " .+
            "Stayman, and partner has shown at least one 4-card major. " .+
            "We have both majors, so definitely have a fit with partner. " .+
            "Bid " .+ T.Bid 4 T.Diamonds .+ " to prompt partner to bid " .+
            "their major. You can then either pass or investigate slam, as " .+
            "relevant."
        in situation "44M" action P.b2N3C3D4D explanation
  in
    stdWrap sit


smolFitH :: Situations
smolFitH = let
    sit = let
        action = do
            setOpener T.South
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
            P.b2N3C3D
            P.noInterference
            P.b2N3C3D3S
            P.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", partner bid puppet " .+
            "Stayman, and we showed at least one 4-card major. Partner " .+
            "then bid a Smolen-like " .+ T.Bid 3 T.Spades .+ ", showing " .+
            "the other major, hearts. Bid game in our known 8-card fit!" .+
            "Partner will likely pass, but might investigate slam afterwards."
        in situation "SFH" action P.b2N3C3D3S4H explanation
  in
    stdWrap sit


smolFitS :: Situations
smolFitS = let
    sit = let
        action = do
            setOpener T.South
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
            P.b2N3C3D
            P.noInterference
            P.b2N3C3D3H
            P.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", partner bid puppet " .+
            "Stayman, and we showed at least one 4-card major. Partner " .+
            "then bid a Smolen-like " .+ T.Bid 3 T.Hearts .+ ", showing " .+
            "the other major, spades. Bid naturally to show our 8-card " .+
            "fit! Partner will likely sign off by raising to game, but " .+
            "might investigate slam by control bidding."
        in situation "SFS" action P.b2N3C3D3H3S explanation
  in
    stdWrap sit


smolNoFit :: Situations
smolNoFit = let
    sit (major, signoff) = let
        action = do
            setOpener T.South
            P.b2N
            P.noInterference
            P.b2N3C
            P.noInterference
            P.b2N3C3D
            P.noInterference
            _ <- major
            P.noInterference
        explanation =
            "We opened " .+ T.Bid 2 T.Notrump .+ ", partner bid puppet " .+
            "Stayman, and we showed we had at least one 4-card major. " .+
            "Partner has shown a single 4-card major by making a " .+
            "Smolen-like bid in the other major. We don't have a fit: " .+
            "sign off in " .+ signoff .+ "."
        in situation "SNF" action signoff explanation
  in
    wrapDlr $ return sit <~ [ (P.b2N3C3D3H, P.b2N3C3D3H3N)
                            , (P.b2N3C3D3S, P.b2N3C3D3S3N)
                            ]


topic :: Topic
topic = makeTopic ("puppet Stayman over " .+ T.Bid 2 T.Notrump) "pup" situations
  where
    situations = wrap [ wrap [threeClubs, threeClubs, threeClubs,
                              threeClubsShortMajors]
                      , wrap [fiveCardMajor, fourCardMajor, noMajor]
                      , wrap [texasTransfer, texasTransferCompleted]
                      , wrap [ fiveCardMajorRaise
                             , wrongFiveCardMajor
                             , fiveCardMajorSlam
                             ]
                      , wrap [smol, bothMajors]
                      , wrap [smolFitH, smolFitS, smolNoFit]
                      ]
