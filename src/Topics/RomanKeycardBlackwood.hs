module Topics.RomanKeycardBlackwood(
    topic1430
  , topic3014
) where

import Control.Monad(join)
import Data.List(sort)

import Action(Action, withholdBid)
import qualified Bids.Jacoby2NT as J2N
import qualified Bids.OneNotrump as NT
import qualified Bids.RomanKeycardBlackwood as RKC
import CommonBids(andNextBidderIs, noInterference)
import qualified EDSL as E
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapWeighted, wrapNW, Situations, makeTopic)


takeIndices_ :: [Int] -> [a] -> [a]
takeIndices_ indices values = takeIndices' values (sort indices)
  where
    takeIndices' _ [] = []
    takeIndices' vals (a:as) = (vals !! a) : rest
      where
        rest = takeIndices' (drop (a + 1) vals) (map (subtract (a + 1)) as)


-- Reminder: keep track of the indices in these lists! In the Situations where
-- a player shows a void, we need to filter out only the auctions in which that
-- player could possibly have a void.

setUpAuctionsH :: [Action]  -- The next bid should be RKC for H
setUpAuctionsH = [ do J2N.b1H  -- Index 0
                      noInterference T.Hearts
                      E.suitLength T.Hearts 4  -- Speed up performance
                      J2N.b1H2N
                      noInterference T.Hearts
                      -- NOTE: We can't use this setup for times when we have a
                      -- 10-card fit and want to pretend we have the queen. Redo
                      -- this auction separately but set the heart length to 6
                      -- for that.
                      E.suitLength T.Hearts 5  -- Speed up performance
                      J2N.b1H2N4H
                      E.makePass
                      E.alternatives [E.pointRange 17 40, E.maxLoserCount 5]
                 , do J2N.b1H  -- Index 1
                      noInterference T.Hearts
                      E.suitLength T.Hearts 4  -- Speed up performance
                      J2N.b1H2N
                      noInterference T.Hearts
                      E.suitLength T.Hearts 5  -- Speed up performance
                      J2N.b1H2N4D
                      E.makePass
                      E.alternatives [E.pointRange 16 40, E.maxLoserCount 5]
                 , do NT.b1N  -- Index 2
                      NT.noInterference
                      NT.b1N4D
                      E.makePass
                      NT.b1N4D4H
                      E.makePass
                      NT.slamInterest
                      E.forbid $ E.hasControl T.Spades
                 , do NT.b1N  -- Index 3
                      NT.noInterference
                      E.suitLength T.Hearts 6  -- Speed up performance
                      NT.b1N2D
                      NT.noInterference
                      E.suitLength T.Hearts 3  -- Speed up performance
                      NT.b1N2D2H
                      E.makePass
                      NT.b1N2D2H4H
                      E.makePass
                      E.forbid $ E.hasControl T.Spades
                      E.pointRange 17 40
                 ]

setUpAuctionsS :: [Action]  -- The next bid should be RKC for S
setUpAuctionsS = [ do J2N.b1S  -- Index 0
                      noInterference T.Spades
                      E.suitLength T.Spades 4  -- Speed up performance
                      J2N.b1S2N
                      noInterference T.Spades
                      -- NOTE: We can't use this setup for times when we have a
                      -- 10-card fit and want to pretend we have the queen. Redo
                      -- this auction separately but set the spade length to 6
                      -- for that.
                      E.suitLength T.Spades 5  -- Speed up performance
                      J2N.b1S2N4S
                      E.makePass
                      E.alternatives [E.pointRange 17 40, E.maxLoserCount 5]
                 , do J2N.b1S  -- Index 1
                      noInterference T.Spades
                      E.suitLength T.Spades 4  -- Speed up performance
                      J2N.b1S2N
                      noInterference T.Spades
                      E.suitLength T.Spades 5  -- Speed up performance
                      J2N.b1S2N4H
                      E.makePass
                      E.alternatives [E.pointRange 16 40, E.maxLoserCount 5]
                 , do NT.b1N  -- Index 2
                      NT.noInterference
                      NT.b1N4H
                      E.makePass
                      NT.b1N4H4S
                      E.makePass
                      NT.slamInterest
                 , do NT.b1N  -- Index 3
                      NT.noInterference
                      E.suitLength T.Spades 6
                      NT.b1N2H
                      NT.noInterference
                      E.suitLength T.Spades 3
                      NT.b1N2H2S
                      E.makePass
                      NT.b1N2H2S4S
                      E.makePass
                      E.pointRange 17 40
                 ]

-- Auctions where the next bid should be RKC but the keycard teller should
-- definitely not lie and pretend they have the queen if they don't.
-- Note that index 1 could work also, but empirically that only has no queen
-- about once every 7 million deals, so skip it for performance reasons.
setUpAuctionsHNoQ :: [Action]
setUpAuctionsHNoQ = takeIndices_ [0] setUpAuctionsH

setUpAuctionsSNoQ :: [Action]
setUpAuctionsSNoQ = takeIndices_ [0] setUpAuctionsS


initiate :: Situations
initiate = let
    sit setup = let
        action = do
            setup `andNextBidderIs` T.South
        explanation =
            "We've found a trump fit and have slam interest. Time to check " .+
            "for keycards by bidding " .+ RKC.b4N .+ "! If we're missing " .+
            "two of them, we'll sign off at the 5 level."
      in situation "init" action RKC.b4N explanation
  in
    wrapNW $ return sit <~ (setUpAuctionsH ++ setUpAuctionsS)


firstResponse1430, firstResponse3014 :: Situations
(firstResponse1430, firstResponse3014) =
    (firstResponse1430', firstResponse3014')
  where
    firstResponse (setups, responses) = let
        inner setup response = let
            action = do
                setup `andNextBidderIs` T.North
                RKC.b4N
                E.makePass
            explanation =
                "Partner has bid " .+ RKC.b4N .+ " to ask how many " .+
                "keycards we have. Give them the answer."
          in situation "resp" action response explanation
      in return inner <~ setups <~ responses
    firstResponse1430' = wrapNW . join $ return firstResponse
        <~ [ (setUpAuctionsH, [ RKC.b1430H5C
                              , RKC.b1430H5D
                              , RKC.bH5H
                              , RKC.bH5S
                              ])
           , (setUpAuctionsS, [ RKC.b1430S5C
                              , RKC.b1430S5D
                              , RKC.bS5H
                              , RKC.bS5S
                              ])
           ]
    firstResponse3014' = wrapNW . join $ return firstResponse
        <~ [ (setUpAuctionsH, [ RKC.b3014H5C
                              , RKC.b3014H5D
                              , RKC.bH5H
                              , RKC.bH5S
                              ])
           , (setUpAuctionsS, [ RKC.b3014S5C
                              , RKC.b3014S5D
                              , RKC.bS5H
                              , RKC.bS5S
                              ])
           ]


signoffPartscore1430, signoffPartscore3014 :: Situations
(signoffPartscore1430, signoffPartscore3014) = (signoff1430', signoff3014')
  where
    signoff (setups, followups) = let
        inner setup (response, signoffBid) = let
            action = do
                setup `andNextBidderIs` T.South
                RKC.b4N
                E.makePass
                _ <- response
                E.makePass
            explanation =
                "We asked for keycards, but learned we're missing 2 of " .+
                "them. Slam is likely to fail: sign off at the 5 level."
          in situation "SO5" action signoffBid explanation
      in return inner <~ setups <~ followups
    signoff1430' = wrapNW . join $ return signoff
        -- Empirically, most of the setups are very unlikely to only have 3
        -- keycards. The only one that seems semi-common is 1M-2N-4M-4N, so
        -- focus on that.
        <~ [ (takeIndices_ [0] setUpAuctionsH,
             [ (RKC.b1430H5C, RKC.b1430H5C5H)
             , (RKC.b1430H5D, RKC.b1430H5D5H)
             , (RKC.bH5H,     RKC.bH5HP)
             -- If hearts are trump and partner bid 5S, we can't sign off.
             -- Handle this separately.
             --, (RKC.bH5S,     trouble)
             ])
           , (takeIndices_ [0] setUpAuctionsS,
              [ (RKC.b1430S5C, RKC.b1430S5C5S)
              , (RKC.b1430S5D, RKC.b1430S5D5S)
              , (RKC.bS5H,     RKC.bS5H5S)
              , (RKC.bS5S,     RKC.bS5SP)
              ])
           ]
    signoff3014' = wrapNW . join $ return signoff
        <~ [ (takeIndices_ [0] setUpAuctionsH,
              [ (RKC.b3014H5C, RKC.b3014H5C5H)
              , (RKC.b3014H5D, RKC.b3014H5D5H)
              , (RKC.bH5H,     RKC.bH5HP)
              -- If hearts are trump and partner bid 5S, we can't sign off.
              -- Handle this separately.
              --, (RKC.bH5S,     trouble)
              ])
           , (takeIndices_ [0] setUpAuctionsS,
              [ (RKC.b3014S5C, RKC.b3014S5C5S)
              , (RKC.b3014S5D, RKC.b3014S5D5S)
              , (RKC.bS5H,     RKC.bS5H5S)
              , (RKC.bS5S,     RKC.bS5SP)
              ])
           ]


oddVoid :: Situations
oddVoid = let
    sit (setups, responses) = let
        inner setup response = let
            action = do
                setup `andNextBidderIs` T.North
                RKC.b4N
                E.makePass
            explanation =
                "Partner initiated a keycard ask. We have an odd number of " .+
                "keycards and a void, so bid the void suit at the 6 level. " .+
                "If the void is in a suit higher than the trump suit, just " .+
                "bid 6 of the trump suit, and partner can infer where your " .+
                "void is. After this, it is up to partner to place the " .+
                "final contract, either in our trump suit or notrump, " .+
                "either in small or grand slam."
          in situation "oddV" action response explanation
      in return inner <~ setups <~ responses
  in
    wrapNW . join $ return sit
        <~ [ -- The keycard teller has already shown a natural diamond suit:
             -- can't have a void in diamonds
             (takeIndices_ [1] setUpAuctionsH, [RKC.bH6C, RKC.bH6H])
             -- The keycard teller has already shown a natural heart suit: can't
             -- have a void in hearts
           , (takeIndices_ [1] setUpAuctionsS, [RKC.bS6C, RKC.bS6D])
           ]


evenVoid :: Situations
evenVoid = let
    sit (setups, response) = let
        inner setup = let
            action = do
                setup `andNextBidderIs` T.North
                RKC.b4N
                E.makePass
            explanation =
                "Partner initiated a keycard ask. We have an even number of " .+
                "keycards and a void, so bid " .+ response .+ " to show " .+
                "this. Afterwards, it is up to partner to infer where your " .+
                "void might be (which can sometimes be a guess) and place " .+
                "the final contract, either in our trump suit or notrump, " .+
                "either in small or grand slam."
          in situation "evenV" action response explanation
      in return inner <~ setups
  in
    wrapNW . join $ return sit <~ [ (takeIndices_ [1] setUpAuctionsH, RKC.bH5N)
                                  , (takeIndices_ [1] setUpAuctionsS, RKC.bS5N)
                                  ]


queenAsk1430, queenAsk3014 :: Situations
(queenAsk1430, queenAsk3014) = (queenAsk1430', queenAsk3014')
  where
    queenAsk (setups, response, askingBid, awkward) = let
        inner setup = let
            action = do
                setup `andNextBidderIs` T.South
                RKC.b4N
                E.makePass
                _ <- response
                E.makePass
            explanation =
                "We are missing at most 1 keycard, but don't yet know " .+
                "whether we also have the queen of trump. Make the cheapest " .+
                "non-signoff bid to ask about the queen. " .+ (
                    if awkward
                    then "(If hearts are trump and the cheapest relay is " .+
                         T.Bid 5 T.Spades .+ ", asking about the queen " .+
                         "pushes to slam no matter what. Make sure you're " .+
                         "okay with that no matter what partner does.)"
                    else "" .+ "")  -- Use (.+) to get the types consistent
          in situation "qask" action askingBid explanation
      in return inner <~ setups
    -- Performance improvement: auctions starting 1N-2D-2H-4H-4N only generate
    -- queen-asks at a rate around 1 in 3 million boards, so sometimes time out
    -- after 10 million boards without finding one. Skip those.
    -- Also, auctions starting 1H-2N-4D-4N occur about one in 4 million times.
    -- So, skip index 1 also.
    setupsH = takeIndices_ [0, 2] setUpAuctionsH
    setupsS = takeIndices_ [0, 2] setUpAuctionsS
    queenAsk1430' = wrapNW . join $ return queenAsk
        <~ [ (setupsH, RKC.b1430H5C, RKC.b1430H5C5D, False)
           , (setupsH, RKC.b1430H5D, RKC.b1430H5D5S, True)
           , (setupsS, RKC.b1430S5C, RKC.b1430S5C5D, False)
           , (setupsS, RKC.b1430S5D, RKC.b1430S5D5H, False)
           ]
    queenAsk3014' = wrapNW . join $ return queenAsk
        <~ [ (setupsH, RKC.b3014H5C, RKC.b3014H5C5D, False)
           , (setupsH, RKC.b3014H5D, RKC.b3014H5D5S, True)
           , (setupsS, RKC.b3014S5C, RKC.b3014S5C5D, False)
           , (setupsS, RKC.b3014S5D, RKC.b3014S5D5H, False)
           ]


noQueen1430, noQueen3014 :: Situations
(noQueen1430, noQueen3014) = (noQueen1430', noQueen3014')
  where
    noQueen (setups, followups) = let
        inner setup (response, queenAsk, signoff) = let
            action = do
                setup `andNextBidderIs` T.North
                RKC.b4N
                E.makePass
                _ <- response
                E.makePass
                _ <- queenAsk
                E.makePass
            explanation =
                "Partner has made a queen ask, but we don't have it. Sign " .+
                "off in our trump suit as cheaply as possible to show this. " .+
                "Partner will likely pass, but could correct to other " .+
                "contracts in rare situations."
          in situation "noQ" action signoff explanation
      in return inner <~ setups <~ followups
    noQueen1430' = wrapNW . join $ return noQueen
        <~ [ (setUpAuctionsHNoQ, [ (RKC.b1430H5C, RKC.b1430H5C5D, RKC.bH5C5D5H)
                                 , (RKC.b1430H5D, RKC.b1430H5D5S, RKC.bH5D5S6H)
                                 ])
           , (setUpAuctionsSNoQ, [ (RKC.b1430S5C, RKC.b1430S5C5D, RKC.bS5C5D5S)
                                 , (RKC.b1430S5D, RKC.b1430S5D5H, RKC.bS5D5H5S)
                                 ])
           ]
    noQueen3014' = wrapNW . join $ return noQueen
        <~ [ (setUpAuctionsHNoQ, [ (RKC.b3014H5C, RKC.b3014H5C5D, RKC.bH5C5D5H)
                                 , (RKC.b3014H5D, RKC.b3014H5D5S, RKC.bH5D5S6H)
                                 ])
           , (setUpAuctionsSNoQ, [ (RKC.b3014S5C, RKC.b3014S5C5D, RKC.bS5C5D5S)
                                 , (RKC.b3014S5D, RKC.b3014S5D5H, RKC.bS5D5H5S)
                                 ])
           ]


queenNoKing1430, queenNoKing3014 :: Situations
(queenNoKing1430, queenNoKing3014) = (queenNoKing1430', queenNoKing3014')
  where
    queenNoKing (setups, followups) = let
        inner setup (response, queenAsk, signoff) = let
            action = do
                setup `andNextBidderIs` T.North
                RKC.b4N
                E.makePass
                _ <- response
                E.makePass
                _ <- queenAsk
                E.makePass
            explanation =
                "Partner has made a queen ask. We have the queen but no " .+
                "side-suit king. Sign off in small slam. Partner will " .+
                "likely pass, but could correct to " .+
                T.Bid 6 T.Notrump .+ " in rare situations, especially at " .+
                "matchpoint scoring. (This practice system is not very " .+
                "nuanced. Sometimes, you'll prefer to bid " .+
                T.Bid 5 T.Notrump .+ " to show the queen of trump, no " .+
                "side-suit king, but unexpected extra strength. That is " .+
                "too subtle to easily program, so we have skipped it. If " .+
                "you wanted to bid " .+ T.Bid 5 T.Notrump .+ " here instead " .+
                "of " .+ signoff .+ ", that might be the right choice.)"
          in situation "QnoK" action signoff explanation
      in return inner <~ setups <~ followups
    -- Performance improvement: empirically, an auction starting 1N-4D-4H-4N,
    -- the 1N bidder nearly always has a side-suit king. So, skip those setups
    -- (index 2). Same with auctions starting 1N-2D-2H-4H, and auctions starting
    -- 1H-2N-4D
    setupsH = takeIndices_ [0] setUpAuctionsH
    setupsS = takeIndices_ [0] setUpAuctionsS
    queenNoKing1430' = wrapNW . join $ return queenNoKing
        <~ [ (setupsH, [ (RKC.b1430H5C, RKC.b1430H5C5D, RKC.bH5C5D6H)
                       -- Have separate commentary for this one
                       --, (RKC.b1430H5D, RKC.b1430H5D5S, RKC.bH5D5S5N)
                       ])
           , (setupsS, [ (RKC.b1430S5C, RKC.b1430S5C5D, RKC.bS5C5D6S)
                       , (RKC.b1430S5D, RKC.b1430S5D5H, RKC.bS5D5H6S)
                       ])
           ]
    queenNoKing3014' = wrapNW . join $ return queenNoKing
        <~ [ (setupsH, [ (RKC.b3014H5C, RKC.b3014H5C5D, RKC.bH5C5D6H)
                       -- Have separate commentary for this one
                       --, (RKC.b3014H5D, RKC.b3014H5D5S, RKC.bH5D5S5N)
                       ])
           , (setupsS, [ (RKC.b3014S5C, RKC.b3014S5C5D, RKC.bS5C5D6S)
                       , (RKC.b3014S5D, RKC.b3014S5D5H, RKC.bS5D5H6S)
                       ])
           ]


queenNoKing5N1430, queenNoKing5N3014 :: Situations
(queenNoKing5N1430, queenNoKing5N3014) = (queenNoKing1430', queenNoKing3014')
  where
    queenNoKing response queenAsk setup = let
        action = do
            setup `andNextBidderIs` T.North
            RKC.b4N
            E.makePass
            _ <- response
            E.makePass
            _ <- queenAsk
            E.makePass
        explanation =
            "Partner has made a queen ask, but it is higher than 5 " .+
            "of our trump suit. This means that denying the queen (by " .+
            "bidding our trump suit as cheaply as possible) is the same " .+
            "as jumping to small slam. Consequently, the bid to show " .+
            "the queen but no showable (minor-suit) king is to bid " .+
            RKC.bH5D5S5N .+ " instead."
      in situation "QnoK5S" action RKC.bH5D5S5N explanation
    -- Performance improvement: empirically, an auction starting 1N-4D-4H-4N,
    -- the 1N bidder nearly always has a side-suit king. So, skip those setups
    -- (index 2). Same with auctions starting 1N-2D-2H-4H. Auctions starting
    -- 1H-2N-4D-4N are also very rare, about one in every 8 million deals.
    setupsH = takeIndices_ [0] setUpAuctionsH
    queenNoKing1430' = wrapNW $
        (return $ queenNoKing RKC.b1430H5D RKC.b1430H5D5S) <~ setupsH
    queenNoKing3014' = wrapNW $
        (return $ queenNoKing RKC.b3014H5D RKC.b3014H5D5S) <~ setupsH


queenKing1430, queenKing3014 :: Situations
(queenKing1430, queenKing3014) = (queenKing1430', queenKing3014')
  where
    queenKing (setups, middle, followups) = let
        inner setup answer = let
            action = do
                setup `andNextBidderIs` T.North
                RKC.b4N
                E.makePass
                middle
            explanation =
                "Partner has made a queen ask. We have the queen and at " .+
                "least one side-suit king. Bid our cheapest side-suit king " .+
                "to show that one and deny any kings in cheaper suits. " .+
                "Partner will place the final contract from here."
          in situation "QK" action answer explanation
      in return inner <~ setups <~ followups
    -- Performance improvement: auctions starting 1N-2D-2H-4H are very unlikely
    -- to show up here. So, skip index 3. Also 1H-2N-4D-4N is about a 1-in-5M
    -- chance, so skip index 1.
    setupsH = takeIndices_ [0, 2] setUpAuctionsH
    setupsS = takeIndices_ [0, 2] setUpAuctionsS
    queenKing1430' = wrapNW . join $ return queenKing
        <~ [ (setupsH,
              do RKC.b1430H5C
                 E.makePass
                 RKC.b1430H5C5D
                 E.makePass,
              [RKC.bH5C5D5S, RKC.bH5C5D6C, RKC.bH5C5D6D])
           , (setupsH,
              do RKC.b1430H5D
                 E.makePass
                 RKC.b1430H5D5S
                 E.makePass,
              [RKC.bH5D5S6C, RKC.bH5D5S6D])
           , (setupsS,
              do RKC.b1430S5C
                 E.makePass
                 RKC.b1430S5C5D
                 E.makePass,
              [RKC.bS5C5D5H, RKC.bS5C5D6C, RKC.bS5C5D6D])
           , (setupsS,
              do RKC.b1430S5D
                 E.makePass
                 RKC.b1430S5D5H
                 E.makePass,
              [RKC.bS5D5H6C, RKC.bS5D5H6D, RKC.bS5D5H6H])
           ]
    queenKing3014' = wrapNW . join $ return queenKing
        <~ [ (setupsH,
              do RKC.b3014H5C
                 E.makePass
                 RKC.b3014H5C5D
                 E.makePass,
              [RKC.bH5C5D5S, RKC.bH5C5D6C, RKC.bH5C5D6D])
           , (setupsH,
              do RKC.b3014H5D
                 E.makePass
                 RKC.b3014H5D5S
                 E.makePass,
              [RKC.bH5D5S6C, RKC.bH5D5S6D])
           , (setupsS,
              do RKC.b3014S5C
                 E.makePass
                 RKC.b3014S5C5D
                 E.makePass,
              [RKC.bS5C5D5H, RKC.bS5C5D6C, RKC.bS5C5D6D])
           , (setupsS,
              do RKC.b3014S5D
                 E.makePass
                 RKC.b3014S5D5H
                 E.makePass,
              [RKC.bS5D5H6C, RKC.bS5D5H6D, RKC.bS5D5H6H])
           ]


slamSignoff1430, slamSignoff3014 :: Situations
(slamSignoff1430, slamSignoff3014) = (signoff1430, signoff3014)
  where
    signoff (setups, payoffs) = let
        inner setup (middle, signoffBid) = let
            action = do
                setup `andNextBidderIs` T.South
                RKC.b4N
                E.makePass
                _ <- middle
                E.makePass
            explanation =
                "Of the 6 cards we care about (the 5 keycards and the " .+
                "queen of trump), we're missing exactly 1. We're likely " .+
                "to make small slam but not grand slam. Time to sign off! " .+
                "(This system is not very nuanced. It's possible, " .+
                "especially at matchpoints, that signing off in " .+
                T.Bid 6 T.Notrump .+ " is the right choice instead.)"
          in situation "slamSO" action signoffBid explanation
      in return inner <~ setups <~ payoffs
    -- Per earlier situations, only index 0 is very efficient to generate no
    -- matter what.
    setupsH = takeIndices_ [0] setUpAuctionsH
    setupsS = takeIndices_ [0] setUpAuctionsS
    signoff1430 = wrapNW . join $ return signoff
        <~ [ (setupsH, [ ( RKC.b1430H5C
                         , RKC.b1430H5C6H
                         )
                       , ( RKC.b1430H5D
                         , RKC.b1430H5D6H
                         )
                       , ( do RKC.b1430H5C
                              E.makePass
                              RKC.b1430H5C5D
                              E.makePass
                              RKC.bH5C5D5H
                         , RKC.b1430H5C5D5H6H
                         )
                       , ( do RKC.b1430H5C
                              E.makePass
                              RKC.b1430H5C5D
                              E.makePass
                              RKC.bH5C5D5S
                         , RKC.b1430H5C5D5S6H
                         )
                       , ( do RKC.b1430H5C
                              E.makePass
                              RKC.b1430H5C5D
                              E.makePass
                              RKC.bH5C5D6C
                         , RKC.b1430H5C5D6C6H
                         )
                       , ( do RKC.b1430H5C
                              E.makePass
                              RKC.b1430H5C5D
                              E.makePass
                              RKC.bH5C5D6D
                         , RKC.b1430H5C5D6D6H
                         )
                       , ( RKC.bH5H
                         , RKC.bH5H6H
                         )
                       , ( RKC.bH5S
                         , RKC.bH5S6H
                         )
                       ])
           , (setupsS, [ ( RKC.b1430S5C
                         , RKC.b1430S5C6S
                         )
                       , ( RKC.b1430S5D
                         , RKC.b1430S5D6S
                         )
                       , ( do RKC.b1430S5C
                              E.makePass
                              RKC.b1430S5C5D
                              E.makePass
                              RKC.bS5C5D5H
                         , RKC.b1430S5C5D5H6S
                         )
                       , ( do RKC.b1430S5C
                              E.makePass
                              RKC.b1430S5C5D
                              E.makePass
                              RKC.bS5C5D5S
                         , RKC.b1430S5C5D5S6S
                         )
                       , ( do RKC.b1430S5C
                              E.makePass
                              RKC.b1430S5C5D
                              E.makePass
                              RKC.bS5C5D6C
                         , RKC.b1430S5C5D6C6S
                         )
                       , ( do RKC.b1430S5C
                              E.makePass
                              RKC.b1430S5C5D
                              E.makePass
                              RKC.bS5C5D6D
                         , RKC.b1430S5C5D6D6S
                         )
                       , ( do RKC.b1430S5D
                              E.makePass
                              RKC.b1430S5D5H
                              E.makePass
                              RKC.bS5D5H5S
                         , RKC.b1430S5D5H5S6S
                         )
                       , ( do RKC.b1430S5D
                              E.makePass
                              RKC.b1430S5D5H
                              E.makePass
                              RKC.bS5D5H6C
                         , RKC.b1430S5D5H6C6S
                         )
                       , ( do RKC.b1430S5D
                              E.makePass
                              RKC.b1430S5D5H
                              E.makePass
                              RKC.bS5D5H6D
                         , RKC.b1430S5D5H6D6S
                         )
                       , ( do RKC.b1430S5D
                              E.makePass
                              RKC.b1430S5D5H
                              E.makePass
                              RKC.bS5D5H6H
                         , RKC.b1430S5D5H6H6S
                         )
                       , ( RKC.bS5H
                         , RKC.bS5H6S
                         )
                       , ( RKC.bS5S
                         , RKC.bS5S6S
                         )
                       ])
           ]
    signoff3014 = wrapNW . join $ return signoff
        <~ [ (setupsH, [ ( RKC.b3014H5C
                         , RKC.b3014H5C6H
                         )
                       , ( RKC.b3014H5D
                         , RKC.b3014H5D6H
                         )
                       , ( do RKC.b3014H5C
                              E.makePass
                              RKC.b3014H5C5D
                              E.makePass
                              RKC.bH5C5D5H
                         , RKC.b3014H5C5D5H6H
                         )
                       , ( do RKC.b3014H5C
                              E.makePass
                              RKC.b3014H5C5D
                              E.makePass
                              RKC.bH5C5D5S
                         , RKC.b3014H5C5D5S6H
                         )
                       , ( do RKC.b3014H5C
                              E.makePass
                              RKC.b3014H5C5D
                              E.makePass
                              RKC.bH5C5D6C
                         , RKC.b3014H5C5D6C6H
                         )
                       , ( do RKC.b3014H5C
                              E.makePass
                              RKC.b3014H5C5D
                              E.makePass
                              RKC.bH5C5D6D
                         , RKC.b3014H5C5D6D6H
                         )
                       , ( RKC.bH5H
                         , RKC.bH5H6H
                         )
                       , ( RKC.bH5S
                         , RKC.bH5S6H
                         )
                       ])
           , (setupsS, [ ( RKC.b3014S5C
                         , RKC.b3014S5C6S
                         )
                       , ( RKC.b3014S5D
                         , RKC.b3014S5D6S
                         )
                       , ( do RKC.b3014S5C
                              E.makePass
                              RKC.b3014S5C5D
                              E.makePass
                              RKC.bS5C5D5H
                         , RKC.b3014S5C5D5H6S
                         )
                       , ( do RKC.b3014S5C
                              E.makePass
                              RKC.b3014S5C5D
                              E.makePass
                              RKC.bS5C5D5S
                         , RKC.b3014S5C5D5S6S
                         )
                       , ( do RKC.b3014S5C
                              E.makePass
                              RKC.b3014S5C5D
                              E.makePass
                              RKC.bS5C5D6C
                         , RKC.b3014S5C5D6C6S
                         )
                       , ( do RKC.b3014S5C
                              E.makePass
                              RKC.b3014S5C5D
                              E.makePass
                              RKC.bS5C5D6D
                         , RKC.b3014S5C5D6D6S
                         )
                       , ( do RKC.b3014S5D
                              E.makePass
                              RKC.b3014S5D5H
                              E.makePass
                              RKC.bS5D5H5S
                         , RKC.b3014S5D5H5S6S
                         )
                       , ( do RKC.b3014S5D
                              E.makePass
                              RKC.b3014S5D5H
                              E.makePass
                              RKC.bS5D5H6C
                         , RKC.b3014S5D5H6C6S
                         )
                       , ( do RKC.b3014S5D
                              E.makePass
                              RKC.b3014S5D5H
                              E.makePass
                              RKC.bS5D5H6D
                         , RKC.b3014S5D5H6D6S
                         )
                       , ( do RKC.b3014S5D
                              E.makePass
                              RKC.b3014S5D5H
                              E.makePass
                              RKC.bS5D5H6H
                         , RKC.b3014S5D5H6H6S
                         )
                       , ( RKC.bS5H
                         , RKC.bS5H6S
                         )
                       , ( RKC.bS5S
                         , RKC.bS5S6S
                         )
                       ])
           ]


kingAsk :: Situations
kingAsk = let
    sit (response, kingAskBid) = let
        action = do
            -- Start 1S-2N-4H
            setUpAuctionsS !! 1 `andNextBidderIs` T.South
            RKC.b4N
            E.makePass
            E.suitLength T.Spades 5
            E.suitLength T.Hearts 5
            _ <- response
            E.makePass
            E.suitLength T.Spades 4
            E.suitLength T.Hearts 3
            E.forEach T.minorSuits (`E.minSuitLength` 2)
            E.soundHolding T.Hearts
            -- If we had another side king, we could already bid 7N.
            E.forbidAll [E.hasCard T.Clubs 'K', E.hasCard T.Diamonds 'K']
        explanation =
            "We have all the keycards and the queen of trump. We're going " .+
            "to take 5 spade tricks, 5 heart tricks, and two minor-suit " .+
            "aces. If partner has a minor-suit king, " .+ T.Bid 7 T.Notrump .+
            " is a laydown. If they don't, then " .+ T.Bid 6 T.Notrump .+
            " is a laydown but any grand slam is likely doomed. Ask about " .+
            "side-suit kings, and see how partner responds."
      in situation "Kask" action kingAskBid explanation
  in
    wrapNW $ return sit <~ [(RKC.bS5H, RKC.bS5H5N), (RKC.bS5S, RKC.bS5S5N)]


kingAskResponsePos :: Situations
kingAskResponsePos = let
    sit (response, kingAskBid) answer = let
        action = do
            -- Start 1S-2N-4H
            setUpAuctionsS !! 1 `andNextBidderIs` T.North
            RKC.b4N
            E.makePass
            E.suitLength T.Spades 5
            E.suitLength T.Hearts 5
            _ <- response
            E.makePass
            E.suitLength T.Spades 4
            E.suitLength T.Hearts 3
            E.forEach T.minorSuits (`E.minSuitLength` 2)
            E.soundHolding T.Hearts
            -- If we had another side king, we could already bid 7N.
            E.forbidAll [E.hasCard T.Clubs 'K', E.hasCard T.Diamonds 'K']
            _ <- kingAskBid
            E.makePass
        explanation =
            "Partner has made a king ask, and seems to have interest " .+
            "in grand slam. Bid our cheapest side-suit king."
      in situation "KaskRP" action answer explanation
  in
    wrapNW $ return sit <~ [(RKC.bS5H, RKC.bS5H5N), (RKC.bS5S, RKC.bS5S5N)]
                        <~ [RKC.bS5H5N6C, RKC.bS5H5N6D, RKC.bS5H5N6H]


kingAskResponseNeg :: Situations
kingAskResponseNeg = let
    sit (response, kingAskBid) = let
        action = do
            -- Start 1S-2N-4H
            setUpAuctionsS !! 1 `andNextBidderIs` T.North
            RKC.b4N
            E.makePass
            E.suitLength T.Spades 5
            E.suitLength T.Hearts 5
            _ <- response
            E.makePass
            E.suitLength T.Spades 4
            E.suitLength T.Hearts 3
            E.forEach T.minorSuits (`E.minSuitLength` 2)
            E.soundHolding T.Hearts
            -- If we had another side king, we could already bid 7N.
            E.forbidAll [E.hasCard T.Clubs 'K', E.hasCard T.Diamonds 'K']
            _ <- kingAskBid
            E.makePass
        explanation =
            "Partner has made a king ask, and seems to have interest " .+
            "in grand slam. However, we don't have any side-suit kings " .+
            "to help them find a thirteenth trick. Sign off in small slam. " .+
            "(Partner might pull this to " .+ T.Bid 6 T.Notrump .+ ", " .+
            "especially at matchpoints.)"
      in situation "KaskRN" action RKC.bS5H5N6S explanation
  in
    wrapNW $ return sit <~ [(RKC.bS5H, RKC.bS5H5N), (RKC.bS5S, RKC.bS5S5N)]


tellerClaimsQueen1430, tellerClaimsQueen3014 :: Situations
(tellerClaimsQueen1430, tellerClaimsQueen3014) = (sit1430, sit3014)
  where
    sit (setups, payoffs) = let
        inner setup (middle, answers) = let
            inner' answer = let
                action = do setup `andNextBidderIs` T.South
                            middle
                explanation =
                    "With a 10-card trump fit, it's very likely that the " .+
                    "queen of trump will drop singleton or doubleton (or " .+
                    "be 3" .+ NDash .+ "0 onside). To help partner place " .+
                    "the contract, we should pretend to have the queen even " .+
                    "when we don't."
              in situation "rQlie" action answer explanation
          in return inner' <~ answers
      in join $ return inner <~ setups <~ payoffs
    setupHearts = [ do J2N.b1H
                       noInterference T.Hearts
                       E.suitLength T.Hearts 4  -- Speed up performance
                       J2N.b1H2N
                       noInterference T.Hearts
                       E.suitLength T.Hearts 6
                       J2N.b1H2N4H
                       E.makePass
                       E.alternatives [E.pointRange 17 40, E.maxLoserCount 5]
                       RKC.b4N
                       E.makePass
                  , do NT.b1N
                       NT.noInterference
                       E.suitLength T.Hearts 6
                       NT.b1N4D
                       E.makePass
                       E.suitLength T.Hearts 4
                       NT.b1N4D4H
                       E.makePass
                       NT.slamInterest
                       E.forbid $ E.hasControl T.Spades
                       RKC.b4N
                       E.makePass
                  ]
    setupSpades = [ do J2N.b1S
                       noInterference T.Spades
                       E.suitLength T.Spades 4  -- Speed up performance
                       J2N.b1S2N
                       noInterference T.Spades
                       E.suitLength T.Spades 6
                       J2N.b1S2N4S
                       E.makePass
                       E.alternatives [E.pointRange 17 40, E.maxLoserCount 5]
                       RKC.b4N
                       E.makePass
                  , do NT.b1N
                       NT.noInterference
                       E.suitLength T.Spades 6
                       NT.b1N4H
                       E.makePass
                       E.suitLength T.Spades 4
                       NT.b1N4H4S
                       E.makePass
                       NT.slamInterest
                       RKC.b4N
                       E.makePass
                  ]
    followupQueenAskH5C = [ do E.forbid $ E.hasCard T.Hearts 'Q'
                               E.hasCard T.Spades 'K'
                               E.stealCall RKC.bH5C5D5S
                          , do E.forbid $ E.hasCard T.Hearts 'Q'
                               E.forbid $ E.hasCard T.Spades 'K'
                               E.hasCard T.Clubs 'K'
                               E.stealCall RKC.bH5C5D6C
                          , do E.forbid $ E.hasCard T.Hearts 'Q'
                               E.forbid $ E.hasCard T.Spades 'K'
                               E.forbid $ E.hasCard T.Clubs 'K'
                               E.hasCard T.Diamonds 'K'
                               E.stealCall RKC.bH5C5D6D
                          , do E.forbid $ E.hasCard T.Hearts 'Q'
                               E.forbid $ E.hasCard T.Spades 'K'
                               E.forbid $ E.hasCard T.Clubs 'K'
                               E.forbid $ E.hasCard T.Diamonds 'K'
                               E.stealCall RKC.bH5C5D6H
                          ]
    followupQueenAskH5D = [ do E.forbid $ E.hasCard T.Hearts 'Q'
                               E.hasCard T.Clubs 'K'
                               E.stealCall RKC.bH5D5S6C
                          , do E.forbid $ E.hasCard T.Hearts 'Q'
                               E.forbid $ E.hasCard T.Clubs 'K'
                               E.hasCard T.Diamonds 'K'
                               E.stealCall RKC.bH5D5S6D
                          , do E.forbid $ E.hasCard T.Hearts 'Q'
                               E.forbid $ E.hasCard T.Clubs 'K'
                               E.forbid $ E.hasCard T.Diamonds 'K'
                               E.stealCall RKC.bH5D5S6H
                          ]
    followupQueenAskS5C = [ do E.forbid $ E.hasCard T.Spades 'Q'
                               E.hasCard T.Hearts 'K'
                               E.stealCall RKC.bS5C5D5H
                          , do E.forbid $ E.hasCard T.Spades 'Q'
                               E.forbid $ E.hasCard T.Hearts 'K'
                               E.hasCard T.Clubs 'K'
                               E.stealCall RKC.bS5C5D6C
                          , do E.forbid $ E.hasCard T.Spades 'Q'
                               E.forbid $ E.hasCard T.Hearts 'K'
                               E.forbid $ E.hasCard T.Clubs 'K'
                               E.hasCard T.Diamonds 'K'
                               E.stealCall RKC.bS5C5D6D
                          , do E.forbid $ E.hasCard T.Spades 'Q'
                               E.forbid $ E.hasCard T.Hearts 'K'
                               E.forbid $ E.hasCard T.Clubs 'K'
                               E.forbid $ E.hasCard T.Diamonds 'K'
                               E.stealCall RKC.bS5C5D6S
                          ]
    followupQueenAskS5D = [ do E.forbid $ E.hasCard T.Spades 'Q'
                               E.hasCard T.Clubs 'K'
                               E.stealCall RKC.bS5D5H6C
                          , do E.forbid $ E.hasCard T.Spades 'Q'
                               E.forbid $ E.hasCard T.Clubs 'K'
                               E.hasCard T.Diamonds 'K'
                               E.stealCall RKC.bS5D5H6D
                          , do E.forbid $ E.hasCard T.Spades 'Q'
                               E.forbid $ E.hasCard T.Clubs 'K'
                               E.forbid $ E.hasCard T.Diamonds 'K'
                               E.hasCard T.Hearts 'K'
                               E.stealCall RKC.bS5D5H6H
                          , do E.forbid $ E.hasCard T.Spades 'Q'
                               E.forbid $ E.hasCard T.Clubs 'K'
                               E.forbid $ E.hasCard T.Diamonds 'K'
                               E.forbid $ E.hasCard T.Hearts 'K'
                               E.stealCall RKC.bS5D5H6S
                          ]
    sit1430 = wrapNW . join $ return sit
        <~ [ ( setupHearts
             , [ ( return ()
                 , [ do withholdBid RKC.bH5H
                        E.stealCall RKC.bH5S
                   ]
                 )
               , ( do RKC.b1430H5C
                      E.hasCard T.Hearts 'Q'  -- The opponents have the queen
                      E.makePass
                      RKC.b1430H5C5D
                      E.makePass
                 , followupQueenAskH5C
                 )
               , ( do RKC.b1430H5D
                      E.makePass
                      RKC.b1430H5D5S
                      E.hasCard T.Hearts 'Q'  -- The opponents have the queen
                      E.makePass
                 , followupQueenAskH5D
                 )
               ]
             )
           , ( setupSpades
             , [ ( return ()
                 , [ do withholdBid RKC.bS5H
                        E.stealCall RKC.bS5S
                   ]
                 )
               , ( do RKC.b1430S5C
                      E.makePass
                      RKC.b1430S5C5D
                      E.hasCard T.Spades 'Q'  -- The opponents have the queen
                      E.makePass
                 , followupQueenAskS5C
                 )
               , ( do RKC.b1430S5D
                      E.hasCard T.Spades 'Q'  -- The opponents have the queen
                      E.makePass
                      RKC.b1430S5D5H
                      E.makePass
                 , followupQueenAskS5D
                 )
               ]
             )
           ]
    sit3014 = wrapNW . join $ return sit
        <~ [ ( setupHearts
             , [ ( return ()
                 , [ do withholdBid RKC.bH5H
                        E.stealCall RKC.bH5S
                   ]
                 )
               , ( do RKC.b3014H5C
                      E.makePass
                      RKC.b3014H5C5D
                      E.hasCard T.Hearts 'Q'  -- The opponents have the queen
                      E.makePass
                 , followupQueenAskH5C
                 )
               , ( do RKC.b3014H5D
                      E.hasCard T.Hearts 'Q'  -- The opponents have the queen
                      E.makePass
                      RKC.b3014H5D5S
                      E.makePass
                 , followupQueenAskH5D
                 )
               ]
             )
           , ( setupSpades
             , [ ( return ()
                 , [ do withholdBid RKC.bS5H
                        E.stealCall RKC.bS5S
                   ]
                 )
               , ( do RKC.b3014S5C
                      E.hasCard T.Spades 'Q'  -- The opponents have the queen
                      E.makePass
                      RKC.b3014S5C5D
                      E.makePass
                 , followupQueenAskS5C
                 )
               , ( do RKC.b3014S5D
                      E.makePass
                      RKC.b3014S5D5H
                      E.hasCard T.Spades 'Q'  -- The opponents have the queen
                      E.makePass
                 , followupQueenAskS5D
                 )
               ]
             )
           ]

-- TODO:
-- Pretending to have the Q with a 10-card fit as the asker
--   - hard to do because the next step is to ask for kings, which means we're
--     aiming for grand slam. Maybe 1S-2N-4H-4N-5m-5N?
-- Going to grand slam?
-- Placing the contract after partner shows a void?
-- Unsure how to phrase: trouble when hearts are trump and you have 1 keycard
--     without the queen (worried about partner responding 5S)
-- Show 0 or 3 keycards when you have 0 and a void, rather than 5N?
-- Forbid the opponents from making a lead-directing double


topic1430 :: Topic
topic1430 =
    makeTopic "Roman Keycard Blackwood 1430" "RKC1430" (wrap sits)
  where
    sits = [ initiate                               --  0
           , firstResponse1430                      --  1
           , signoffPartscore1430                   --  2
           , wrap [oddVoid, evenVoid]               --  3
           , queenAsk1430                           --  4
           , noQueen1430                            --  5
           , wrapWeighted [(3, queenNoKing1430), (1, queenNoKing5N1430)]
           , queenKing1430                          --  7
           , slamSignoff1430                        --  8
           , kingAsk                                --  9, rare: often times out
           , kingAskResponsePos                     -- 10, rare: often times out
           , kingAskResponseNeg                     -- 11, rare: often times out
           , tellerClaimsQueen1430                  -- 12, rare: often times out
           ]


topic3014 :: Topic
topic3014 =
    makeTopic "Roman Keycard Blackwood 3014" "RKC3014" (wrap sits)
  where
    sits = [ initiate                               --  0
           , firstResponse3014                      --  1
           , signoffPartscore3014                   --  2
           , wrap [oddVoid, evenVoid]               --  3
           , queenAsk3014                           --  4
           , noQueen3014                            --  5
           , wrapWeighted [(3, queenNoKing3014), (1, queenNoKing5N3014)]
           , queenKing3014                          --  7
           , slamSignoff3014                        --  8
           , kingAsk                                --  9, rare: often times out
           , kingAskResponsePos                     -- 10, rare: often times out
           , kingAskResponseNeg                     -- 11, rare: often times out
           , tellerClaimsQueen3014                  -- 12, rare: often times out
           ]
