module Topics.RomanKeycardBlackwood(topic1430, topic3014) where

import Control.Monad(join)
import Data.List(sort)

import Action(Action)
import qualified Bids.RomanKeycardBlackwood as RKC
import qualified Bids.Jacoby2NT as J2N
import CommonBids(andNextBidderIs, noInterference)
import EDSL(makePass, pointRange, suitLength)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapNW, Situations, makeTopic)


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

setUpAuctionsH :: [Action]  -- Auctions where the next bid should be RKC for H
setUpAuctionsH = [ do J2N.b1H  -- Index 0
                      noInterference T.Hearts
                      suitLength T.Hearts 4  -- Speed up performance
                      J2N.b1H2N
                      noInterference T.Hearts
                      -- Unlike index 1, don't force opener to have only 5
                      -- hearts. Maybe they have 6 and we're going to practice
                      -- pretending to have the queen with a 10-card fit.
                      J2N.b1H2N4H
                      makePass
                      pointRange 17 40
                 , do J2N.b1H  -- Index 1
                      noInterference T.Hearts
                      suitLength T.Hearts 4  -- Speed up performance
                      J2N.b1H2N
                      noInterference T.Hearts
                      suitLength T.Hearts 5  -- Speed up performance
                      J2N.b1H2N4D
                      makePass
                      pointRange 16 40
                 ]

setUpAuctionsS :: [Action]  -- Auctions where the next bid should be RKC for S
setUpAuctionsS = [ do J2N.b1S  -- Index 0
                      noInterference T.Spades
                      suitLength T.Spades 4  -- Speed up performance
                      J2N.b1S2N
                      noInterference T.Spades
                      -- Unlike index 1, don't force opener to have only 5
                      -- spades. Maybe they have 6 and we're going to practice
                      -- pretending to have the queen with a 10-card fit.
                      J2N.b1S2N4S
                      makePass
                      pointRange 17 40
                 , do J2N.b1S  -- Index 1
                      noInterference T.Spades
                      suitLength T.Spades 4  -- Speed up performance
                      J2N.b1S2N
                      noInterference T.Spades
                      suitLength T.Spades 5  -- Speed up performance
                      J2N.b1S2N4H
                      makePass
                      pointRange 16 40
                 ]

-- Auctions where the next bid should be RKC but the keycard teller should
-- definitely not lie and pretend they have the queen if they don't
setUpAuctionsHNoQ :: [Action]
setUpAuctionsHNoQ = [ do J2N.b1H  -- Index 0
                         noInterference T.Hearts
                         suitLength T.Hearts 4  -- Speed up performance
                         J2N.b1H2N
                         noInterference T.Hearts
                         suitLength T.Hearts 5  -- With 6+, pretend to have Q
                         J2N.b1H2N4H
                         makePass
                         pointRange 17 40
                    , do J2N.b1H  -- Index 1
                         noInterference T.Hearts
                         suitLength T.Hearts 4  -- Speed up performance
                         J2N.b1H2N
                         noInterference T.Hearts
                         suitLength T.Hearts 5  -- Speed up performance
                         J2N.b1H2N4D
                         makePass
                         pointRange 16 40
                    ]

-- Auctions where the next bid should be RKC but the keycard teller should
-- definitely not lie and pretend they have the queen if they don't
setUpAuctionsSNoQ :: [Action]
setUpAuctionsSNoQ = [ do J2N.b1S  -- Index 0
                         noInterference T.Spades
                         suitLength T.Spades 4  -- Speed up performance
                         J2N.b1S2N
                         noInterference T.Spades
                         suitLength T.Hearts 5  -- With 6+, pretend to have Q
                         J2N.b1S2N4S
                         makePass
                         pointRange 17 40
                    , do J2N.b1S  -- Index 1
                         noInterference T.Spades
                         suitLength T.Spades 4  -- Speed up performance
                         J2N.b1S2N
                         noInterference T.Spades
                         suitLength T.Spades 5  -- Speed up performance
                         J2N.b1S2N4H
                         makePass
                         pointRange 16 40
                    ]


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
                makePass
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


signoff1430, signoff3014 :: Situations
(signoff1430, signoff3014) = (signoff1430', signoff3014')
  where
    signoff (setups, followups) = let
        inner setup (response, signoffBid) = let
            action = do
                setup `andNextBidderIs` T.South
                RKC.b4N
                makePass
                _ <- response
                makePass
            explanation =
                "We asked for keycards, but learned we're missing 2 of " .+
                "them. Slam is likely to fail: sign off at the 5 level."
          in situation "signoff" action signoffBid explanation
      in return inner <~ setups <~ followups
    signoff1430' = wrapNW . join $ return signoff
        <~ [ (setUpAuctionsH, [ (RKC.b1430H5C, RKC.b1430H5C5H)
                              , (RKC.b1430H5D, RKC.b1430H5D5H)
                              , (RKC.bH5H,     RKC.bH5HP)
                              -- If hearts are trump and partner bid 5S, we
                              -- can't sign off. Handle this separately.
                              --, (RKC.bH5S,     trouble)
                              ])
           , (setUpAuctionsS, [ (RKC.b1430S5C, RKC.b1430S5C5S)
                              , (RKC.b1430S5D, RKC.b1430S5D5S)
                              , (RKC.bS5H,     RKC.bS5H5S)
                              , (RKC.bS5S,     RKC.bS5SP)
                              ])
           ]
    signoff3014' = wrapNW . join $ return signoff
        <~ [ (setUpAuctionsH, [ (RKC.b3014H5C, RKC.b3014H5C5H)
                              , (RKC.b3014H5D, RKC.b3014H5D5H)
                              , (RKC.bH5H,     RKC.bH5HP)
                              -- If hearts are trump and partner bid 5S, we
                              -- can't sign off. Handle this separately.
                              --, (RKC.bH5S,     trouble)
                              ])
           , (setUpAuctionsS, [ (RKC.b3014S5C, RKC.b3014S5C5S)
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
                makePass
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
             ([setUpAuctionsH !! 1], [RKC.bH6C, RKC.bH6H])
             -- The keycard teller has already shown a natural heart suit: can't
             -- have a void in hearts
           , ([setUpAuctionsS !! 1], [RKC.bS6C, RKC.bS6D])
           ]


evenVoid :: Situations
evenVoid = let
    sit (setups, response) = let
        inner setup = let
            action = do
                setup `andNextBidderIs` T.North
                RKC.b4N
                makePass
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
                makePass
                _ <- response
                makePass
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
    queenAsk1430' = wrapNW . join $ return queenAsk
        <~ [ (setUpAuctionsH, RKC.b1430H5C, RKC.b1430H5C5D, False)
           , (setUpAuctionsH, RKC.b1430H5D, RKC.b1430H5D5S, True)
           , (setUpAuctionsS, RKC.b1430S5C, RKC.b1430S5C5D, False)
           , (setUpAuctionsS, RKC.b1430S5D, RKC.b1430S5D5H, False)
           ]
    queenAsk3014' = wrapNW . join $ return queenAsk
        <~ [ (setUpAuctionsH, RKC.b3014H5C, RKC.b3014H5C5D, False)
           , (setUpAuctionsH, RKC.b3014H5D, RKC.b3014H5D5S, True)
           , (setUpAuctionsS, RKC.b3014S5C, RKC.b3014S5C5D, False)
           , (setUpAuctionsS, RKC.b3014S5D, RKC.b3014S5D5H, False)
           ]


noQueen1430, noQueen3014 :: Situations
(noQueen1430, noQueen3014) = (noQueen1430', noQueen3014')
  where
    noQueen (setups, followups) = let
        inner setup (response, queenAsk, signoff) = let
            action = do
                setup `andNextBidderIs` T.North
                RKC.b4N
                makePass
                _ <- response
                makePass
                _ <- queenAsk
                makePass
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
                makePass
                _ <- response
                makePass
                _ <- queenAsk
                makePass
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
    queenNoKing1430' = wrapNW . join $ return queenNoKing
        <~ [ (setUpAuctionsH, [ (RKC.b1430H5C, RKC.b1430H5C5D, RKC.bH5C5D6H)
                              -- Have separate commentary for this one
                              --, (RKC.b1430H5D, RKC.b1430H5D5S, RKC.bH5D5S5N)
                              ])
           , (setUpAuctionsS, [ (RKC.b1430S5C, RKC.b1430S5C5D, RKC.bS5C5D6S)
                              , (RKC.b1430S5D, RKC.b1430S5D5H, RKC.bS5D5H6S)
                              ])
           ]
    queenNoKing3014' = wrapNW . join $ return queenNoKing
        <~ [ (setUpAuctionsH, [ (RKC.b3014H5C, RKC.b3014H5C5D, RKC.bH5C5D6H)
                              -- Have separate commentary for this one
                              --, (RKC.b3014H5D, RKC.b3014H5D5S, RKC.bH5D5S5N)
                              ])
           , (setUpAuctionsS, [ (RKC.b3014S5C, RKC.b3014S5C5D, RKC.bS5C5D6S)
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
            makePass
            _ <- response
            makePass
            _ <- queenAsk
            makePass
        explanation =
            "Partner has made a queen ask, but it is higher than 5 " .+
            "of our trump suit. This means that denying the queen (by " .+
            "bidding our trump suit as cheaply as possible) is the same " .+
            "as jumping to small slam. Consequently, the bid to show " .+
            "the queen but no showable (minor-suit) king is to bid " .+
            RKC.bH5D5S5N .+ " instead."
      in situation "QnoK5S" action RKC.bH5D5S5N explanation
    queenNoKing1430' = wrapNW $
        (return $ queenNoKing RKC.b1430H5D RKC.b1430H5D5S) <~ setUpAuctionsH
    queenNoKing3014' = wrapNW $
        (return $ queenNoKing RKC.b3014H5D RKC.b3014H5D5S) <~ setUpAuctionsH


queenKing1430, queenKing3014 :: Situations
(queenKing1430, queenKing3014) = (queenKing1430', queenKing3014')
  where
    queenKing (setups, middle, followups) = let
        inner setup answer = let
            action = do
                setup `andNextBidderIs` T.North
                RKC.b4N
                makePass
                middle
            explanation =
                "Partner has made a queen ask. We have the queen and at " .+
                "least one side-suit king. Bid our cheapest side-suit king " .+
                "to show that one and deny any kings in cheaper suits. " .+
                "Partner will place the final contract from here."
          in situation "QK" action answer explanation
      in return inner <~ setups <~ followups
    queenKing1430' = wrapNW . join $ return queenKing
        <~ [ (setUpAuctionsH,
              do RKC.b1430H5C
                 makePass
                 RKC.b1430H5C5D
                 makePass,
              [RKC.bH5C5D5S, RKC.bH5C5D6C, RKC.bH5C5D6D])
           , (setUpAuctionsH,
              do RKC.b1430H5D
                 makePass
                 RKC.b1430H5D5S
                 makePass,
              [RKC.bH5D5S6C, RKC.bH5D5S6D])
           , (setUpAuctionsS,
              do RKC.b1430S5C
                 makePass
                 RKC.b1430S5C5D
                 makePass,
              [RKC.bS5C5D5H, RKC.bS5C5D6C, RKC.bS5C5D6D])
           , (setUpAuctionsS,
              do RKC.b1430S5D
                 makePass
                 RKC.b1430S5D5H
                 makePass,
              [RKC.bS5D5H6C, RKC.bS5D5H6D, RKC.bS5D5H6H])
           ]
    queenKing3014' = wrapNW . join $ return queenKing
        <~ [ (setUpAuctionsH,
              do RKC.b3014H5C
                 makePass
                 RKC.b3014H5C5D
                 makePass,
              [RKC.bH5C5D5S, RKC.bH5C5D6C, RKC.bH5C5D6D])
           , (setUpAuctionsH,
              do RKC.b3014H5D
                 makePass
                 RKC.b3014H5D5S
                 makePass,
              [RKC.bH5D5S6C, RKC.bH5D5S6D])
           , (setUpAuctionsS,
              do RKC.b3014S5C
                 makePass
                 RKC.b3014S5C5D
                 makePass,
              [RKC.bS5C5D5H, RKC.bS5C5D6C, RKC.bS5C5D6D])
           , (setUpAuctionsS,
              do RKC.b3014S5D
                 makePass
                 RKC.b3014S5D5H
                 makePass,
              [RKC.bS5D5H6C, RKC.bS5D5H6D, RKC.bS5D5H6H])
           ]



-- TODO:
-- Respond to queen ask
-- Signing off in slam
-- 5N as king ask
-- respond to 5N
-- Going to grand slam?
-- Placing the contract after partner shows a void?
-- Unsure how to phrase: trouble when hearts are trump and you have 1 keycard
--     without the queen (worried about partner responding 5S)
-- Show 0 or 3 keycards when you have 0 and a void, rather than 5N?


topic1430 :: Topic
topic1430 = makeTopic "Roman Keycard Blackwood 1430" "RKC1430" situations
  where
    situations = wrap [ initiate
                      , firstResponse1430
                      , signoff1430
                      , wrap [oddVoid, evenVoid]
                      , queenAsk1430
                      , noQueen1430
                      , wrap [ queenNoKing1430
                             , queenNoKing1430
                             , queenNoKing1430
                             , queenNoKing5N1430
                             ]
                      , queenKing1430
                      ]

topic3014 :: Topic
topic3014 = makeTopic "Roman Keycard Blackwood 3014" "RKC3014" situations
  where
    situations = wrap [ initiate
                      , firstResponse3014
                      , signoff3014
                      , wrap [oddVoid, evenVoid]
                      , queenAsk3014
                      , noQueen3014
                      , wrap [ queenNoKing3014
                             , queenNoKing3014
                             , queenNoKing3014
                             , queenNoKing5N3014
                             ]
                      , queenKing3014
                      ]
