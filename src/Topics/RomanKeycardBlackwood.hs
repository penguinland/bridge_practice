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
setUpAuctionsH :: [Action]
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

setUpAuctionsS :: [Action]
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


firstResponse1430 :: Situations
firstResponse1430 = let
    sit (setups, responses) = let
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
  in
    wrapNW . join $ return sit <~ [ (setUpAuctionsH, [ RKC.b1430H5C
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


firstResponse3014 :: Situations
firstResponse3014 = let
    sit (setups, responses) = let
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
  in
    wrapNW . join $ return sit <~ [ (setUpAuctionsH, [ RKC.b1430H5C
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


signoff1430 :: Situations
signoff1430 = let
    sit (setups, followups) = let
        inner setup (response, signoff) = let
            action = do
                setup `andNextBidderIs` T.South
                RKC.b4N
                makePass
                _ <- response
                makePass
            explanation =
                "We asked for keycards, but learned we're missing 2 of " .+
                "them. Slam is likely to fail: sign off at the 5 level."
          in situation "signoff" action signoff explanation
      in return inner <~ setups <~ followups
  in
    wrapNW . join $ return sit
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


signoff3014 :: Situations
signoff3014 = let
    sit (setups, followups) = let
        inner setup (response, signoff) = let
            action = do
                setup `andNextBidderIs` T.South
                RKC.b4N
                makePass
                _ <- response
                makePass
            explanation =
                "We asked for keycards, but learned we're missing 2 of " .+
                "them. Slam is likely to fail: sign off at the 5 level."
          in situation "signoff" action signoff explanation
      in return inner <~ setups <~ followups
  in
    wrapNW . join $ return sit
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


queenAsk1430 :: Situations
queenAsk1430 = let
    sit (setups, response, queenAsk) = let
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
                "non-signoff bid to ask about the queen. (If hearts are " .+
                "trump and the cheapest relay is " .+ T.Bid 5 T.Spades .+
                ", you are pushing to slam no matter what. Make sure you're " .+
                "okay with that no matter what partner does.)"
          in situation "qask" action queenAsk explanation
      in return inner <~ setups
  in
    wrapNW . join $ return sit
        <~ [ (setUpAuctionsH, RKC.b1430H5C, RKC.b1430H5C5D)
           , (setUpAuctionsH, RKC.b1430H5D, RKC.b1430H5D5S)
           , (setUpAuctionsS, RKC.b1430S5C, RKC.b1430S5C5D)
           , (setUpAuctionsS, RKC.b1430S5D, RKC.b1430S5D5H)
           ]


queenAsk3014 :: Situations
queenAsk3014 = let
    sit (setups, response, queenAsk) = let
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
                "non-signoff bid to ask about the queen. (If hearts are " .+
                "trump and the cheapest relay is " .+ T.Bid 5 T.Spades .+
                ", you are pushing to slam no matter what. Make sure you're " .+
                "okay with that no matter what partner does.)"
          in situation "qask" action queenAsk explanation
      in return inner <~ setups
  in
    wrapNW . join $ return sit
        <~ [ (setUpAuctionsH, RKC.b3014H5C, RKC.b3014H5C5D)
           , (setUpAuctionsH, RKC.b3014H5D, RKC.b3014H5D5S)
           , (setUpAuctionsS, RKC.b3014S5C, RKC.b3014S5C5D)
           , (setUpAuctionsS, RKC.b3014S5D, RKC.b3014S5D5H)
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
                      ]

topic3014 :: Topic
topic3014 = makeTopic "Roman Keycard Blackwood 3014" "RKC3014" situations
  where
    situations = wrap [ initiate
                      , firstResponse3014
                      , signoff3014
                      , wrap [oddVoid, evenVoid]
                      , queenAsk3014
                      ]
