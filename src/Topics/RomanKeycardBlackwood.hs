module Topics.RomanKeycardBlackwood(topic1430, topic3014) where

import Control.Monad(join)
import Data.List(sort)

import Action(Action)
import qualified Bids.RomanKeycardBlackwood as RKC
import qualified Bids.Jacoby2NT as J2N
import CommonBids(andNextBidderIs, noInterference)
import EDSL(makeCall, makePass, pointRange, keycardCount)
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
                      J2N.b1H2N
                      noInterference T.Hearts
                      J2N.b1H2N4H
                      makePass
                      pointRange 17 40
                 , do J2N.b1H  -- Index 1
                      noInterference T.Hearts
                      J2N.b1H2N
                      noInterference T.Hearts
                      J2N.b1H2N4D
                      makePass
                      pointRange 16 40
                 ]

setUpAuctionsS :: [Action]
setUpAuctionsS = [ do J2N.b1S  -- Index 0
                      noInterference T.Spades
                      J2N.b1S2N
                      noInterference T.Spades
                      J2N.b1S2N4S
                      makePass
                      pointRange 17 40
                 , do J2N.b1S  -- Index 1
                      noInterference T.Spades
                      J2N.b1S2N
                      noInterference T.Spades
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
    sit (setups, suit, followups) = let
        inner setup (response, countA, countB, signoff) = let
            action = do
                setup `andNextBidderIs` T.South
                RKC.b4N
                makePass
                _ <- response
                makePass
                keycardCount suit countA countB
            explanation =
                "We asked for keycards, but learned we're missing 2 of " .+
                "them. Slam is likely to fail: sign off at the 5 level."
          in situation "fail" action signoff explanation
      in return inner <~ setups <~ followups
  in
    wrapNW . join $ return sit <~ [
        (setUpAuctionsH, T.Hearts,
            [ (RKC.b1430H5C, 2, 5, makeCall (T.Bid 5 T.Hearts))
            , (RKC.b1430H5D, 3, 0, makeCall (T.Bid 5 T.Hearts))
            , (RKC.bH5H,     1, 4, makeCall (T.Pass          ))
            -- If hearts are trump and partner bid 5S, we can't sign off. Handle
            -- this separately.
            --, (RKC.bH5S,     1, 4, makeCall (trouble         ))
            ])
      , (setUpAuctionsS, T.Spades,
            [ (RKC.b1430S5C, 2, 5, makeCall (T.Bid 5 T.Spades))
            , (RKC.b1430S5D, 3, 0, makeCall (T.Bid 5 T.Spades))
            , (RKC.bS5H,     1, 4, makeCall (T.Bid 5 T.Spades))
            , (RKC.bS5S,     1, 4, makeCall (T.Pass          ))
            ])
      ]


signoff3014 :: Situations
signoff3014 = let
    sit (setups, suit, followups) = let
        inner setup (response, countA, countB, signoff) = let
            action = do
                setup `andNextBidderIs` T.South
                RKC.b4N
                makePass
                _ <- response
                makePass
                keycardCount suit countA countB
            explanation =
                "We asked for keycards, but learned we're missing 2 of " .+
                "them. Slam is likely to fail: sign off at the 5 level."
          in situation "fail" action signoff explanation
      in return inner <~ setups <~ followups
  in
    wrapNW . join $ return sit <~ [
        (setUpAuctionsH, T.Hearts,
            [ (RKC.b3014H5C, 3, 0, makeCall (T.Bid 5 T.Hearts))
            , (RKC.b3014H5D, 2, 5, makeCall (T.Bid 5 T.Hearts))
            , (RKC.bH5H,     1, 4, makeCall (T.Pass          ))
            -- If hearts are trump and partner bid 5S, we can't sign off. Handle
            -- this separately.
            --, (RKC.bH5S,     1, 4, makeCall (trouble         ))
            ])
      , (setUpAuctionsS, T.Spades,
            [ (RKC.b3014S5C, 3, 0, makeCall (T.Bid 5 T.Spades))
            , (RKC.b3014S5D, 2, 5, makeCall (T.Bid 5 T.Spades))
            , (RKC.bS5H,     1, 4, makeCall (T.Bid 5 T.Spades))
            , (RKC.bS5S,     1, 4, makeCall (T.Pass          ))
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
          in situation "oddVoid" action response explanation
      in return inner <~ setups <~ responses
    auctionsH = takeIndices_ [1] setUpAuctionsH
    auctionsS = takeIndices_ [1] setUpAuctionsS
  in
    wrapNW . join $ return sit <~ [
        (auctionsH, [RKC.bH6C, RKC.bH6D, RKC.bH6H])
      , (auctionsS, [RKC.bS6C, RKC.bS6D, RKC.bS6H])
      ]


-- TODO:
-- Queen ask
-- Respond to queen ask
-- Signing off in slam
-- 5N as king ask
-- respond to 5N
-- Going to grand slam?
-- Placing the contract after partner shows a void?
-- Unsure how to phrase: trouble when hearts are trump and you have 1 keycard
--     without the queen (worried about partner responding 5S)


topic1430 :: Topic
topic1430 = makeTopic "Roman Keycard Blackwood 1430" "RKC1430" situations
  where
    situations = wrap [ initiate
                      , firstResponse1430
                      , signoff1430
                      , oddVoid
                      ]

topic3014 :: Topic
topic3014 = makeTopic "Roman Keycard Blackwood 3014" "RKC3014" situations
  where
    situations = wrap [ initiate
                      , firstResponse3014
                      , signoff3014
                      , oddVoid
                      ]
