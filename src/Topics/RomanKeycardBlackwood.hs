module Topics.RomanKeycardBlackwood(topic1430, topic3014) where

import Control.Monad(join)

import Action(Action)
import qualified Bids.RomanKeycardBlackwood as RKC
import qualified Bids.Jacoby2NT as J2N
import CommonBids(andNextBidderIs, noInterference)
import EDSL(makePass, pointRange)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapNW, Situations, makeTopic)


setUpAuctionsH :: [Action]
setUpAuctionsH = [ do J2N.b1H
                      noInterference T.Hearts
                      J2N.b1H2N
                      noInterference T.Hearts
                      J2N.b1H2N4H
                      makePass
                      pointRange 18 40
                 , do J2N.b1H
                      noInterference T.Hearts
                      J2N.b1H2N
                      noInterference T.Hearts
                      J2N.b1H2N4D
                      makePass
                      pointRange 18 40
                 ]

setUpAuctionsS :: [Action]
setUpAuctionsS = [ do J2N.b1S
                      noInterference T.Spades
                      J2N.b1S2N
                      noInterference T.Spades
                      J2N.b1S2N4S
                      makePass
                      pointRange 18 40
                 , do J2N.b1S
                      noInterference T.Spades
                      J2N.b1S2N
                      noInterference T.Spades
                      J2N.b1S2N4H
                      makePass
                      pointRange 18 40
                 ]


initiate :: Situations
initiate = let
    sit setup = let
        action = do
            setup `andNextBidderIs` T.South
        explanation =
            "We've found a trump fit and have slam interest. Time to check " .+
            "for keycards by bidding " .+ RKC.bRKC4N .+ "! If we're missing " .+
            "two of them, we'll sign off at the 5 level."
      in situation "init" action RKC.bRKC4N explanation
  in
    wrapNW $ return sit <~ (setUpAuctionsH ++ setUpAuctionsS)


firstResponse1430 :: Situations
firstResponse1430 = let
    sit (setups, responses) = let
        inner setup response = let
            action = do
                setup `andNextBidderIs` T.North
                RKC.bRKC4N
                makePass
            explanation =
                "Partner has bid " .+ RKC.bRKC4N .+ " to ask how many " .+
                "keycards we have. Give them the answer."
          in situation "resp" action response explanation
      in return inner <~ setups <~ responses
  in
    wrapNW . join $ return sit <~ [ (setUpAuctionsH, [ RKC.bRKC1430H5C
                                                     , RKC.bRKC1430H5D
                                                     , RKC.bRKCH5H
                                                     , RKC.bRKCH5S
                                                     ])
                                  , (setUpAuctionsS, [ RKC.bRKC1430S5C
                                                     , RKC.bRKC1430S5D
                                                     , RKC.bRKCS5H
                                                     , RKC.bRKCS5S
                                                     ])
                                  ]


topic1430 :: Topic
topic1430 = makeTopic "Roman Keycard Blackwood 1430" "RKC1430" situations
  where
    situations = wrap [ initiate
                      , firstResponse1430
                      ]

topic3014 :: Topic
topic3014 = makeTopic "Roman Keycard Blackwood 3014" "RKC3014" situations
  where
    situations = wrap [ initiate
                      --, firstResponse3014
                      ]
