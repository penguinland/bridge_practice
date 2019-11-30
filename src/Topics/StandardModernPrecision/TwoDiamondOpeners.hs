module Topics.StandardModernPrecision.TwoDiamondOpeners(topic) where

import Output(output)
import Topic(Topic(..), base, (<~), wrap, Situations)
import Auction({-forbid,-} pointRange, suitLength, minSuitLength, maxSuitLength,
               Action, alternatives, constrain, makePass, makeCall)
import Situation(situation)
import qualified Terminology as T
import qualified CommonBids as B


twoDiamondOpener :: Action
twoDiamondOpener = do
    pointRange 11 15
    constrain "two_diamond_opener" ["shape(", ", 4414 + 4405 + 4315 + 3415)"]

noDirectOvercall :: Action
noDirectOvercall = do
    B.cannotPreempt
    alternatives $ [
        pointRange 0 10  -- Not enough to overcall
      , pointRange 11 16 >>  -- Enough to overcall, but no suit
            constrain "no_suit" ["shape(", ", any 4333 + any 4432)"]]
    makePass


bestFitSpades :: Action
bestFitSpades = do
    minSuitLength T.Spades 4
    maxSuitLength T.Hearts 3
    maxSuitLength T.Diamonds 6
    maxSuitLength T.Clubs 3


open :: Situations
open = let
    sit dealer vul = let
        action = do
            B.setDealerAndOpener dealer T.South
            twoDiamondOpener
        explanation fmt =
            "With an opening hand too weak to bid " ++
            output fmt (T.Bid 1 T.Clubs) ++ ", open " ++
            output fmt (T.Bid 2 T.Diamonds) ++ " with no 5-card major, " ++
            "no 6-card club suit, and at most 1 diamond."
      in
        situation "Open" dealer vul action (T.Bid 2 T.Diamonds) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities


immediateSignoffSpades3 :: Situations
immediateSignoffSpades3 = let
    sit dealer vul = let
        action = do
            B.setDealerAndOpener dealer T.North
            twoDiamondOpener
            suitLength T.Spades 3
            makeCall (T.Bid 2 T.Diamonds)
            noDirectOvercall
            pointRange 0 9
            bestFitSpades
            suitLength T.Spades 4
        explanation fmt =
            "Without the strength for a game contract, sign off in " ++
            output fmt (T.Bid 2 T.Spades) ++ " with a likely fit. If " ++
            "you're stuck playing a 4-3 fit, oh well."
      in
        situation "S43" dealer vul action (T.Bid 2 T.Spades) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities


immediateSignoffSpades4 :: Situations
immediateSignoffSpades4 = let
    sit dealer vul = let
        action = do
            B.setDealerAndOpener dealer T.North
            twoDiamondOpener
            suitLength T.Spades 4
            makeCall (T.Bid 2 T.Diamonds)
            noDirectOvercall
            pointRange 0 9
            bestFitSpades
            suitLength T.Spades 4
        explanation fmt =
            "Without the strength for a game contract, sign off in " ++
            output fmt (T.Bid 2 T.Spades) ++ " with a likely fit."
      in
        situation "S44" dealer vul action (T.Bid 2 T.Spades) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities


immediateSignoffSpades5 :: Situations
immediateSignoffSpades5 = let
    sit dealer vul = let
        action = do
            B.setDealerAndOpener dealer T.North
            twoDiamondOpener
            makeCall (T.Bid 2 T.Diamonds)
            noDirectOvercall
            pointRange 0 6  -- With 7-9 HCP, make a mixed raise!
            bestFitSpades
            minSuitLength T.Spades 5
        explanation fmt =
            "Without the strength for a game contract, sign off in " ++
            output fmt (T.Bid 2 T.Spades) ++ " with a guaranteed fit."
      in
        situation "Sfit" dealer vul action (T.Bid 2 T.Spades) explanation
  in
    -- Although this can happen when anyone is dealer, it is very rare when East
    -- deals, and generating those hands is difficult for dealer (often
    -- requiring over 1,000,000 hands to be generated). Consequently, we limit
    -- these situations to times when East isn't dealer.
    wrap $ base sit <~ [T.North, T.South, T.West] <~ T.allVulnerabilities


passSignoff2Spades :: Situations
passSignoff2Spades = let
    sit dealer vul spadeLength = let
        action = do
            B.setDealerAndOpener dealer T.South
            twoDiamondOpener
            makeCall (T.Bid 2 T.Diamonds)
            noDirectOvercall
            bestFitSpades
            alternatives [    suitLength T.Spades 4 >> pointRange 0 9
                         , minSuitLength T.Spades 5 >> pointRange 0 6
                         ]
            makeCall (T.Bid 2 T.Spades)
            noDirectOvercall
            suitLength T.Spades spadeLength
        explanation _ =
            "Partner has less-than-invitational values and is signing off. " ++
            "Just pass" ++
            (if spadeLength == 4 then "." else
                ", even though you might be in a 7-card fit.")
      in
        situation "P2S" dealer vul action (T.Pass) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities <~ [3, 4]




topic :: Topic
topic = Topic "SMP 2D auctions" "SMP2D" situations
  where
    situations = wrap [ open
                      , wrap [ immediateSignoffSpades3
                             , immediateSignoffSpades4
                             , immediateSignoffSpades5]
                      , passSignoff2Spades
                      ]
