module Topics.StandardModernPrecision.TwoDiamondOpeners(topic) where

import Output(output)
import Topic(Topic(..), base, (<~), wrap, Situations)
import Auction(forbid, pointRange, suitLength, minSuitLength, maxSuitLength,
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
    minSuitLength T.Spades   4
    maxSuitLength T.Hearts   3
    maxSuitLength T.Diamonds 6
    maxSuitLength T.Clubs    3


bestFitClubs :: Action
bestFitClubs = do
    maxSuitLength T.Spades   3
    maxSuitLength T.Hearts   3
    maxSuitLength T.Diamonds 6
    minSuitLength T.Clubs    4
    forbid $ constrain "s3334" ["shape(", ", 3334)"]  -- maybe bid H instead


fitHearts :: Action
fitHearts = do
    alternatives [ constrain "h_2d_resp" ["shape(", ", 3343 + 3352)"]
                 , do maxSuitLength T.Spades   4  -- With 5-4 majors, bid 2S
                      minSuitLength T.Hearts   4
                      maxSuitLength T.Diamonds 6
                      maxSuitLength T.Clubs    4
                 ]


open :: Situations
open = let
    sit dealer vul = let
        action = do
            B.setOpener T.South
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
            B.setOpener T.North
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
            B.setOpener T.North
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
            B.setOpener T.North
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
            B.setOpener T.South
            twoDiamondOpener
            makeCall (T.Bid 2 T.Diamonds)
            noDirectOvercall
            bestFitSpades
            alternatives [    suitLength T.Spades 4 >> pointRange 0 9
                         , minSuitLength T.Spades 5 >> pointRange 0 6
                         ]
            makeCall (T.Bid 2 T.Spades)
            forbid $ B.takeoutDouble T.Spades
            noDirectOvercall
            suitLength T.Spades spadeLength
        explanation _ =
            "Partner has less-than-invitational values and is signing off. " ++
            "Just pass" ++
            (if spadeLength == 4 then "." else
                ", even though you might be in a 7-card fit.")
      in
        situation "2SP" dealer vul action (T.Pass) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities <~ [3, 4]


immediateSignoffClubs :: Situations
immediateSignoffClubs = let
    sit dealer vul = let
        action = do
            B.setOpener T.North
            twoDiamondOpener
            makeCall (T.Bid 2 T.Diamonds)
            noDirectOvercall
            bestFitClubs
            pointRange 0 9
        explanation _ =
            "Without the strength to invite to game, sign off in a club " ++
            "partial."
      in
        situation "3C" dealer vul action (T.Bid 3 T.Clubs) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities


passSignoffClubs :: Situations
passSignoffClubs = let
    sit dealer vul = let
        action = do
            B.setOpener T.South
            twoDiamondOpener
            makeCall (T.Bid 2 T.Diamonds)
            noDirectOvercall
            bestFitClubs
            pointRange 0 9
            makeCall (T.Bid 3 T.Clubs)
            forbid $ B.takeoutDouble T.Clubs
            noDirectOvercall
        explanation _ =
            "Partner has less-than-invitational values and is signing off. " ++
            "Just pass."
      in
        situation "3CP" dealer vul action (T.Pass) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities


immediateSignoffHearts :: Situations
immediateSignoffHearts = let
    sit dealer vul = let
        action = do
            B.setOpener T.North
            twoDiamondOpener
            makeCall (T.Bid 2 T.Diamonds)
            noDirectOvercall
            fitHearts
            pointRange 0 9
        explanation fmt =
            "Without the strength to invite to game, sign off in " ++
            output fmt (T.Bid 2 T.Hearts) ++ ". Remember that opener might " ++
            "pull the bid to " ++ output fmt (T.Bid 2 T.Spades) ++ " with " ++
            "exactly 4315 shape."
      in
        situation "2H" dealer vul action (T.Bid 2 T.Hearts) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities


passSignoffHearts :: Situations
passSignoffHearts = let
    sit dealer vul = let
        action = do
            B.setOpener T.South
            twoDiamondOpener
            makeCall (T.Bid 2 T.Diamonds)
            noDirectOvercall
            fitHearts
            pointRange 0 9
            makeCall (T.Bid 2 T.Hearts)
            forbid $ B.takeoutDouble T.Clubs
            noDirectOvercall
            minSuitLength T.Hearts 4
        explanation _ =
            "Partner has less-than-invitational values and is signing off. " ++
            "Given that you have 4 hearts, pass."
      in
        situation "2HP" dealer vul action (T.Pass) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities


correctSignoffHearts :: Situations
correctSignoffHearts = let
    sit dealer vul heartLength = let
        action = do
            B.setOpener T.South
            twoDiamondOpener
            makeCall (T.Bid 2 T.Diamonds)
            noDirectOvercall
            fitHearts
            maxSuitLength T.Hearts heartLength
            -- With 7-9 HCP and 5+ hearts, make a mixed raise instead!
            alternatives [ pointRange 0 9 >> maxSuitLength T.Hearts 4
                         , pointRange 0 6]
            makeCall (T.Bid 2 T.Hearts)
            forbid $ B.takeoutDouble T.Clubs
            noDirectOvercall
            maxSuitLength T.Hearts 3
        explanation fmt =
            "Partner has less-than-invitational values and is signing off. " ++
            "With 4315 shape, though, pull this to " ++
            output fmt (T.Bid 2 T.Spades) ++ " in case partner only had 3 " ++
            "hearts (e.g., 3343 or 3352 shape)."
      in
        situation "2H2S" dealer vul action (T.Pass) explanation
  in
    -- We want to commonly show times when responder has just 3 hearts (and
    -- we're avoiding a 3-3 fit) and times when responder has a real heart suit.
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities <~ [3, 6]


topic :: Topic
topic = Topic "SMP 2D auctions" "SMP2D" situations
  where
    situations = wrap [ open
                      , wrap [ immediateSignoffSpades3
                             , immediateSignoffSpades4
                             , immediateSignoffSpades5]
                      , passSignoff2Spades
                      , immediateSignoffClubs
                      , passSignoffClubs
                      , immediateSignoffHearts
                      , wrap [ passSignoffHearts
                             , correctSignoffHearts]
                      ]
