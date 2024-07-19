module Topics.StandardModernPrecision.Lampe(topic) where

import CommonBids(noInterference)
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)
import Bids.StandardModernPrecision.BasicBids(
    firstSeatOpener, smpWrapN, smpWrapS)
import qualified Bids.StandardModernPrecision.Lampe as B


twoClubs :: Situations
twoClubs = let
    action = do
        firstSeatOpener
        B.b1D
        noInterference T.Diamonds
    explanation =
        "With at least invitational strength and a minor, start with an " .+
        "artificial " .+ T.Bid 2 T.Clubs .+ "."
  in
    smpWrapN . return $ situation "2C" action B.b1D2C explanation


twoClubsUnbal :: Situations
twoClubsUnbal = let
    action = do
        firstSeatOpener
        B.b1D
        noInterference T.Diamonds
        B.b1D2C
        noInterference T.Diamonds
    explanation =
        "With an unbalanced minimum, rebid an artificial " .+
        T.Bid 2 T.Diamonds .+ "."
  in
    smpWrapS . return $ situation "2CUnbal" action B.b1D2C2D explanation


twoClubsBal :: Situations
twoClubsBal = let
    action = do
        firstSeatOpener
        B.b1D
        noInterference T.Diamonds
        B.b1D2C
        noInterference T.Diamonds
    explanation =
        "With a balanced minimum, rebid an artificial " .+
        T.Bid 2 T.Hearts .+ "."
  in
    smpWrapS . return $ situation "2CBal" action B.b1D2C2H explanation


twoClubsSS :: Situations
twoClubsSS = let
    sit answer = let
        action = do
            firstSeatOpener
            B.b1D
            noInterference T.Diamonds
            B.b1D2C
            noInterference T.Diamonds
        explanation =
            "You opened an ambiguous " .+ T.Bid 1 T.Diamonds .+ ", and " .+
            "partner has shown an invitational or better hand with a minor." .+
            "With a maximum, make a shape-shower bid immediately. Partner " .+
            "is invitational or better, and this bid indicates you would " .+
            "accept an invitation, so is game-forcing."
      in
        situation "2CSS" action answer explanation
  in
    -- For us to bid a forcing 1N, we must be an unpassed hand.
    wrap $ return sit
        <~ [B.b1D2C2S, B.b1D2C2N, B.b1D2C3C, B.b1D2C3D, B.b1D2C3H, B.b1D2C3S]
        <~ T.allVulnerabilities
        <~ [T.South]


twoDiamonds :: Situations
twoDiamonds = let
    action = do
        firstSeatOpener
        B.b1D
        noInterference T.Diamonds
    explanation =
        "With both majors and less than game forcing strength, make a " .+
        "Reverse-Flannery-like " .+ T.Bid 2 T.Diamonds .+ " bid."
  in
    smpWrapN . return $ situation "2D" action B.b1D2D explanation


topic :: Topic
topic = makeTopic summary "Lampe" situations
  where
    summary = "Lampe responses to Precision 1" .+ T.Diamonds .+ NDash .+ "2m"
    situations = wrap [ twoClubs
                      , twoClubsUnbal
                      , twoClubsBal
                      , twoClubsSS
                      , twoDiamonds
                      ]
