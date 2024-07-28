module Topics.StandardModernPrecision.OneClubResponses(
  topic
, topicExtras) where

import Bids.StandardModernPrecision.BasicBids(
    firstSeatOpener, oppsPass, b1C, smpWrapN, smpWrapS)
import qualified Bids.StandardModernPrecision.OneClub as B
import CommonBids(cannotPreempt)
import EDSL(forbid, maxSuitLength, makePass, pointRange, forEach)
import Output(Punct(..), (.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)


oneDiamond :: Situations
oneDiamond = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
    explanation =
        "When game might not be possible opposite a random 17 HCP, start\
      \ with " .+ T.Bid 1 T.Diamonds .+ ". This initiates MaFiA."
  in
    smpWrapN . return $ situation "1D" action B.b1C1D explanation


oneHeart :: Situations
oneHeart = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
    explanation =
        "You've got a game-forcing hand but slam is unlikely. With 8 to 11 HCP,\
      \ bid " .+ T.Bid 1 T.Hearts .+ " to show this kind of hand.\
      \ Subsequent bids are natural 5-card suits (and later 4-card suits), not\
      \ MaFiA."
  in
    smpWrapN . return $ situation "1H" action B.b1C1H explanation


oneHeartNoSpades :: Situations
oneHeartNoSpades = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
    explanation =
        "You've got a game-forcing hand but slam is unlikely. With 8 to 11 HCP\
      \ and no spade suit, bid " .+ T.Bid 1 T.Hearts .+ " to show\
      \ this kind of hand. Partner's rebid will be a natural 5-card suit, not\
      \ MaFiA."
  in
    smpWrapN . return $ situation "1Hns" action B.b1C1Hnos explanation


oneNotrump :: Situations
oneNotrump = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
    explanation =
        "You've got at least mild slam interest with 12+ HCP, and a balanced\
      \ hand with no 5-card suit. Bid a natural " .+ T.Bid 1 T.Notrump .+ ",\
      \ and we'll go from there. Stayman is on, but transfers are not."
  in
    smpWrapN . return $ situation "1N" action B.b1C1N explanation


oneNotrumpAlt :: Situations
oneNotrumpAlt = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
    explanation =
        "You've got at least mild slam interest with 12+ HCP, and a 5-card\
      \ heart suit. Bid " .+ T.Bid 1 T.Notrump .+ " to show this.\
      \ Partner's bids are natural: " .+ T.Bid 2 T.Hearts .+ " is\
      \ a heart raise, other suits are 5 cards long, and " .+
        T.Bid 2 T.Notrump .+ " shows a notrump response.\
      \ Compared to the naive approach, this never wastes extra bidding room\
      \ and often saves some for control bids after we've found a fit."
  in
    smpWrapN . return $ situation "1Nalt" action B.b1C1Nalt explanation


slamSingleSuit :: Situations
slamSingleSuitModified :: Situations
(slamSingleSuit, slamSingleSuitModified) = let
    finalAction T.Clubs    = B.b1C2C
    finalAction T.Diamonds = B.b1C2D
    finalAction T.Hearts   = B.b1C2H
    finalAction T.Spades   = B.b1C1S
    finalAction _          = error "This should never happen"

    sit strain = let
        level = if strain == T.Spades then 1 else 2
        bid = finalAction strain
        action = do
            firstSeatOpener
            b1C
            oppsPass
            forEach (filter (/= strain) T.allSuits) (`maxSuitLength` 4)
        explanation =
            "You've got at least mild slam interest with 12+ HCP, and a 5+ card\
          \ suit. Bid a natural " .+ T.Bid level strain .+ ",\
          \ and we'll go from there. Subsequent bids are natural 5-card (and\
          \ later 4-card) suits, not MaFiA. Once we find a trump fit, we'll\
          \ start control bidding."
      in
        situation "Slam" action bid explanation
  in
    ( smpWrapN $ return sit <~ T.allSuits
    , smpWrapN $ return sit <~ [T.Clubs, T.Diamonds])


twoHeartsBalanced :: Situations
twoHeartsBalanced = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
    explanation =
        "You've got decent slam interest with 14+ HCP, but a balanced\
      \ hand with no 5-card suit. Bid " .+ T.Bid 2 T.Hearts .+ "\
      \ to show this. Partner can try " .+ T.Bid 2 T.Notrump .+ "\
      \ to show hearts, " .+ T.Bid 3 T.Clubs .+ " as Stayman\
      \ (regular-type! Puppet is not needed because you've already denied a\
      \ 5-card major), or otherwise bid naturally. If partner has clubs,\
      \ they're likely to prefer notrump, but could also try a jump to " .+
        T.Bid 3 T.Spades .+ " to show that (which should be\
      \ surprising enough for you to recognize/remember)."
  in
    smpWrapN . return $ situation "2HAlt" action B.b1C2Halt explanation


twoNotrumpBalanced :: Situations
twoNotrumpBalanced = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
    explanation =
        "You've got very mild slam interest with 12" .+ NDash .+
        "13 HCP, but a balanced hand with no 5-card suit. Bid " .+
        T.Bid 2 T.Notrump .+ " to show this. Partner can try " .+
        T.Bid 3 T.Clubs .+ " as Stayman (regular-type! Puppet is\
      \ not needed because you've already denied a 5-card major), or otherwise\
      \ bid naturally. They're captain of the auction: they'll know whether to\
      \ sign off in game or investigate slam."
  in
    smpWrapN . return $ situation "2NAlt" action B.b1C2Nalt explanation


oneSpadeGF :: Situations
oneSpadeGF = let
    explanationMin =
        "You've got game-forcing strength and a 5-card spade suit. Start with\
      \ bidding " .+ T.Bid 1 T.Spades .+ " to describe this type\
      \ of hand. Partner will bid naturally, and when we find a fit, you'll\
      \ sign off in game to show no interest in going further (though partner\
      \ can always push on if they've got a monster)."
    explanationMax =
        "You've got a 5-card spade suit and at least mild slam interest. Start\
      \ with " .+ T.Bid 1 T.Spades .+ " to show partner we'e at\
      \ least game forcing. When you find a fit, start control bidding to show\
      \ partner you're interested in slam, too."
    sit (explanation, minHcp, maxHcp) = let
        action = do
            firstSeatOpener
            b1C
            oppsPass
            pointRange minHcp maxHcp
      in
        situation "1Sgf" action B.b1C1Sgf explanation
  in
    smpWrapN $ return sit <~ [(explanationMin, 8, 11), (explanationMax, 12, 40)]

twoSpades :: Situations
twoSpades = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
    explanation =
        "You've got at least mild slam interest with 12+ HCP, but an awkward\
      \ triple-four-one shape. Show this by bidding " .+
        T.Bid 2 T.Spades .+ ". Partner will relay to " .+ T.Bid 2 T.Notrump .+
       ", then bid your singleton. Partner will then either bid " .+
        T.Bid 3 T.Notrump .+ ", set trump at the 3 level (triggering a round\
      \ of control bidding), or  use " .+ T.Bid 4 T.Clubs .+ "/" .+
        T.Bid 4 T.Diamonds .+ "/RKC to tell us how high to go and\
      \ what suit is trump."
  in
    smpWrapN . return $ situation "2S" action B.b1C2S explanation


passGameSingleSuit :: Situations
passGameSingleSuit = let
    finalAction T.Clubs    = B.bP1C2C
    finalAction T.Diamonds = B.bP1C2D
    finalAction T.Hearts   = B.bP1C1H
    finalAction T.Spades   = B.bP1C1S
    finalAction _          = error "This should never happen"

    sit strain = let
        level = if strain `elem` T.majorSuits then 1 else 2
        bid = finalAction strain
        action = do
            forbid firstSeatOpener
            cannotPreempt
            makePass
            forbid firstSeatOpener
            oppsPass
            firstSeatOpener
            b1C
            oppsPass
            forEach (filter (/= strain) T.allSuits) (`maxSuitLength` 4)
        explanation =
            "You're game-forcing with a 5+ card suit. but you're a passed hand,\
          \ so all the slam bids have turned into game bids instead. Bid a\
          \ natural " .+ T.Bid level strain .+ ",\
          \ and we'll look for a trump fit from there. Partner's next bid is\
          \ a 5-card suit, and bids after that are 4+ cards."
      in
        situation "PG" action bid explanation
  in
    smpWrapS $ return sit <~ T.allSuits


passOneNotrump :: Situations
passOneNotrump = let
    action = do
        forbid firstSeatOpener
        cannotPreempt
        makePass
        forbid firstSeatOpener
        oppsPass
        firstSeatOpener
        b1C
        oppsPass
    explanation =
        "You're a passed hand with game-forcing strength but no 5-card suit.\
      \ Because you're a passed hand, the slam-interest bids are repurposed to\
      \ be merely game forcing. Bid a natural " .+ T.Bid 1 T.Notrump .+ ", and\
      \ we'll go from there. Stayman is on, but transfers are off (so the\
      \ stronger hand will be declarer more often)."
  in
    smpWrapS . return $ situation "P1N" action B.bP1C1N explanation


passTwoSpades :: Situations
passTwoSpades = let
    action = do
        forbid firstSeatOpener
        cannotPreempt
        makePass
        forbid firstSeatOpener
        oppsPass
        firstSeatOpener
        b1C
        oppsPass
    explanation =
        "You're a passed hand with game-forcing strength, but an awkward\
      \ triple-four-one shape. Show this by bidding " .+
        T.Bid 2 T.Spades .+ ". Partner will relay to " .+
        T.Bid 2 T.Notrump .+ ", then bid your singleton. Partner\
      \ will then either sign off in " .+ T.Bid 3 T.Notrump .+ ",\
      \ set trump at the 3 level (triggering a round of control bidding), or \
      \ use " .+ T.Bid 4 T.Clubs .+ "/" .+
        T.Bid 4 T.Diamonds .+ "/RKC to indicate how high to go\
      \ and which suit is trump."
  in
    smpWrapS . return $ situation "P2S" action B.bP1C2S explanation


-- TODO: figure out how two-suited hands show slam interest. Which suit do you
-- start with?


topic :: Topic
topic = makeTopic description "SMP1C" situations
  where
    description = "SMP immediate responses to " .+ T.Bid 1 T.Clubs .+
                  " openings"
    situations = wrap [ oneDiamond
                      , oneHeart  -- Differs from topicExtras, below
                      , oneNotrump  -- Differs from topicExtras, below
                      , slamSingleSuit  -- Differs from topicExtras, below
                      , twoSpades
                      , passGameSingleSuit
                      , passOneNotrump
                      , passTwoSpades
                      ]

topicExtras :: Topic
topicExtras = makeTopic description "SMP1CM" situations
  where
    description = "SMP modified immediate responses to " .+ T.Bid 1 T.Clubs .+
                  " openings"
    situations = wrap [ oneDiamond
                      , oneHeartNoSpades  -- Differs from topic, above
                      , oneSpadeGF  -- Differs from topic, above
                      , oneNotrumpAlt  -- Differs from topic, above
                      , slamSingleSuitModified  -- Differs from topic, above
                      , wrap [twoHeartsBalanced, twoNotrumpBalanced]  -- Differs
                      , twoSpades
                      , passGameSingleSuit
                      , passOneNotrump
                      , passTwoSpades
                      ]
