module Topics.StandardModernPrecision.ResponsesToStrongClub(topic) where

import Output(output)
import Topic(Topic(..), wrap, Situations)
import Auction(withholdBid, forbid, makePass, maxSuitLength)
import Situation(situation, base, (<~))
import CommonBids(cannotPreempt)
import qualified Terminology as T
import qualified Topics.StandardModernPrecision.Bids as B


oneDiamond :: Situations
oneDiamond = let
    action = do
        B.firstSeatOpener
        B.b1C
        cannotPreempt
        makePass
        withholdBid B.b1C1D
    explanation fmt =
        "When game might not be possible opposite a random 17 HCP, start\
      \ with " ++ output fmt (T.Bid 1 T.Diamonds) ++ ". This initiates MaFiA."
  in
    B.smpWrapN $ situation "1D" action (T.Bid 1 T.Diamonds) explanation


oneHeart :: Situations
oneHeart = let
    action = do
        B.firstSeatOpener
        B.b1C
        cannotPreempt
        makePass
        withholdBid B.b1C1H
    explanation fmt =
        "You've got a game-forcing hand but slam is unlikely. With 8 to 11 HCP,\
      \ bid " ++ output fmt (T.Bid 1 T.Hearts) ++ " to show this kind of hand.\
      \ Subsequent bids are natural 5-card suits (and later 4-card suits), not\
      \ MaFiA."
  in
    B.smpWrapN $ situation "1H" action (T.Bid 1 T.Hearts) explanation


oneNotrump :: Situations
oneNotrump = let
    action = do
        B.firstSeatOpener
        B.b1C
        cannotPreempt
        makePass
        withholdBid B.b1C1N
    explanation fmt =
        "You've got at least mild slam interest with 12+ HCP, and a balanced\
      \ hand with no 5-card suit. Bid a natural " ++
        output fmt (T.Bid 1 T.Notrump) ++ ", and we'll\
      \ go from there. Systems are on, even though this might wrong-side the\
      \ contract: better to be familiar and easy to remember than right-side\
      \ it, at least until we're more practiced with SMP."
  in
    B.smpWrapN $ situation "1N" action (T.Bid 1 T.Notrump) explanation


slamSingleSuit :: Situations
slamSingleSuit = let
    finalAction T.Clubs    = B.b1C2C
    finalAction T.Diamonds = B.b1C2D
    finalAction T.Hearts   = B.b1C2H
    finalAction T.Spades   = B.b1C1S
    finalAction _          = error "This should never happen"

    sit strain = let
        level = if strain == T.Spades then 1 else 2
        action = do
            B.firstSeatOpener
            B.b1C
            cannotPreempt
            makePass
            sequence_ . map (flip maxSuitLength 4) . filter (/= strain) $
                T.allSuits
            withholdBid . finalAction $ strain
        explanation fmt =
            "You've got at least mild slam interest with 12+ HCP, and a 5+ card\
          \ suit. Bid a natural " ++ output fmt (T.Bid level strain) ++ ",\
          \ and we'll go from there. Once we find a trump fit, we'll start\
          \ control bidding. Subsequent bids are natural 5-card (and later\
          \ 4-card) suits, not MaFiA."
      in
        situation "Slam" action (T.Bid level strain) explanation
  in
    wrap $ base sit <~ T.allSuits <~ T.allVulnerabilities <~ [T.North]


twoSpades :: Situations
twoSpades = let
    action = do
        B.firstSeatOpener
        B.b1C
        cannotPreempt
        makePass
        withholdBid B.b1C2S
    explanation fmt =
        "You've got at least mild slam interest with 12+ HCP, but an awkward\
      \ triple four one shape. Show this by bidding " ++
        output fmt (T.Bid 2 T.Spades) ++ ". Partner will relay to " ++
        output fmt (T.Bid 2 T.Notrump) ++ ", then bid your singleton. Partner\
      \ will then either set trump for us to start control bidding, or use " ++
        output fmt (T.Bid 4 T.Clubs) ++ "/" ++
        output fmt (T.Bid 4 T.Diamonds) ++ "/RKC."
  in
    B.smpWrapN $ situation "2S" action (T.Bid 2 T.Spades) explanation


passGameSingleSuit :: Situations
passGameSingleSuit = let
    finalAction T.Clubs    = B.bP1C2C
    finalAction T.Diamonds = B.bP1C2D
    finalAction T.Hearts   = B.bP1C1H
    finalAction T.Spades   = B.bP1C1S
    finalAction _          = error "This should never happen"

    sit strain = let
        level = if any (== strain) T.majorSuits then 1 else 2
        action = do
            forbid B.firstSeatOpener
            makePass
            forbid B.firstSeatOpener
            cannotPreempt
            makePass
            B.firstSeatOpener
            B.b1C
            cannotPreempt
            makePass
            sequence_ . map (flip maxSuitLength 4) . filter (/= strain) $
                T.allSuits
            withholdBid . finalAction $ strain
        explanation fmt =
            "You're game-forcing with a 5+ card suit. but you're a passed hand,\
          \ so all the slam bids have turned into game bids instead. Bid a\
          \ natural " ++ output fmt (T.Bid level strain) ++ ",\
          \ and we'll look for a trump fit from there. Partner's next bid is\
          \ a 5-card suit, and bids after that are 4+ cards."
      in
        situation "PG" action (T.Bid level strain) explanation
  in
    wrap $ base sit <~ T.allSuits <~ T.allVulnerabilities <~ [T.South]


passOneNotrump :: Situations
passOneNotrump = let
    action = do
        forbid B.firstSeatOpener
        makePass
        forbid B.firstSeatOpener
        cannotPreempt
        makePass
        B.firstSeatOpener
        B.b1C
        cannotPreempt
        makePass
        withholdBid B.bP1C1N
    explanation fmt =
        "You're a passed hand with game-forcing strength but no 5-card suit.\
      \ Because you're a passed hand, the slam-interest bids are repurposed to\
      \ be merely game forcing. Bid a natural " ++
        output fmt (T.Bid 1 T.Notrump) ++ ", and we'll\
      \ go from there. Systems are on, even though this will wrong-side the\
      \ contract: better to be familiar and easy to remember than right-side\
      \ it, at least until we're more practiced with SMP."
  in
    B.smpWrapS $ situation "P1N" action (T.Bid 1 T.Notrump) explanation


passTwoSpades :: Situations
passTwoSpades = let
    action = do
        forbid B.firstSeatOpener
        makePass
        forbid B.firstSeatOpener
        cannotPreempt
        makePass
        B.firstSeatOpener
        B.b1C
        cannotPreempt
        makePass
        withholdBid B.bP1C2S
    explanation fmt =
        "You're a passed hand with game-forcing strength, but an awkward\
      \ triple four one shape. Show this by bidding " ++
        output fmt (T.Bid 2 T.Spades) ++ ". Partner will relay to " ++
        output fmt (T.Bid 2 T.Notrump) ++ ", then bid your singleton. Partner\
      \ will then either set trump for us to start control bidding, or use " ++
        output fmt (T.Bid 4 T.Clubs) ++ "/" ++
        output fmt (T.Bid 4 T.Diamonds) ++ "/RKC."
        -- TODO: not sure this explanation is right; revisit it after you
        -- understand 4C/4D/RKC correctly
  in
    B.smpWrapS $ situation "P2S" action (T.Bid 2 T.Spades) explanation


-- TODO: figure out how two-suited hands show slam interest. Which suit do you
-- start with?


topic :: Topic
topic = Topic "SMP immediate responses to 1C" "SMP1C" situations
  where
    situations = wrap [ oneDiamond
                      , oneHeart
                      , oneNotrump
                      , slamSingleSuit
                      , twoSpades
                      , passGameSingleSuit
                      , passOneNotrump
                      , passTwoSpades
                      ]
