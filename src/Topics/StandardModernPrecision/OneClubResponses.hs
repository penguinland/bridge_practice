module Topics.StandardModernPrecision.OneClubResponses(topic) where

import Output(output)
import Topic(Topic(..), wrap, Situations)
import Auction(withholdBid, forbid, maxSuitLength, makePass)
import Situation(situation, (<~))
import CommonBids(cannotPreempt)
import qualified Terminology as T
import Topics.StandardModernPrecision.BasicBids(firstSeatOpener, oppsPass, b1C, smpWrapN, smpWrapS)
import qualified Topics.StandardModernPrecision.Bids1C as B


oneDiamond :: Situations
oneDiamond = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
        withholdBid B.b1C1D
    explanation fmt =
        "When game might not be possible opposite a random 17 HCP, start\
      \ with " ++ output fmt (T.Bid 1 T.Diamonds) ++ ". This initiates MaFiA."
  in
    smpWrapN . return $ situation "1D" action B.b1C1D explanation


oneHeart :: Situations
oneHeart = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
        withholdBid B.b1C1H
    explanation fmt =
        "You've got a game-forcing hand but slam is unlikely. With 8 to 11 HCP,\
      \ bid " ++ output fmt (T.Bid 1 T.Hearts) ++ " to show this kind of hand.\
      \ Subsequent bids are natural 5-card suits (and later 4-card suits), not\
      \ MaFiA."
  in
    smpWrapN . return $ situation "1H" action B.b1C1H explanation


oneNotrump :: Situations
oneNotrump = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
        withholdBid B.b1C1N
    explanation fmt =
        "You've got at least mild slam interest with 12+ HCP, and a balanced\
      \ hand with no 5-card suit. Bid a natural " ++
        output fmt (T.Bid 1 T.Notrump) ++ ", and we'll\
      \ go from there. Stayman is on, but transfers are not."
  in
    smpWrapN . return $ situation "1N" action B.b1C1N explanation


slamSingleSuit :: Situations
slamSingleSuit = let
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
            mapM_ (`maxSuitLength` 4) . filter (/= strain) $ T.allSuits
            withholdBid bid
        explanation fmt =
            "You've got at least mild slam interest with 12+ HCP, and a 5+ card\
          \ suit. Bid a natural " ++ output fmt (T.Bid level strain) ++ ",\
          \ and we'll go from there. Once we find a trump fit, we'll start\
          \ control bidding. Subsequent bids are natural 5-card (and later\
          \ 4-card) suits, not MaFiA."
      in
        situation "Slam" action bid explanation
  in
    smpWrapN $ return sit <~ T.allSuits


twoSpades :: Situations
twoSpades = let
    action = do
        firstSeatOpener
        b1C
        oppsPass
        withholdBid B.b1C2S
    explanation fmt =
        "You've got at least mild slam interest with 12+ HCP, but an awkward\
      \ triple-four-one shape. Show this by bidding " ++
        output fmt (T.Bid 2 T.Spades) ++ ". Partner will relay to " ++
        output fmt (T.Bid 2 T.Notrump) ++ ", then bid your singleton. Partner\
      \ will then either bid " ++ output fmt (T.Bid 3 T.Notrump) ++ ",\
      \ set trump at the 3 level (triggering a round of control bidding), or \
      \ use " ++ output fmt (T.Bid 4 T.Clubs) ++ "/" ++
        output fmt (T.Bid 4 T.Diamonds) ++ "/RKC to tell us how high to go and\
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
            mapM_ (`maxSuitLength` 4) . filter (/= strain) $ T.allSuits
            withholdBid bid
        explanation fmt =
            "You're game-forcing with a 5+ card suit. but you're a passed hand,\
          \ so all the slam bids have turned into game bids instead. Bid a\
          \ natural " ++ output fmt (T.Bid level strain) ++ ",\
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
        withholdBid B.bP1C1N
    explanation fmt =
        "You're a passed hand with game-forcing strength but no 5-card suit.\
      \ Because you're a passed hand, the slam-interest bids are repurposed to\
      \ be merely game forcing. Bid a natural " ++
        output fmt (T.Bid 1 T.Notrump) ++ ", and we'll\
      \ go from there. Stayman is on, but transfers are off (so the stronger\
      \ hand will be declarer more often)."
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
        withholdBid B.bP1C2S
    explanation fmt =
        "You're a passed hand with game-forcing strength, but an awkward\
      \ triple-four-one shape. Show this by bidding " ++
        output fmt (T.Bid 2 T.Spades) ++ ". Partner will relay to " ++
        output fmt (T.Bid 2 T.Notrump) ++ ", then bid your singleton. Partner\
      \ will then either sign off in " ++ output fmt (T.Bid 3 T.Notrump) ++ ",\
      \ set trump at the 3 level (triggering a round of control bidding), or \
      \ use " ++ output fmt (T.Bid 4 T.Clubs) ++ "/" ++
        output fmt (T.Bid 4 T.Diamonds) ++ "/RKC to indicate how high to go\
      \ and which suit is trump."
  in
    smpWrapS . return $ situation "P2S" action B.bP1C2S explanation


-- TODO: figure out how two-suited hands show slam interest. Which suit do you
-- start with?


topic :: Topic
topic = Topic "SMP immediate responses to 1C openings" "SMP1C" situations
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
