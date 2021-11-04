module Topics.StandardModernPrecision.ResponsesToStrongClub(topic) where

import Output(output)
import Topic(Topic(..), wrap, Situations)
import Auction(withholdBid, makePass)
import Situation(situation)--, base, (<~))
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
      \ with " ++ output fmt (T.Bid 1 T.Diamonds) ++ ". This is the start of\
      \ MaFiA."
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
      \ Subsequent bids are natural 5-card suits, not MaFiA."
  in
    B.smpWrapN $ situation "1H" action (T.Bid 1 T.Hearts) explanation


oneSpade :: Situations
oneSpade = let
    action = do
        B.firstSeatOpener
        B.b1C
        cannotPreempt
        makePass
        withholdBid B.b1C1S
    explanation fmt =
        "You've got at least mild slam interest with 12+ HCP, and a 5+ card\
      \ major. Bid a natural " ++ output fmt (T.Bid 1 T.Spades) ++ ", and we'll\
      \ go from there. Once we find a trump fit, we'll start control bidding.\
      \ Subsequent bids are natural 5-card suits, not MaFiA."
  in
    B.smpWrapN $ situation "1S" action (T.Bid 1 T.Spades) explanation


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


topic :: Topic
topic = Topic "SMP immediate responses to 1C" "SMP1C" situations
  where
    situations = wrap [ oneDiamond
                      , oneHeart
                      , oneSpade
                      , oneNotrump
                      , twoSpades
{-
                      , twoClubs
                      , twoDiamonds
                      , twoHearts
                      , passOneHeart
                      , passOneSpade
                      , passOneNotrump
                      , passTwoClubs
                      , passTwoDiamonds
                      , passTwoSpades
-}
                      ]
