module Topics.TexasTransfers(topic) where

import Control.Monad(when)

import qualified Bids.Cappelletti as Capp
import qualified Bids.DONT as DONT
import qualified Bids.Meckwell as MW
import qualified Bids.NaturalOneNotrumpDefense as Nat
import qualified Bids.OneNotrump as B
import CommonBids(setOpener)
import EDSL(makePass, makeCall, suitLength, minSuitLength, maxSuitLength,
            pointRange)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapWeighted, wrapNW, wrapSE, Situations, makeTopic)


makeTransferSignoff :: Situations
makeTransferSignoff = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            B.gameNoSlam
        explanation =
            "Partner has opened " .+ T.Bid 1 T.Notrump .+ ", and we've " .+
            "got a 6-card major and a game-forcing hand with no interest " .+
            "in slam. Make a Texas Transfer by bidding 1 below our suit. " .+
            "Partner will complete the transfer by bidding our suit, and " .+
            "then we can pass. We're guaranteed a trump fit because " .+
            "partner should have at least a doubleton in every suit."
        in situation "SO" action bid explanation
  in
    -- Optimization to make the code faster: it is exceedingly rare for South to
    -- make a Texas Transfer as a passed hand (to be unable to open the bidding
    -- on the Rule of 20 with a 6-card suit and 10+ HCP, we'd need 6332 shape
    -- with exactly 10 HCP). It's not impossible: it happens once every couple
    -- hundred thousand hands. To speed up program execution, we focus only on
    -- the times when South is an unpassed hand.
    wrapNW $ return sit <~ [B.b1N4D, B.b1N4H]


makeTransferSlam :: Situations
makeTransferSlam = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            B.slamInterest
        explanation =
            "Partner has opened " .+ T.Bid 1 T.Notrump .+ ", and we've " .+
            "got a 6-card major and definite slam interest. " .+
            "Make a Texas Transfer by bidding 1 below our suit. " .+
            "Partner will complete the transfer by bidding our suit, and " .+
            "then we can investigate slam with whatever conventions you " .+
            "and partner have agreed on. We're guaranteed a trump fit " .+
            "because partner should have at least a doubleton in every suit."
        in situation "SI" action bid explanation
  in
    -- Note that South cannot be a passed hand and have interest in slam.
    wrapNW $ return sit <~ [B.b1N4D, B.b1N4H]


completeTransfer :: Situations
completeTransfer = let
    sit (partnerBid, ourBid) = let
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            _ <- partnerBid  -- Make the linter happy
            makePass
        explanation =
            "We have opened " .+ T.Bid 1 T.Notrump .+ ", and partner has " .+
            "made a Texas Transfer. Complete the transfer, and see what " .+
            "they do next."
        in situation "comp" action ourBid explanation
  in
    -- Same optimization here: don't have North make a Texas Transfer as a
    -- passed hand.
    wrapSE $ return sit <~ [(B.b1N4D, B.b1N4D4H), (B.b1N4H, B.b1N4H4S)]


completeTransferDoubleton :: Situations
completeTransferDoubleton = let
    sit (partnerBid, ourBid) = let
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            _ <- partnerBid  -- Make the linter happy
            makePass
            suitLength (T.suitBid ourBid) 2
        explanation =
            "We have opened " .+ T.Bid 1 T.Notrump .+ ", and partner has " .+
            "made a Texas Transfer. Complete the transfer, and see what " .+
            "they do next. Even if we only have a doubleton, partner is " .+
            "promising a 6-card suit, so we've got a fit."
        in situation "comp2" action ourBid explanation
  in
    -- Same optimization here: don't have North make a Texas Transfer as a
    -- passed hand.
    wrapSE $ return sit <~ [(B.b1N4D, B.b1N4D4H), (B.b1N4H, B.b1N4H4S)]


-- WARNING: This situation is rare, and typically requires generating 100,000
-- boards. Use it sparingly.
completeTransferSuperfit :: Situations
completeTransferSuperfit = let
    sit (partnerBid, ourBid) = let
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            _ <- partnerBid  -- Make the linter happy
            makePass
            suitLength (T.suitBid ourBid) 5
        explanation =
            "We have opened " .+ T.Bid 1 T.Notrump .+ ", and partner has " .+
            "made a Texas Transfer. Complete the transfer, and see what " .+
            "they do next. Even if we know we have an 11-card fit, " .+
            "partner might only have 10 HCP. Leave it up to them whether " .+
            "to investigate slam."
        in situation "comp5" action ourBid explanation
  in
    -- Same optimization here: don't have North make a Texas Transfer as a
    -- passed hand.
    wrapSE $ return sit <~ [(B.b1N4D, B.b1N4D4H), (B.b1N4H, B.b1N4H4S)]


transferSlamInvite :: Situations
transferSlamInvite = let
    sit (bid, suit) = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            minSuitLength suit 6
            B.slamInvite
        explanation =
            "Partner has opened " .+ T.Bid 1 T.Notrump .+ ", and we've " .+
            "got a 6-card major and a slam invite. Make a Jacoby transfer, " .+
            "then raise to game. If partner is a maximum, they'll " .+
            "investigate slam, and with a minimum, they'll pass."
        in situation "SInv" action bid explanation
  in
    wrapNW $ return sit <~ [(B.b1N2D, T.Hearts), (B.b1N2H, T.Spades)]


transferSlamInviteDeclined :: Situations
transferSlamInviteDeclined = let
    sit (transferBid, acceptBid, raiseBid) = let
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            _ <- transferBid
            makePass
            _ <- acceptBid
            makePass
            _ <- raiseBid
            makePass
            pointRange 15 15
            maxSuitLength (T.suitBid acceptBid) 3
        explanation =
            "We opened " .+ T.Bid 1 T.Notrump .+ ", partner made a Jacoby " .+
            "transfer, and after we completed it, partner raised to game " .+
            "without even checking if we like the suit. That's a slam " .+
            "invite: with a minimum hand and no extra trump length, pass."
        in situation "SInvDec" action makePass explanation
  in
    wrapSE $ return sit <~ [(B.b1N2D, B.b1N2D2H, B.b1N2D2H4H)
                           ,(B.b1N2H, B.b1N2H2S, B.b1N2H2S4S)]


transferSlamInviteAccepted :: Situations
transferSlamInviteAccepted = let
    sit (transferBid, acceptBid, raiseBid) = let
        action = do
            setOpener T.South
            B.b1N
            B.noInterference
            _ <- transferBid
            makePass
            _ <- acceptBid
            makePass
            _ <- raiseBid
            makePass
            pointRange 17 17
            minSuitLength (T.suitBid acceptBid) 3
        explanation =
            "We opened " .+ T.Bid 1 T.Notrump .+ ", partner made a Jacoby " .+
            "transfer, and after we completed it, partner raised to game " .+
            "without even checking if we like the suit. That's a slam " .+
            "invite: with a maximum hand and extra trump length, accept the " .+
            "invite by investigating slam (if your partnership uses " .+
            "something other than " .+ T.Bid 4 T.Notrump .+ " to " .+
            "invistigate slam, your preferred bid might differ)."
        in situation "SInvAcc" action (makeCall $ T.Bid 4 T.Notrump) explanation
  in
    wrapSE $ return sit <~ [(B.b1N2D, B.b1N2D2H, B.b1N2D2H4H)
                           ,(B.b1N2H, B.b1N2H2S, B.b1N2H2S4S)]


transferOverInterference :: Situations
transferOverInterference = let
    sit (overcall, transferBid) = let
        action = do
            setOpener T.North
            B.b1N
            overcall
        explanation =
            "Partner opened " .+ T.Bid 1 T.Notrump .+ ", and the next " .+
            "player interfered. As long as they only bid at the 2 or 3 " .+
            "level, Texas transfers are still on!"
        in situation "int" action transferBid explanation
  in
    wrapNW $ return sit <~ [ (Nat.b1No2C,  B.b1N4D)
                           , (Nat.b1No2D,  B.b1N4D)
                           -- Don't make a transfer to the opponent's suit!
                           --, (Nat.b1No2H,  B.b1N4D)
                           , (Nat.b1No2S,  B.b1N4D)
                           , (MW.b1NoX,    B.b1N4D)
                           , (MW.b1No2C,   B.b1N4D)
                           , (MW.b1No2D,   B.b1N4D)
                           --, (MW.b1No2H,   B.b1N4D)
                           , (MW.b1No2S,   B.b1N4D)
                           , (MW.b1No2N,   B.b1N4D)
                           -- If partner has at least 15 HCPs and we have at
                           -- least 10, the opponents aren't strong enough to
                           -- make a penalty double.
                           --, (Capp.b1NoX,  B.b1N4D)
                           , (Capp.b1No2C, B.b1N4D)
                           --, (Capp.b1No2D, B.b1N4D)
                           --, (Capp.b1No2H, B.b1N4D)
                           , (Capp.b1No2S, B.b1N4D)
                           , (Capp.b1No2N, B.b1N4D)
                           , (DONT.b1NoX,  B.b1N4D)
                           , (DONT.b1No2C, B.b1N4D)
                           , (DONT.b1No2D, B.b1N4D)
                           --, (DONT.b1No2H, B.b1N4D)
                           , (DONT.b1No2S, B.b1N4D)
                           , (DONT.b1No3C, B.b1N4D)
                           , (DONT.b1No3D, B.b1N4D)
                           --, (DONT.b1No3H, B.b1N4D)
                           , (DONT.b1No3S, B.b1N4D)
                           , (Nat.b1No2C,  B.b1N4H)
                           , (Nat.b1No2D,  B.b1N4H)
                           , (Nat.b1No2H,  B.b1N4H)
                           --, (Nat.b1No2S,  B.b1N4H)
                           , (MW.b1NoX,    B.b1N4H)
                           , (MW.b1No2C,   B.b1N4H)
                           , (MW.b1No2D,   B.b1N4H)
                           , (MW.b1No2H,   B.b1N4H)
                           --, (MW.b1No2S,   B.b1N4H)
                           , (MW.b1No2N,   B.b1N4H)
                           -- Again, not enough points in the deck for a penalty
                           -- double here.
                           --, (Capp.b1NoX,  B.b1N4H)
                           , (Capp.b1No2C, B.b1N4H)
                           --, (Capp.b1No2D, B.b1N4H)
                           , (Capp.b1No2H, B.b1N4H)
                           --, (Capp.b1No2S, B.b1N4H)
                           , (Capp.b1No2N, B.b1N4H)
                           , (DONT.b1NoX,  B.b1N4H)
                           , (DONT.b1No2C, B.b1N4H)
                           , (DONT.b1No2D, B.b1N4H)
                           --, (DONT.b1No2H, B.b1N4H)
                           --, (DONT.b1No2S, B.b1N4H)
                           , (DONT.b1No3C, B.b1N4H)
                           , (DONT.b1No3D, B.b1N4H)
                           , (DONT.b1No3H, B.b1N4H)
                           --, (DONT.b1No3S, B.b1N4H)
                           ]


completeTransferOverInterference :: Situations
completeTransferOverInterference = let
    sit (overcall, transferBid, completedTransfer) = let
        action = do
            setOpener T.South
            B.b1N
            _ <- overcall
            _ <- transferBid
            makePass
        explanation =
            "We opened " .+ T.Bid 1 T.Notrump .+ ", and the next " .+
            "player interfered. As long as they only bid at the 2 or 3 " .+
            "level, Texas transfers are still on! Complete partner's transfer."
        in situation "intComp" action completedTransfer explanation
  in
    wrapSE $ return sit <~ [ (Nat.b1No2C,  B.b1N4D, B.b1N4D4H)
                           , (Nat.b1No2D,  B.b1N4D, B.b1N4D4H)
                           -- No transfers into the opponent's suit!
                           --, (Nat.b1No2H,  B.b1N4D, B.b1N4D4H)
                           , (Nat.b1No2S,  B.b1N4D, B.b1N4D4H)
                           , (MW.b1NoX,    B.b1N4D, B.b1N4D4H)
                           , (MW.b1No2C,   B.b1N4D, B.b1N4D4H)
                           , (MW.b1No2D,   B.b1N4D, B.b1N4D4H)
                           --, (MW.b1No2H,   B.b1N4D, B.b1N4D4H)
                           , (MW.b1No2S,   B.b1N4D, B.b1N4D4H)
                           , (MW.b1No2N,   B.b1N4D, B.b1N4D4H)
                           , (Capp.b1NoX,  B.b1N4D, B.b1N4D4H)
                           , (Capp.b1No2C, B.b1N4D, B.b1N4D4H)
                           --, (Capp.b1No2D, B.b1N4D, B.b1N4D4H)
                           --, (Capp.b1No2H, B.b1N4D, B.b1N4D4H)
                           , (Capp.b1No2S, B.b1N4D, B.b1N4D4H)
                           , (Capp.b1No2N, B.b1N4D, B.b1N4D4H)
                           , (DONT.b1NoX,  B.b1N4D, B.b1N4D4H)
                           , (DONT.b1No2C, B.b1N4D, B.b1N4D4H)
                           , (DONT.b1No2D, B.b1N4D, B.b1N4D4H)
                           --, (DONT.b1No2H, B.b1N4D, B.b1N4D4H)
                           , (DONT.b1No2S, B.b1N4D, B.b1N4D4H)
                           , (DONT.b1No3C, B.b1N4D, B.b1N4D4H)
                           , (DONT.b1No3D, B.b1N4D, B.b1N4D4H)
                           --, (DONT.b1No3H, B.b1N4D, B.b1N4D4H)
                           , (DONT.b1No3S, B.b1N4D, B.b1N4D4H)
                           , (Nat.b1No2C,  B.b1N4H, B.b1N4H4S)
                           , (Nat.b1No2D,  B.b1N4H, B.b1N4H4S)
                           , (Nat.b1No2H,  B.b1N4H, B.b1N4H4S)
                           --, (Nat.b1No2S,  B.b1N4H, B.b1N4H4S)
                           , (MW.b1NoX,    B.b1N4H, B.b1N4H4S)
                           , (MW.b1No2C,   B.b1N4H, B.b1N4H4S)
                           , (MW.b1No2D,   B.b1N4H, B.b1N4H4S)
                           , (MW.b1No2H,   B.b1N4H, B.b1N4H4S)
                           --, (MW.b1No2S,   B.b1N4H, B.b1N4H4S)
                           , (MW.b1No2N,   B.b1N4H, B.b1N4H4S)
                           , (Capp.b1NoX,  B.b1N4H, B.b1N4H4S)
                           , (Capp.b1No2C, B.b1N4H, B.b1N4H4S)
                           --, (Capp.b1No2D, B.b1N4H, B.b1N4H4S)
                           , (Capp.b1No2H, B.b1N4H, B.b1N4H4S)
                           --, (Capp.b1No2S, B.b1N4H, B.b1N4H4S)
                           , (Capp.b1No2N, B.b1N4H, B.b1N4H4S)
                           , (DONT.b1NoX,  B.b1N4H, B.b1N4H4S)
                           , (DONT.b1No2C, B.b1N4H, B.b1N4H4S)
                           , (DONT.b1No2D, B.b1N4H, B.b1N4H4S)
                           --, (DONT.b1No2H, B.b1N4H, B.b1N4H4S)
                           --, (DONT.b1No2S, B.b1N4H, B.b1N4H4S)
                           , (DONT.b1No3C, B.b1N4H, B.b1N4H4S)
                           , (DONT.b1No3D, B.b1N4H, B.b1N4H4S)
                           , (DONT.b1No3H, B.b1N4H, B.b1N4H4S)
                           --, (DONT.b1No3S, B.b1N4H, B.b1N4H4S)
                           ]


completeTransferOverInterferenceCuebid :: Situations
completeTransferOverInterferenceCuebid = let
    sit (overcall, transferBid, completedTransfer) isShort = let
        action = do
            setOpener T.South
            B.b1N
            _ <- overcall
            _ <- transferBid
            makePass
            when isShort $ suitLength (T.suitBid completedTransfer) 2
        explanation =
            "We opened " .+ T.Bid 1 T.Notrump .+ ", and the next " .+
            "player interfered. As long as they only bid at the 2 or 3 " .+
            "level, Texas transfers are still on! Partner's bid is a " .+
            "Texas transfer, not a cue bid: complete the transfer" .+
            (if isShort then ", even if we're short in the suit" else "") .+
            "."
        in situation "intCue" action completedTransfer explanation
  in
    wrapSE $ return sit <~ [ (DONT.b1No3D, B.b1N4D, B.b1N4D4H)
                           , (DONT.b1No3H, B.b1N4H, B.b1N4H4S)
                           ]
                        <~ [True, False]


-- TODO: More situations:
-- 6-4 in majors: Stayman, then Texas if necessary (from both perspectives)


topic :: Topic
topic = makeTopic "Texas transfers" "TexTr" situations
  where
    situations = wrap [ makeTransferSignoff
                      , makeTransferSlam
                      , completeTransfer
                      -- Make the unusual variants rarer than the common ones.
                      , wrap [ completeTransferDoubleton
                             , completeTransferSuperfit
                             , transferSlamInvite
                             , transferSlamInviteDeclined
                             , transferSlamInviteAccepted
                             ]
                      , wrapWeighted [
                            (2, transferOverInterference)
                          , (2, completeTransferOverInterference)
                          , (1, completeTransferOverInterferenceCuebid)
                          ]
                      ]
