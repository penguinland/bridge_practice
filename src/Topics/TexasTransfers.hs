module Topics.TexasTransfers(topic) where

import qualified Bids.OneNotrump as B
import CommonBids(setOpener)
-- TODO: replace makePass with something more intelligent
import EDSL(makePass, makeCall, suitLength, minSuitLength, maxSuitLength,
            pointRange)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapVulNW, wrapVulSE, Situations, makeTopic)


makeTransferSignoff :: Situations
makeTransferSignoff = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            makePass
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
    wrapVulNW $ return sit <~ [B.b1N4D, B.b1N4H]


makeTransferSlam :: Situations
makeTransferSlam = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            makePass
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
    wrapVulNW $ return sit <~ [B.b1N4D, B.b1N4H]


completeTransfer :: Situations
completeTransfer = let
    sit (partnerBid, ourBid) = let
        action = do
            setOpener T.South
            B.b1N
            makePass
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
    wrapVulSE $ return sit <~ [(B.b1N4D, B.b1N4D4H), (B.b1N4H, B.b1N4H4S)]


completeTransferDoubleton :: Situations
completeTransferDoubleton = let
    sit (partnerBid, ourBid) = let
        action = do
            setOpener T.South
            B.b1N
            makePass
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
    wrapVulSE $ return sit <~ [(B.b1N4D, B.b1N4D4H), (B.b1N4H, B.b1N4H4S)]


-- WARNING: This situation is rare, and typically requires generating 100,000
-- boards. Use it sparingly.
completeTransferSuperfit :: Situations
completeTransferSuperfit = let
    sit (partnerBid, ourBid) = let
        action = do
            setOpener T.South
            B.b1N
            makePass
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
    wrapVulSE $ return sit <~ [(B.b1N4D, B.b1N4D4H), (B.b1N4H, B.b1N4H4S)]


transferSlamInvite :: Situations
transferSlamInvite = let
    sit (bid, suit) = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            minSuitLength suit 6
            B.slamInvite
        explanation =
            "Partner has opened " .+ T.Bid 1 T.Notrump .+ ", and we've " .+
            "got a 6-card major and a slam invite. Make a Jacoby transfer, " .+
            "then raise to game. If partner is a maximum, they'll " .+
            "investigate slam, and with a minimum, they'll pass."
        in situation "SInv" action bid explanation
  in
    wrapVulNW $ return sit <~ [(B.b1N2D, T.Hearts), (B.b1N2H, T.Spades)]


transferSlamInviteDeclined :: Situations
transferSlamInviteDeclined = let
    sit (transferBid, acceptBid, raiseBid) = let
        action = do
            setOpener T.South
            B.b1N
            makePass
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
    wrapVulSE $ return sit <~ [(B.b1N2D, B.b1N2D2H, B.b1N2D2H4H)
                              ,(B.b1N2H, B.b1N2H2S, B.b1N2H2S4S)]


transferSlamInviteAccepted :: Situations
transferSlamInviteAccepted = let
    sit (transferBid, acceptBid, raiseBid) = let
        action = do
            setOpener T.South
            B.b1N
            makePass
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
    wrapVulSE $ return sit <~ [(B.b1N2D, B.b1N2D2H, B.b1N2D2H4H)
                              ,(B.b1N2H, B.b1N2H2S, B.b1N2H2S4S)]


-- TODO: More situations:
-- 6-4 in majors: Stayman, then Texas if necessary (from both perspectives)
-- Texas after interference (from both perspectives, special-case the cue bids)


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
                      ]
