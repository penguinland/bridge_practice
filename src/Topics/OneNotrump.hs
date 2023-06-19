module Topics.OneNotrump(texasTransfers) where

-- TODO: replace makePass with something more intelligent
import Auction(makePass, suitLength)
import CommonBids(setOpener)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, --wrapVulDlr,
 Situations, makeTopic)
import qualified Topics.BidsOneNotrump as B


makeTransferSignoff :: Situations
makeTransferSignoff = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.gameNoSlam
        explanation =
            "Partner has opened " .+ T.Bid 1 T.Notrump .+ ", and you've " .+
            "got a 6-card major and a game-forcing hand with no interest " .+
            "in slam. Make a Texas Transfer by bidding 1 below your suit. " .+
            "Partner will complete the transfer by bidding your suit, and " .+
            "then you can pass. You're guaranteed a trump fit because " .+
            "partner should have at least a doubleton in every suit."
        in situation "SO" action bid explanation
  in
    -- Optimization to make the code faster: it is exceedingly rare for South to
    -- make a Texas Transfer as a passed hand (to be unable to open the bidding
    -- on the Rule of 20 with a 6-card suit and 10+ HCP, you'll need 6332 shape
    -- with exactly 10 HCP). It's not impossible: it happens once every couple
    -- hundred thousand hands. To speed up program execution, we focus only on
    -- the times when South is an unpassed hand.
    wrap $ return sit <~ [B.b1N4D, B.b1N4H] <~ T.allVulnerabilities
                      <~ [T.North, T.West]


makeTransferSlam :: Situations
makeTransferSlam = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            B.slamInterest
        explanation =
            "Partner has opened " .+ T.Bid 1 T.Notrump .+ ", and you've " .+
            "got a 6-card major and slam interest. " .+
            "Make a Texas Transfer by bidding 1 below your suit. " .+
            "Partner will complete the transfer by bidding your suit, and " .+
            "then you can investigate slam with whatever systems you and " .+
            "partner have agreed on. You're guaranteed a trump fit because " .+
            "partner should have at least a doubleton in every suit."
        in situation "SI" action bid explanation
  in
    -- Note that South cannot be a passed hand and have interest in slam.
    wrap $ return sit <~ [B.b1N4D, B.b1N4H] <~ T.allVulnerabilities
                      <~ [T.North, T.West]


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
    wrap $ return sit <~ [(B.b1N4D, B.b1N4D4H), (B.b1N4H, B.b1N4H4S)]
                      <~ T.allVulnerabilities <~ [T.South, T.East]


completeTransferDoubleton :: Situations
completeTransferDoubleton = let
    sit (partnerBid, ourBid, suit) = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            _ <- partnerBid  -- Make the linter happy
            makePass
            suitLength suit 2
        explanation =
            "We have opened " .+ T.Bid 1 T.Notrump .+ ", and partner has " .+
            "made a Texas Transfer. Complete the transfer, and see what " .+
            "they do next. Even if we only have a doubleton, partner is " .+
            "promising a 6-card suit, so we've got a fit."
        in situation "comp2" action ourBid explanation
  in
    -- Same optimization here: don't have North make a Texas Transfer as a
    -- passed hand.
    wrap $ return sit <~ [ (B.b1N4D, B.b1N4D4H, T.Hearts)
                         , (B.b1N4H, B.b1N4H4S, T.Spades)]
                      <~ T.allVulnerabilities <~ [T.South, T.East]


texasTransfers :: Topic
texasTransfers = makeTopic "Texas Transfers" "TexTr" situations
  where
    situations = wrap [ makeTransferSignoff
                      , makeTransferSlam
                      , completeTransfer
                      , completeTransferDoubleton
                      ]
