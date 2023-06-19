module Topics.OneNotrump(texasTransfers) where

import Auction(makePass)  -- TODO: replace this with something more intelligent
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


texasTransfers :: Topic
texasTransfers = makeTopic "Texas Transfers" "TexTr" situations
  where
    situations = wrap [ makeTransferSignoff
                      --, makeTransferSlam
                      --, completeTransfer
                      ]