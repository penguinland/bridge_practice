module Topics.TransfersOver1MX(topic) where

import qualified Bids.TransfersOver1MX as B
import CommonBids(setOpener)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapDlr, Situations, makeTopic)


signoff :: Situations
signoff = let
    sit (opener, double, response) = let
        action = do
            setOpener T.North
            _ <- opener
            double
        explanation =
            "Partner opened the bidding, and the next player made a " .+
            "takeout double. We have a very weak raise of partner's suit, " .+
            "so just bid it. Partner will know not to bid on unless " .+
            "they've got a monster hand."
      in situation "2M" action response explanation
  in
    wrapDlr $ return sit <~ [ (B.b1H, B.b1HoX, B.b1HoX2H)
                            , (B.b1S, B.b1SoX, B.b1SoX2S)
                            ]

-- Constructive raise of partner's suit
-- 1H-(X)-1S is natural
-- 1M-(X)-XX to punish
-- transfers
-- complete the transfer
-- pass completed transfer to sign off
-- rebid partner's suit to show invite
-- jump to game in partner's suit


topic :: Topic
topic = makeTopic "transfer responses over 1M-(X)" "1MXxfer" situations
  where
    situations = wrap [ signoff
                      , signoff
                      ]
