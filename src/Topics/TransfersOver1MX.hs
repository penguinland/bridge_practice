module Topics.TransfersOver1MX(topic) where

import Action(Action)
import qualified Bids.TransfersOver1MX as B
import CommonBids(setOpener)
import EDSL(nameAction, pointRange, forEach, maxSuitLength, minLoserCount,
            makeCall)
import Output((.+), Punct(..))
import Situation(Situation, situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapDlr, Situations, makeTopic)


advancerPasses :: Action
advancerPasses = nameAction "advancer_passes" $ do
    pointRange 0 8
    forEach T.allSuits (`maxSuitLength` 5)
    minLoserCount 8
    makeCall T.Pass


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


constrRaise :: Situations
constrRaise = let
    sit (opener, double, response) = let
        action = do
            setOpener T.North
            _ <- opener
            double
        explanation =
            "Partner opened the bidding, and the next player made a " .+
            "takeout double. We have a constructive raise of partner's " .+
            "suit, so transfer into it. Partner will likely just complete " .+
            "the transfer, but might bid higher if they've got extras."
      in situation "2Mm1" action response explanation
  in
    wrapDlr $ return sit <~ [ (B.b1H, B.b1HoX, B.b1HoX2D)
                            , (B.b1S, B.b1SoX, B.b1SoX2H)
                            ]


constrRaiseCompleted :: Situations
constrRaiseCompleted = let
    sit :: (Action, Action, Action, Action) -> T.Direction -> T.Vulnerability ->
        Situation
    sit (opener, double, response, rebid) = let
        action = do
            setOpener T.South
            _ <- opener
            _ <- double
            _ <- response
            advancerPasses
        explanation =
            "We opened the bidding, and the next player made a " .+
            "takeout double. Partner then transferred to our suit, showing " .+
            "a constructive raise. Complete the transfer, and treat the " .+
            "auction as though it started " .+ opener .+ NDash .+ rebid .+
            ". Unless you've got a huge hand, aim to stop in partscore."
      in situation "2Mm1C" action rebid explanation
  in
    wrapDlr $ return sit <~ [ (B.b1H, B.b1HoX, B.b1HoX2D, B.b1HoX2D2H)
                            , (B.b1S, B.b1SoX, B.b1SoX2H, B.b1SoX2H2S)
                            ]


-- 1H-(X)-1S is natural
-- 1M-(X)-XX to punish
-- transfers for signoff
-- transfers for limit raise
-- complete the transfer
-- pass completed transfer to sign off
-- rebid partner's suit to show invite
-- jump to game in partner's suit


topic :: Topic
topic = makeTopic "transfer responses over 1M-(X)" "1MXxfer" situations
  where
    situations = wrap [ signoff
                      , constrRaise
                      , constrRaiseCompleted
                      ]
