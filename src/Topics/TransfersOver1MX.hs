module Topics.TransfersOver1MX(topic) where

import Action(Action)
import qualified Bids.TransfersOver1MX as B
import CommonBids(setOpener)
import EDSL(nameAction, pointRange, forEach, suitLength, maxSuitLength,
            minLoserCount, makeCall)
import Output((.+), Punct(..))
import Situation(Situation, situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapDlr, wrapNW, stdWrap, Situations, makeTopic)


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


initiateSignoff :: Situations
initiateSignoff = let
    sit (opener, double, response, suit) = let
        action = do
            setOpener T.North
            _ <- opener
            _ <- double
            maxSuitLength suit 2
        explanation =
            "Partner opened the bidding, and the next player made a " .+
            "takeout double. We have a weak hand with a long suit, the " .+
            "kind where we had planned to just bid a forcing " .+
            T.Bid 1 T.Notrump .+ ", then bid our suit as a signoff " .+
            "(unless partner showed a very strong hand). Instead, " .+
            "transfer into our suit, and plan to pass when partner " .+
            "completes the transfer. If they don't complete it because " .+
            "they're unexpectedly strong, we'll figure out something else."
      in situation "initSO" action response explanation
  in
    -- Subtle point: we must be an unpassed hand, because we would have bid a
    -- weak 2 in most of these positions.
    wrapNW $ return sit <~ [ (B.b1H, B.b1HoX, B.b1HoX1N, T.Hearts)
                           , (B.b1H, B.b1HoX, B.b1HoX2C, T.Hearts)
                           , (B.b1S, B.b1SoX, B.b1SoX1N, T.Spades)
                           , (B.b1S, B.b1SoX, B.b1SoX2C, T.Spades)
                           , (B.b1S, B.b1SoX, B.b1SoX2D, T.Spades)
                           ]


initiateLimitRaise :: Situations
initiateLimitRaise = let
    sit (opener, double, response, suit) = let
        action = do
            setOpener T.North
            _ <- opener
            _ <- double
            suitLength suit 3
        explanation =
            "Partner opened the bidding, and the next player made a " .+
            "takeout double. We have 3-card support for partner's " .+
            "suit, at least invitational strength, and a decent side suit. " .+
            "Transfer into the side suit, planning to rebid partner's suit " .+
            "when they complete the transfer (rebid at the 2 level for a " .+
            "limit raise, higher for a game force). If advancer preempts, " .+
            "partner will know to lead our side suit, and we'll get off to " .+
            "a good start on defense."
      in situation "initLR" action response explanation
  in
    wrapDlr $ return sit <~ [ (B.b1H, B.b1HoX, B.b1HoX1N, T.Hearts)
                            , (B.b1H, B.b1HoX, B.b1HoX2C, T.Hearts)
                            , (B.b1S, B.b1SoX, B.b1SoX1N, T.Spades)
                            , (B.b1S, B.b1SoX, B.b1SoX2C, T.Spades)
                            , (B.b1S, B.b1SoX, B.b1SoX2D, T.Spades)
                            ]


completeTransfer :: Situations
completeTransfer = let
    sit (opener, double, response, rebid) = let
        action = do
            setOpener T.South
            _ <- opener
            _ <- double
            _ <- response
            advancerPasses
        explanation =
            "We opened the bidding, and the next player made a " .+
            "takeout double. Partner then made a transfer into a new suit. " .+
            "Unless we have a very strong hand and no tolerance for " .+
            "partner's suit, we should complete the transfer. Either this " .+
            "is the only suit partner can play in, or they will bid again " .+
            "afterwards to show 3-card support for our major."
      in situation "complT" action rebid explanation
  in
    wrapDlr $ return sit <~ [ (B.b1H, B.b1HoX, B.b1HoX1N, B.b1HoX1N2C)
                            , (B.b1H, B.b1HoX, B.b1HoX2C, B.b1HoX2C2D)
                            , (B.b1S, B.b1SoX, B.b1SoX1N, B.b1SoX1N2C)
                            , (B.b1S, B.b1SoX, B.b1SoX2C, B.b1SoX2C2D)
                            , (B.b1S, B.b1SoX, B.b1SoX2D, B.b1SoX2D2H)
                            ]


oneSpadeNatural :: Situations
oneSpadeNatural = let
    sit = let
        action = do
            setOpener T.North
            B.b1H
            B.b1HoX
        explanation =
            "Partner opened the bidding, and the next player made a " .+
            "takeout double. We can bid a natural " .+ B.b1HoX1S .+ " to " .+
            "show our suit. Remember that the transfers only start at " .+
            T.Bid 1 T.Notrump .+ " as a transfer to clubs!"
      in situation "complT" action B.b1HoX1S explanation
  in
    stdWrap sit


redouble :: Situations
redouble = let
    sit (opener, double, response) = let
        action = do
            setOpener T.North
            _ <- opener
            double
        explanation =
            "Partner opened the bidding, and the next player made a " .+
            "takeout double. We don't have an obvious suit to bid, but " .+
            "do have enough points that this is our hand. Redouble to tell " .+
            "partner not to let the opponents steal the contract. Maybe " .+
            "partner has more to tell us about their own hand, or maybe " .+
            "we defend a doubled contract and punish the opponents for " .+
            "stepping out too far."
      in situation "rdbl" action response explanation
  in
    wrapDlr $ return sit <~ [ (B.b1H, B.b1HoX, B.b1HoXXX)
                            , (B.b1S, B.b1SoX, B.b1SoXXX)
                            ]


-- TODO:
-- don't complete the transfer with a void and extras
-- pass completed transfer to sign off
-- rebid partner's suit to show invite
-- jump to game in partner's suit


topic :: Topic
topic = makeTopic topicName "1MXxfer" situations
  where
    topicName = "transfer responses over 1M" .+ NDash .+ "(X)"
    situations = wrap [ signoff
                      , constrRaise
                      , constrRaiseCompleted
                      , initiateSignoff
                      , initiateLimitRaise
                      , completeTransfer
                      , oneSpadeNatural
                      , redouble
                      ]
