module Topics.MajorSuitRaises(topic) where

import qualified Bids.MajorSuitRaises as B
import CommonBids(setOpener, noInterference)
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapVulDlr, wrapVulNW, Situations, makeTopic)


simpleRaise :: Situations
simpleRaise = let
    sit (opening, response) = let
        action = do
            setOpener T.North
            _ <- opening
            noInterference $ T.suitBid opening
        explanation =
            "With at least 3-card support for partner's suit but no " .+
            "interest in game, raise partner's suit to the 2 level. " .+
            "This will likely be passed out, but gives partner the chance " .+
            "to show a huge hand, and also helps partner compete if the " .+
            "opponents interfere."
      in
        situation "simpR" action response explanation
  in
    wrapVulDlr $ return sit <~ [(B.b1H, B.b1H2H), (B.b1S, B.b1S2S)]


limitRaise :: Situations
limitRaise = let
    sit (opening, response) = let
        action = do
            setOpener T.North
            _ <- opening
            noInterference $ T.suitBid opening
        explanation =
            -- Don't mention needing at least 4-card support here: we're not
            -- assuming that we're playing 2/1, so might not fold the 3-card
            -- limit raises into the forcing 1N structure.
            "With support for partner's suit and " .+
            "invitational strength, make a limit raise by jumping in " .+
            "partner's suit. They will pass with a minimum, and bid game " .+
            "with extras."
      in
        situation "limtR" action response explanation
  in
    -- You should be an unpassed hand to make a limit raise; otherwise, consider
    -- using Drury.
    wrapVulNW $ return sit <~ [(B.b1H, B.b1H3H), (B.b1S, B.b1S3S)]


blast3N :: Situations
blast3N = let
    sit (opening, response) = let
        suit = T.suitBid opening
        action = do
            setOpener T.North
            _ <- opening
            noInterference suit
        explanation =
            "With 13" .+ NDash .+ "15 HCP, we probably want to be in game " .+
            "but not slam when partner opens the bidding. We've got an " .+
            "8-card fit in partner's major, but with our 4333 shape, " .+
            "we're unlikely to ruff anything in the short hand, so will " .+
            "likely take the same number of tricks in notrump as we " .+
            "would in " .+ show suit .+ ". Offer " .+ T.Bid 3 T.Notrump .+
            " to show this: partner can pass with a balanced minimum, " .+
            "correct to " .+ T.Bid 4 suit .+ " with an unbalanced minimum, " .+
            "and invesigate slam if they have a very strong hand."
      in
        situation "3N" action response explanation
  in
    -- You should be an unpassed hand to be game-forcing.
    wrapVulNW $ return sit <~ [(B.b1H, B.b1H3N), (B.b1S, B.b1S3N)]


topic :: Topic
topic = makeTopic "natural major-suit raises" "MajRai" situations
  where
    situations = wrap [ simpleRaise
                      , limitRaise
                      , blast3N
                      ]
