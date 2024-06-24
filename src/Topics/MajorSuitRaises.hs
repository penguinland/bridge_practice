module Topics.MajorSuitRaises(topic) where

import Output((.+))
import Topic(Topic, wrap, wrapVulDlr, Situations, makeTopic)
import Auction(Action, makePass, alternatives, pointRange, constrain)
import Situation(situation, (<~))
import qualified Terminology as T
import qualified Topics.BidsMajorSuitRaises as B
import CommonBids(setOpener, cannotPreempt)


noInterference :: Action
noInterference = do
    cannotPreempt
    -- Building out the entire overcall structure just so we can forbid
    -- any of it here is too complicated for now. Let's just say either
    -- the opponent has at most 14 HCP, and either doesn't have a 5-card
    -- suit or has at most 7 HCP.
    pointRange 0 14
    alternatives [
        constrain "no_five_card_suit" [
            "shape(", ", any 4441, any 4333, any 4432)"]
      , pointRange 0 7]
    makePass


simpleRaise :: Situations
simpleRaise = let
    sit (opening, response) = let
        action = do
            setOpener T.North
            _ <- opening
            noInterference
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
            noInterference
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
    wrap $ return sit <~ [(B.b1H, B.b1H3H), (B.b1S, B.b1S3S)]
                      <~ T.allVulnerabilities
                      <~ [T.West, T.North]


blast3N :: Situations
blast3N = let
    sit (opening, response, suit) = let
        action = do
            setOpener T.North
            _ <- opening
            noInterference
        explanation =
            "With 13-15 HCP, you probably want to be in game but not slam " .+
            "when partner opens the bidding. You've got an 8-card fit in " .+
            "partner's major, but with your 4333 shape, you're unlikely to " .+
            "ruff anything in the short hand, so will likely take the same " .+
            "number of tricks in notrump as you would in " .+ show suit .+
            ". Offer " .+ T.Bid 3 T.Notrump .+ " to show this: partner can " .+
            "pass with a balanced minimum, correct to " .+ T.Bid 4 suit .+
            " with an unbalanced minimum, and invesigate slam if they have " .+
            "a very strong hand."
      in
        situation "3N" action response explanation
  in
    -- You should be an unpassed hand to be game-forcing.
    wrap $ return sit
        <~ [(B.b1H, B.b1H3N, T.Hearts), (B.b1S, B.b1S3N, T.Spades)]
        <~ T.allVulnerabilities
        <~ [T.West, T.North]


topic :: Topic
topic = makeTopic "natural major-suit raises" "MajRai" situations
  where
    situations = wrap [ simpleRaise
                      , limitRaise
                      , blast3N
                      ]
