module Topics.TwoWayNewMinor(topic) where

import Action(Action)
import qualified Bids.StandardOpenings as O
import qualified Bids.TwoWayNewMinor as B
import CommonBids(noInterference, setOpener)
import EDSL(balancedHand, makeCall, pointRange)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, Situations, makeTopic, stdWrapNW, stdWrapSE,
             wrap, wrapNW)

-- Establish 1m--1M--1NT.  The detail of opener's natural 1NT rebid is not
-- the subject here; the topic begins once that auction is known.
setup :: T.Suit -> T.Suit -> T.Direction -> T.Direction -> Action
setup minor major opener _responder = do
    setOpener opener
    if minor == T.Clubs then O.b1C else O.b1D
    noInterference minor
    makeCall $ T.Bid 1 major
    noInterference minor
    balancedHand
    pointRange 12 14
    makeCall $ T.Bid 1 T.Notrump
    noInterference minor

twoClubs :: Situations
twoClubs = let
    sit (minor, major) = let
        action = setup minor major T.North T.South
        explanation =
            "After " .+ T.Bid 1 minor .+ ", " .+ T.Bid 1 major .+ ", and " .+
            T.Bid 1 T.Notrump .+ ", bid " .+ T.Bid 2 T.Clubs .+
            " with an invitational hand, or with a weak hand and long diamonds."
      in situation "2C" action B.responder2C explanation
  in wrapNW $ return sit <~ [(T.Clubs, T.Hearts), (T.Diamonds, T.Spades)]

twoDiamonds :: Situations
twoDiamonds = let
    sit (minor, major, other) = let
        action = setup minor major T.North T.South
        explanation =
            "Use " .+ T.Bid 2 T.Diamonds .+ " as the game-forcing new-minor " .+
            "inquiry when the hand's direction is still unclear: a five-card " .+
            "major, four cards in the other major, or 19+ HCP."
      in situation "2D" action (B.responder2D major other) explanation
  in wrapNW $ return sit <~ [(T.Clubs, T.Hearts, T.Spades),
                             (T.Diamonds, T.Spades, T.Hearts)]

forcedRelay :: Situations
forcedRelay = let
    action = do
        setup T.Clubs T.Hearts T.South T.North
        B.responder2C
        noInterference T.Clubs
    explanation =
        "Over the two-way " .+ T.Bid 2 T.Clubs .+ ", opener always bids " .+
        T.Bid 2 T.Diamonds .+ ". It is a forced relay, not a description of " .+
        "opener's hand."
  in stdWrapSE $ situation "relay" action B.opener2C2D explanation

openerReplies :: Situations
openerReplies = let
    replies minor major other = let
        sit reply = let
            action = do
                setup minor major T.South T.North
                B.responder2D major other
                noInterference minor
            explanation =
                "Over the game-forcing " .+ T.Bid 2 T.Diamonds .+ ", opener bids " .+
                "up the line: four cards in the other major, three-card support, " .+
                T.Bid 2 T.Notrump .+ " with nothing special, or the opened minor " .+
                "at the three level with five cards."
          in situation "reply" action reply explanation
      in wrap [ stdWrapSE (sit $ B.openerOtherMajor major other)
              , stdWrapSE (sit $ B.openerSupport major)
              , stdWrapSE (sit $ B.openerNothingSpecial major other minor)
              , stdWrapSE (sit $ B.openerFiveMinor major other minor)
              ]
  in wrap [ replies T.Clubs T.Hearts T.Spades
          , replies T.Diamonds T.Spades T.Hearts
          ]

weakContinuations :: Situations
weakContinuations = let
    afterOneNotrump minor major =
        setup minor major T.North T.South
    majorRebid = let
        action = afterOneNotrump T.Clubs T.Hearts
        explanation = "With a weak hand and five cards in our major, rebid it directly over opener's " .+ T.Bid 1 T.Notrump .+ "."
      in situation "weakM" action (B.weakMajorRebid T.Hearts) explanation
    spadesHearts = let
        action = afterOneNotrump T.Diamonds T.Spades
        explanation = "After responding " .+ T.Bid 1 T.Spades .+ ", a weak hand with at least five spades, at least four hearts, and more spades than hearts bids " .+ T.Bid 2 T.Hearts .+ " directly over opener's " .+ T.Bid 1 T.Notrump .+ "."
      in situation "weak54" action B.weakSpadesHearts explanation
    transfer = let
        action = afterOneNotrump T.Clubs T.Hearts
        explanation = "With a weak hand and at least six clubs, bid " .+ T.Bid 2 T.Notrump .+ " directly over " .+ T.Bid 1 T.Notrump .+ " to transfer opener to " .+ T.Bid 3 T.Clubs .+ " and sign off."
      in situation "weak2N" action B.weak2N explanation
    complete = let
        action = do
            afterOneNotrump T.Clubs T.Hearts
            B.weak2N
            noInterference T.Clubs
        explanation = "Complete responder's weak-hand transfer by bidding " .+ T.Bid 3 T.Clubs .+ "."
      in situation "weak3C" action B.complete2NTransfer explanation
  in wrap [stdWrapNW majorRebid, stdWrapNW spadesHearts, stdWrapNW transfer,
           stdWrapNW complete]

invitationalContinuation :: Situations
invitationalContinuation = let
    action = setup T.Clubs T.Hearts T.North T.South
    explanation =
        "After responding " .+ T.Bid 1 T.Hearts .+ ", bid " .+ T.Bid 2 T.Spades .+ " directly over opener's " .+ T.Bid 1 T.Notrump .+ " with an invitational 4-4 major-suit hand."
  in stdWrapNW $ situation "inv44" action B.invitational44Majors explanation

invitationalRebids :: Situations
invitationalRebids = let
    afterRelay minor major = do
        setup minor major T.North T.South
        B.responder2C
        noInterference minor
        B.opener2C2D
        noInterference minor
    sit ref minor major bid explanation =
        situation ref (afterRelay minor major) bid explanation
  in wrap [ stdWrapNW $ sit "inv5M" T.Clubs T.Hearts
                (B.invitationalMajor T.Hearts)
                "After the relay, rebid a five-card major to invite."
          , stdWrapNW $ sit "inv54" T.Diamonds T.Spades B.invitational54Majors
                "After responding one spade, show an invitational hand with at least five spades, at least four hearts, and more spades than hearts by bidding two hearts."
          , stdWrapNW $ sit "invFitM" T.Clubs T.Hearts
                (B.invitationalMinorFit T.Clubs)
                "With an invitational hand and a good fit for opener's minor, raise it to the three level after the relay."
          , stdWrapNW $ sit "inv2N" T.Diamonds T.Spades
                (B.invitational2N T.Spades T.Hearts)
                "With a balanced invitational hand and no further major-suit pattern, bid two notrump after the relay."
          , stdWrapNW $ sit "inv6M" T.Clubs T.Hearts
                (B.invitationalSixMajor T.Hearts)
                "A good six-card major invites by jumping to the three level after the relay."
          ]

openerAfterFiveMajor :: Situations
openerAfterFiveMajor = let
    auction = do
        setup T.Clubs T.Hearts T.South T.North
        B.responder2C
        noInterference T.Clubs
        B.opener2C2D
        noInterference T.Clubs
        B.invitationalMajor T.Hearts
        noInterference T.Clubs
    sit ref bid explanation = situation ref auction bid explanation
  in wrap [ stdWrapSE $ sit "fiveMPass" B.openerPass
                "With a minimum, pass responder's invitational five-card-major rebid."
          , stdWrapSE $ sit "fiveMGame" (B.openerGameMajor T.Hearts 3)
                "With a maximum and a heart fit, accept the invitation by bidding game."
          , stdWrapSE $ sit "fiveM2N" (B.opener2NMaximumNoFit [(T.Hearts, 2)])
                "With a maximum but no heart fit, bid two notrump."
          ]

openerAfter44Majors :: Situations
openerAfter44Majors = let
    auction = do
        setup T.Clubs T.Hearts T.South T.North
        B.invitational44Majors
        noInterference T.Clubs
    sit ref bid explanation = situation ref auction bid explanation
  in wrap [ stdWrapSE $ sit "44PassS" B.opener44PassSpades
                "With four spades and a minimum, pass in the known spade fit."
          , stdWrapSE $ sit "443H" B.opener44ThreeHearts
                "With a heart fit, a minimum, and no four-card spade fit, bid three hearts."
          , stdWrapSE $ sit "442N" B.opener44TwoNotrump
                "With a minimum and no major-suit fit, bid two notrump."
          , stdWrapSE $ sit "443N" B.opener44ThreeNotrump
                "With a maximum and no major-suit fit, bid three notrump."
          , stdWrapSE $ sit "44GameS" (B.openerGameMajor T.Spades 4)
                "With a maximum and four spades, bid four spades."
          , stdWrapSE $ sit "44GameH" B.opener44GameHearts
                "With a maximum, four hearts, and no four spades, bid four hearts."
          ]

openerAfter54Majors :: Situations
openerAfter54Majors = let
    auction = do
        setup T.Diamonds T.Spades T.South T.North
        B.responder2C
        noInterference T.Diamonds
        B.opener2C2D
        noInterference T.Diamonds
        B.invitational54Majors
        noInterference T.Diamonds
    sit ref bid explanation = situation ref auction bid explanation
  in wrap [ stdWrapSE $ sit "54TwoS" B.opener54TwoSpades
                "With two or more spades, at most three hearts, and a minimum, bid two spades."
          , stdWrapSE $ sit "54PassH" B.opener54PassHearts
                "With four hearts and a minimum, pass in hearts."
          , stdWrapSE $ sit "542N" (B.opener2NMaximumNoFit [(T.Spades, 2), (T.Hearts, 3)])
                "With a maximum and no major-suit fit, bid two notrump."
          , stdWrapSE $ sit "54GameS" (B.openerGameMajor T.Spades 2)
                "With a maximum and a spade fit, bid four spades."
          , stdWrapSE $ sit "54GameH" B.opener54GameHearts
                "With a maximum, a heart fit, and no spade fit, bid four hearts."
          ]

directGames :: Situations
directGames = let
    sit (major, bid) = let
        action = setup T.Clubs major T.North T.South
        explanation =
            "With a game-forcing hand and no slam interest, place the contract " .+
            "directly in game rather than using new minor."
      in situation "game" action bid explanation
  in wrapNW $ return sit <~ [(T.Hearts, B.responderGame T.Hearts),
                             (T.Spades, B.responderGame T.Spades),
                             (T.Hearts, B.responder3N T.Hearts)]

directSlams :: Situations
directSlams = let
    twoSuit = let
        action = setup T.Clubs T.Hearts T.North T.South
        explanation = "A direct three-level new suit shows slam interest and 5-5 shape."
      in situation "slam55" action (B.responderSlamTwoSuit T.Hearts T.Spades) explanation
    rebid = let
        action = setup T.Diamonds T.Spades T.North T.South
        explanation = "A direct three-level rebid of our major shows slam interest and six cards in the major."
      in situation "slam6M" action (B.responderSlamRebid T.Spades) explanation
  in wrap [stdWrapNW twoSuit, stdWrapNW rebid]

directNotrumpSlams :: Situations
directNotrumpSlams = let
    sit (bid, explanation) = let
        action = setup T.Clubs T.Hearts T.North T.South
      in situation "NTslam" action bid explanation
  in wrapNW $ return sit <~
       [ ( B.responder4N T.Hearts T.Spades
         , "With a balanced 19-20 HCP hand, no five-card heart suit, and no four-card spade suit, invite slam with four notrump."
         )
       , ( B.responder6N T.Hearts T.Spades
         , "With a balanced 21+ HCP hand, no five-card heart suit, and no four-card spade suit, bid six notrump directly."
         )
       ]

topic :: Topic
topic = makeTopic "two-way new minor" "2WNM" $
    wrap [twoClubs, twoDiamonds, forcedRelay, openerReplies, weakContinuations,
          invitationalContinuation, invitationalRebids, openerAfterFiveMajor,
          openerAfter44Majors, openerAfter54Majors, directGames, directSlams,
          directNotrumpSlams]
