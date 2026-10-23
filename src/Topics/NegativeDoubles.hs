module Topics.NegativeDoubles(topic) where

import Action(Action)
import qualified Bids.NegativeDoubles as N
import qualified Bids.Overcalls as O
import CommonBids(setOpener)
import EDSL(makePass)
import Output(Description, (.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrapWeighted, wrapNW, wrapSE, Situations, makeTopic)


makeNegativeDouble :: String -> Description -> [(Action, Action, Action)] -> Situations
makeNegativeDouble ref explanation auctions = let
    sit (opening, overcall, answer) = let
        action = do
            setOpener T.North
            _ <- opening
            overcall
      in situation ref action answer explanation
  in
    -- North opens, East overcalls, and South is the unpassed responder whose
    -- choice the practice hand asks for.
    wrapNW $ return sit <~ auctions


bothMajorsAtOneLevel :: Situations
bothMajorsAtOneLevel = makeNegativeDouble "ndxMaj1"
    ("Partner opened a minor and RHO overcalled " .+ T.Bid 1 T.Diamonds .+
     ". Our negative double shows 6+ HCP and normally both 4-card majors. With a " .+
     "weak 5-4 hand, doubling shows both majors in one bid when we are too weak to " .+
     "comfortably rebid at the two level while seeking a fit. With only one major, bid " .+
     "it directly instead.")
    [ (O.b1C, O.b1Co1D, N.b1Co1DoX) ]


exactlyFourSpades :: Situations
exactlyFourSpades = makeNegativeDouble "ndx4S"
    ("After partner opens a minor and RHO overcalls " .+ T.Bid 1 T.Hearts .+
     ". Our negative double shows 6+ HCP and exactly four spades. Bid " .+
     T.Bid 1 T.Spades .+ " directly with five or more spades.")
    [ (O.b1C, O.b1Co1H, N.b1Co1HoX)
    , (O.b1D, O.b1Do1H, N.b1Do1HoX)
    ]


heartAfterSpadeOvercall :: Situations
heartAfterSpadeOvercall = makeNegativeDouble "ndxHeart"
    ("Partner opened a minor and RHO overcalled in spades. Our negative double " .+
     "shows 6+ HCP and at least four hearts. It is takeout, not a penalty " .+
     "double.")
    [ (O.b1C, O.b1Co1S, N.b1Co1SoX)
    , (O.b1D, O.b1Do1S, N.b1Do1SoX)
    ]


bothMinorsAfterSpadeOvercall :: Situations
bothMinorsAfterSpadeOvercall = makeNegativeDouble "ndxMinors"
    ("Partner opened " .+ T.Bid 1 T.Hearts .+ " and RHO overcalled in " .+
     "spades. Our negative double shows 6+ HCP and both minors. It is takeout, not " .+
     "a penalty double.")
    [ (O.b1H, O.b1Ho1S, N.b1Ho1SoX) ]


otherMajorAtTwoLevel :: Situations
otherMajorAtTwoLevel = makeNegativeDouble "ndxMaj2"
    ("Our negative double shows 9+ HCP and at least one unbid major: both " .+
     "majors, or one major with tolerance for opener's minor. With exactly one " .+
     "5-card major and 11+ HCP, bid it directly instead. Be prepared for " .+
     "opener's rebid if no fit appears.")
    [ (O.b1C, O.b1Co2D, N.b1Co2DoX)
    , (O.b1D, O.b1Do2C, N.b1Do2CoX)
    ]


heartPreemptDouble :: Situations
heartPreemptDouble = makeNegativeDouble "ndxPreemptHeart"
    ("Our negative double shows 9+ HCP, spades, and tolerance for opener's " .+
     "minor. With exactly one 5-card spade suit and 11+ HCP, bid " .+
     "2S directly instead. Opener bids 2S with a minimum fit or 3S with " .+
     "extras.")
    [ (O.b1C, O.b1Co2H, N.b1Co2HoX) ]


spadePreemptDouble :: Situations
spadePreemptDouble = makeNegativeDouble "ndxPreemptSpade"
    ("Our negative double shows 10+ HCP, hearts, and tolerance for opener's " .+
     "minor. It forces opener to choose 2NT or a three-level contract, so " .+
     "we need a hand prepared for that continuation. With 13+ HCP, we can be " .+
     "confident our side has game-forcing values.")
    [ (O.b1C, O.b1Co2S, N.b1Co2SoX)
    , (O.b1D, O.b1Do2S, N.b1Do2SoX)
    ]


majorPreemptDoubles :: Situations
majorPreemptDoubles = wrapWeighted
    [ (1, heartPreemptDouble)
    , (1, spadePreemptDouble)
    ]


-- South is opener in these continuations, so the prompt remains South's
-- choice even though we are practicing opener's second bid.
makeOpenerRebid :: String -> Description -> [(Action, Action)] -> Situations
makeOpenerRebid ref explanation rebids = let
    sit (opening, answer) = let
        action = do
            setOpener T.South
            _ <- opening
            _ <- O.b1Co1D
            _ <- N.b1Co1DoX
            makePass
      in situation ref action answer explanation
  in
    wrapSE $ return sit <~ rebids


fitRebids :: Situations
fitRebids = wrapWeighted
    [ (1, makeOpenerRebid "ndxFitMin" minimumExplanation
          [(O.b1C, N.b1Co1DoXo1H)])
    , (1, makeOpenerRebid "ndxFitInv" superacceptExplanation
          [(O.b1C, N.b1Co1DoXo2H)])
    , (1, makeOpenerRebid "ndxFitSuper" superacceptExplanation
          [(O.b1C, N.b1Co1DoXo3H)])
    , (1, makeOpenerRebid "ndxFitGame" gameExplanation
          [(O.b1C, N.b1Co1DoXo4H)])
    ]
  where
    minimumExplanation =
        "Partner's negative double has shown both majors, and we have a heart " .+
        "fit. With a minimum opening, bid hearts as cheaply as possible."
    superacceptExplanation =
        "Partner's negative double has shown both majors, and we have a heart " .+
        "fit. A jump in hearts is a superaccept showing extras; its exact range is a " .+
        "partnership agreement."
    gameExplanation =
        "Partner's negative double has shown both majors, and we have a heart " .+
        "fit. With 20--21 HCP, a direct game bid shows a monster hand. Partner can " .+
        "initiate slam exploration with extra values."


twoLevelFitRebids :: Situations
twoLevelFitRebids = let
    setup = do
        _ <- O.b1Co2D
        _ <- N.b1Co2DoXWithHearts
        makePass
    sit (answer, ref, explanation) = let
        action = do
            setOpener T.South
            _ <- O.b1C
            setup
      in situation ref action answer explanation
  in
    wrapSE $ return sit <~ [ (N.b1Co2DoXo2H, "ndxFit2Min", minimumExplanation)
                            , (N.b1Co2DoXo3H, "ndxFit2Super", superacceptExplanation)
                            ]
  where
    minimumExplanation =
        "Our " .+ T.Bid 2 T.Hearts .+ " bid shows a minimum opening and four " .+
        "hearts. Only partner knows whether hearts are their major, so they " .+
        "will decide whether we have found a fit."
    superacceptExplanation =
        "Partner's two-level negative double has shown at least one major; in " .+
        "this deal it includes hearts, and we have a fit. Bid " .+
        T.Bid 2 T.Hearts .+ " with a minimum; " .+
        "a jump to " .+ T.Bid 3 T.Hearts .+ " is a superaccept showing extras, " .+
        "with exact ranges by partnership agreement. It is riskier than a " .+
        "one-level superaccept, since partner might instead hold only spades."


responderBailouts :: Situations
responderBailouts = let
    sit (opening, overcall, negativeDouble, correction) = let
        action = do
            setOpener T.North
            _ <- opening
            _ <- overcall
            _ <- negativeDouble
            makePass
            -- Opener tries hearts, but doubler has only spades.
            _ <- N.b1Co2DoXo2H
            makePass
          in situation "ndxBail" action correction explanation
  in
    -- The auction is deliberately awaiting South's correction.
    wrapNW $ return sit <~
        [ (O.b1C, O.b1Co2D, N.b1Co2DoXWithSpades, N.b1Co2DoXr3C)
        , (O.b1D, O.b1Do2C, N.b1Do2CoXWithSpades, N.b1Do2CoXr3D)
        ]
  where
    explanation =
        "Our negative double showed 9+ HCP, spades, and tolerance for opener's " .+
        "minor. It did not show hearts. Correct to opener's minor. With a " .+
        "stopper in the " .+
        "opponents' suit, we can instead bid " .+
        T.Bid 2 T.Notrump .+ "."


twoLevelNotrumpRebids :: Situations
twoLevelNotrumpRebids = let
    setup = do
        _ <- O.b1Co2D
        _ <- N.b1Co2DoX
        makePass
    sit (answer, ref) = let
        action = do
            setOpener T.South
            _ <- O.b1C
            setup
      in situation ref action answer explanation
  in
    wrapSE $ return sit <~ [ (N.b1Co2DoXo2N, "ndx2NTMin")
                            , (N.b1Co2DoXo3N, "ndx2NTGame")
                            ]
  where
    explanation =
        "After the two-level overcall and negative double, there is no major " .+
        "fit. With a balanced hand and a diamond stopper, bid " .+
        T.Bid 2 T.Notrump .+ " with 12--14 HCP or " .+
        T.Bid 3 T.Notrump .+ " with 18--19 HCP. A balanced 15--17 HCP hand " .+
        "would have opened " .+ T.Bid 1 T.Notrump .+ "."


notrumpAndCueRebids :: Situations
notrumpAndCueRebids = makeOpenerRebid "ndxNT"
    ("There is no major fit. With a balanced hand and a diamond stopper, bid " .+
     "notrump at the level our strength suggests. With a strong balanced hand " .+
     "but no diamond stopper, cue-bid the opponents' suit instead. A balanced " .+
     "15--17 HCP hand would have opened " .+ T.Bid 1 T.Notrump .+ ".")
    [ (O.b1C, N.b1Co1DoXo1N)
    , (O.b1C, N.b1Co1DoXo2N)
    , (O.b1C, N.b1Co1DoXo2D)
    ]


openingSuitRebid :: Situations
openingSuitRebid = makeOpenerRebid "ndxRebid"
    ("With no major fit and no suitable notrump or cue-bid action, rebid the " .+
     "opening suit with six or more clubs, or with a good five-card club suit: " .+
     "two of the top three honors or three of the top five.")
    [ (O.b1C, N.b1Co1DoXo2C) ]


topic :: Topic
topic = makeTopic "negative doubles" "ndx" $
    wrapWeighted [ (1, bothMajorsAtOneLevel)
                 , (1, exactlyFourSpades)
                 , (1, heartAfterSpadeOvercall)
                 , (1, bothMinorsAfterSpadeOvercall)
                 , (1, otherMajorAtTwoLevel)
                 , (1, majorPreemptDoubles)
                 , (1, fitRebids)
                 , (1, twoLevelFitRebids)
                 , (1, responderBailouts)
                 , (1, twoLevelNotrumpRebids)
                 , (1, notrumpAndCueRebids)
                 , (1, openingSuitRebid)
                 ]
