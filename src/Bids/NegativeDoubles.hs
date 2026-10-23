module Bids.NegativeDoubles(
    b1Co1DoX
  , b1Co1HoX
  , b1Co1SoX
  , b1Do1HoX
  , b1Do1SoX
  , b1Ho1SoX
  , b1Co2DoX
  , b1Co2DoXWithHearts
  , b1Co2DoXWithSpades
  , b1Co2DoXr3C
  , b1Co2HoX
  , b1Co2SoX
  , b1Do2CoX
  , b1Do2CoXWithSpades
  , b1Do2CoXr3D
  , b1Do2SoX
  , b1Co1DoXo1H
  , b1Co1DoXo2H
  , b1Co1DoXo3H
  , b1Co1DoXo4H
  , b1Co2DoXo2H
  , b1Co2DoXo3H
  , b1Co2DoXo2N
  , b1Co2DoXo3N
  , b1Co1DoXo1N
  , b1Co1DoXo2N
  , b1Co1DoXo2D
  , b1Co1DoXo2C
) where

import Action(Action)
import EDSL(makeCall, minSuitLength, maxSuitLength, pointRange, suitLength,
            balancedHand, hasStopper, hasTopN, alternatives, forbid,
            nameAction)
import qualified Terminology as T


-- A negative double is takeout, not penalty.  It has no upper strength limit:
-- with a strong hand, responder doubles first and describes the strength later.
negativeDouble :: String -> Int -> Action -> Action
negativeDouble name minimumPoints shape = nameAction name $ do
    pointRange minimumPoints 40
    shape
    makeCall T.Double


-- In these teaching deals, responder may tolerate opener's minor for a
-- possible correction, but does not have a second long suit of their own.
negativeDoubleOverMinor :: String -> Int -> T.Suit -> Action -> Action
negativeDoubleOverMinor name minimumPoints openerSuit shape =
    negativeDouble name minimumPoints $ do
        maxSuitLength openerSuit 4
        shape


bothMajors :: Action
bothMajors = do
    minSuitLength T.Hearts 4
    maxSuitLength T.Hearts 5
    minSuitLength T.Spades 4
    maxSuitLength T.Spades 5


-- For the basic 1C-(1D)-X lesson, deal the normal 4-4 pattern.  A weak
-- 5-4 hand can also sensibly double when a direct bid would make the later
-- auction awkward, so retain it as an occasional exception without making
-- more distributional two-suiters routine practice cases.
bothMajorsAtOneLevel :: Action
bothMajorsAtOneLevel = alternatives
    [ suitLength T.Hearts 4 >> suitLength T.Spades 4
    , pointRange 6 8 >> suitLength T.Hearts 5 >> suitLength T.Spades 4
    , pointRange 6 8 >> suitLength T.Hearts 4 >> suitLength T.Spades 5
    ]


fourSpades :: Action
fourSpades = suitLength T.Spades 4


fourHearts :: Action
fourHearts = do
    minSuitLength T.Hearts 4
    maxSuitLength T.Hearts 5
    -- With 11+ HCP and five hearts, a one-level heart overcall is available
    -- and describes the hand more directly than a negative double.
    forbid $ do
        pointRange 11 40
        minSuitLength T.Hearts 5


bothMinors :: Action
bothMinors = do
    minSuitLength T.Clubs 4
    minSuitLength T.Diamonds 4
    -- With 11+ HCP and only one five-card minor, bid that minor directly.
    -- Keep the double for hands that genuinely need one call to show both.
    forbid $ do
        pointRange 11 40
        alternatives
            [ minSuitLength T.Clubs 5 >> maxSuitLength T.Diamonds 4
            , minSuitLength T.Diamonds 5 >> maxSuitLength T.Clubs 4
            ]



-- At a two-level overcall, a responder with 11+ HCP and one 5-card major
-- should bid that major directly when it is available.  The negative-double
-- one-major branches therefore cover four-card majors, or hands too weak for
-- the direct two-level bid.
oneMajorAtTwoLevel :: T.Suit -> T.Suit -> Action
oneMajorAtTwoLevel major otherMajor = do
    minSuitLength major 4
    maxSuitLength major 5
    maxSuitLength otherMajor 3
    forbid $ do
        pointRange 11 40
        minSuitLength major 5


-- The two auctions where an opponent overcalls a red suit have special,
-- precise meanings.  Over 1C-(1D), double promises both majors; over
-- 1C/1D-(1H), it promises exactly four spades.
b1Co1DoX :: Action
b1Co1DoX = negativeDoubleOverMinor "neg1C1D" 6 T.Clubs bothMajorsAtOneLevel

b1Co1HoX :: Action
b1Co1HoX = negativeDoubleOverMinor "neg1C1H" 6 T.Clubs fourSpades

b1Do1HoX :: Action
b1Do1HoX = negativeDoubleOverMinor "neg1D1H" 6 T.Diamonds fourSpades


-- In the remaining one-level sequences, the double emphasizes the other
-- major, or (after 1H-(1S)) both unbid minors.
b1Co1SoX :: Action
b1Co1SoX = negativeDoubleOverMinor "neg1C1S" 6 T.Clubs fourHearts

b1Do1SoX :: Action
b1Do1SoX = negativeDoubleOverMinor "neg1D1S" 6 T.Diamonds fourHearts

b1Ho1SoX :: Action
b1Ho1SoX = negativeDouble "neg1H1S" 6 $ do
    bothMinors
    -- With three-card support, responder raises opener's hearts instead.
    maxSuitLength T.Hearts 2


-- At the two level over a minor, the double either promises both majors or
-- exactly one major plus tolerance for opener's minor. Keep the alternatives
-- distinct in generated hands, rather than giving doubler all three suits.
twoLevelMajorChoice :: T.Suit -> Action
twoLevelMajorChoice openerSuit = alternatives
    [ bothMajors >> maxSuitLength openerSuit 2
    , oneMajorAtTwoLevel T.Hearts T.Spades >>
        minSuitLength openerSuit 3
    , oneMajorAtTwoLevel T.Spades T.Hearts >>
        minSuitLength openerSuit 3
    ]


twoLevelHeartChoice :: T.Suit -> Action
twoLevelHeartChoice openerSuit = alternatives
    [ bothMajors >> maxSuitLength openerSuit 2
    , oneMajorAtTwoLevel T.Hearts T.Spades >>
        minSuitLength openerSuit 3
    ]


-- Used in responder continuations where opener tries hearts but doubler has
-- only spades.  Four-card minor support makes the three-level correction a
-- reasonable resting place even opposite a short-minor opening.
twoLevelSpadeChoice :: T.Suit -> Action
twoLevelSpadeChoice openerSuit = do
    oneMajorAtTwoLevel T.Spades T.Hearts
    minSuitLength openerSuit 4


-- A one-club opening can be short.  At the two level, doubler therefore
-- needs a credible escape if opener cannot find the indicated major.
clubBailout :: T.Suit -> Action
clubBailout opponentSuit = alternatives
    [ minSuitLength T.Clubs 4
    , hasStopper opponentSuit
    ]


-- It needs roughly 9--10 HCP; we use 9 as the lower bound here.
b1Co2DoX :: Action
b1Co2DoX = negativeDoubleOverMinor "neg1C2D" 9 T.Clubs $ do
    twoLevelMajorChoice T.Clubs
    clubBailout T.Diamonds


-- Used when opener is going to show a heart fit after the double.
b1Co2DoXWithHearts :: Action
b1Co2DoXWithHearts = negativeDoubleOverMinor "neg1C2D_hearts" 9 T.Clubs $ do
    twoLevelHeartChoice T.Clubs
    clubBailout T.Diamonds


b1Co2DoXWithSpades :: Action
b1Co2DoXWithSpades = negativeDoubleOverMinor "neg1C2D_spades" 9 T.Clubs $
    twoLevelSpadeChoice T.Clubs


b1Co2DoXr3C :: Action
b1Co2DoXr3C = nameAction "neg1C2D_correct3C" $ do
    minSuitLength T.Clubs 4
    makeCall (T.Bid 3 T.Clubs)


-- Over a two-heart weak preempt, the negative double shows the unbid major.
b1Co2HoX :: Action
b1Co2HoX = negativeDoubleOverMinor "neg1C2H" 9 T.Clubs $ do
    oneMajorAtTwoLevel T.Spades T.Hearts
    minSuitLength T.Clubs 3
    clubBailout T.Hearts


-- A two-spade preempt leaves no economical major response.  The double
-- therefore needs more values and asks opener to choose between hearts and
-- the original minor.
b1Co2SoX :: Action
b1Co2SoX = negativeDoubleOverMinor "neg1C2S" 10 T.Clubs $ do
    minSuitLength T.Hearts 4
    maxSuitLength T.Hearts 5
    minSuitLength T.Clubs 3
    clubBailout T.Spades

b1Do2CoX :: Action
b1Do2CoX = negativeDoubleOverMinor "neg1D2C" 9 T.Diamonds $
    twoLevelMajorChoice T.Diamonds


b1Do2CoXWithSpades :: Action
b1Do2CoXWithSpades = negativeDoubleOverMinor "neg1D2C_spades" 9 T.Diamonds $
    twoLevelSpadeChoice T.Diamonds


b1Do2CoXr3D :: Action
b1Do2CoXr3D = nameAction "neg1D2C_correct3D" $ do
    minSuitLength T.Diamonds 4
    makeCall (T.Bid 3 T.Diamonds)


b1Do2SoX :: Action
b1Do2SoX = negativeDoubleOverMinor "neg1D2S" 10 T.Diamonds $ do
    minSuitLength T.Hearts 4
    maxSuitLength T.Hearts 5
    minSuitLength T.Diamonds 3


-- Opener's rebids after 1C-(1D)-X-(P).  North's double has promised both
-- majors, so opener first looks for a four-card major fit.
openerRebid :: String -> Int -> Int -> Action -> T.Call -> Action
openerRebid name low high shape call = nameAction name $ do
    pointRange low high
    shape
    makeCall call


noMajorFit :: Action
noMajorFit = do
    maxSuitLength T.Hearts 3
    maxSuitLength T.Spades 3


b1Co1DoXo1H :: Action
b1Co1DoXo1H = openerRebid "neg_rebid_1H" 12 14
    (minSuitLength T.Hearts 4) (T.Bid 1 T.Hearts)

b1Co1DoXo2H :: Action
b1Co1DoXo2H = openerRebid "neg_rebid_2H" 15 17
    (minSuitLength T.Hearts 4) (T.Bid 2 T.Hearts)

b1Co1DoXo3H :: Action
b1Co1DoXo3H = openerRebid "neg_rebid_3H" 18 19
    (minSuitLength T.Hearts 4) (T.Bid 3 T.Hearts)

b1Co1DoXo4H :: Action
b1Co1DoXo4H = openerRebid "neg_rebid_4H" 20 21
    (minSuitLength T.Hearts 4) (T.Bid 4 T.Hearts)


-- After a two-level overcall, the cheapest heart fit is 2H. A jump to 3H is
-- a superaccept with extras; we do not jump to game without a guaranteed fit.
b1Co2DoXo2H :: Action
b1Co2DoXo2H = openerRebid "neg_rebid_2H_after_2D" 12 14
    (minSuitLength T.Hearts 4) (T.Bid 2 T.Hearts)

b1Co2DoXo3H :: Action
b1Co2DoXo3H = openerRebid "neg_rebid_3H_after_2D" 15 17
    (do
        suitLength T.Hearts 4
        maxSuitLength T.Spades 3
    ) (T.Bid 3 T.Hearts)


-- With no major fit after a two-level overcall, notrump still needs a stopper
-- in the overcalled suit. A balanced 15--17 hand would have opened 1NT.
b1Co2DoXo2N :: Action
b1Co2DoXo2N = openerRebid "neg_rebid_2N_after_2D" 12 14 shape
    (T.Bid 2 T.Notrump)
  where
    shape = balancedHand >> noMajorFit >> hasStopper T.Diamonds

b1Co2DoXo3N :: Action
b1Co2DoXo3N = openerRebid "neg_rebid_3N_after_2D" 18 19 shape
    (T.Bid 3 T.Notrump)
  where
    shape = balancedHand >> noMajorFit >> hasStopper T.Diamonds


-- With no major fit, notrump describes a balanced hand with a diamond
-- stopper. A balanced 15--17 hand would instead have opened 1NT, leaving
-- 1NT for 12--14 and 2NT for 18--19 after a 1C opening.
b1Co1DoXo1N :: Action
b1Co1DoXo1N = openerRebid "neg_rebid_1N" 12 14 shape
    (T.Bid 1 T.Notrump)
  where
    shape = balancedHand >> noMajorFit >> hasStopper T.Diamonds

b1Co1DoXo2N :: Action
b1Co1DoXo2N = openerRebid "neg_rebid_2N" 18 19 shape
    (T.Bid 2 T.Notrump)
  where
    shape = balancedHand >> noMajorFit >> hasStopper T.Diamonds


-- A cue-bid shows the strong balanced hand that would bid notrump, except
-- that it lacks a stopper in the opponents' diamonds.
b1Co1DoXo2D :: Action
b1Co1DoXo2D = openerRebid "neg_rebid_2D" 18 19 shape
    (T.Bid 2 T.Diamonds)
  where
    shape = balancedHand >> noMajorFit >> forbid (hasStopper T.Diamonds)


-- Without a major fit, opener may rebid the opening clubs with six or more,
-- or with a good five-card suit: two of the top three or three of the top five.
-- Keep these practice hands near that threshold: a suit with four top-five
-- honors is also rebiddable, but is too obviously powerful to teach the choice.
b1Co1DoXo2C :: Action
b1Co1DoXo2C = openerRebid "neg_rebid_2C" 12 19 shape
    (T.Bid 2 T.Clubs)
  where
    goodFive = do
        minSuitLength T.Clubs 5
        alternatives [hasTopN T.Clubs 3 2, hasTopN T.Clubs 5 3]
    shape = do
        noMajorFit
        forbid $ hasTopN T.Clubs 5 4
        alternatives [minSuitLength T.Clubs 6, goodFive]
