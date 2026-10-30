module Bids.TwoWayNewMinor(
    responder2C, responder2D, responderGame, responder3N, responder4N,
    responder6N, responderSlamTwoSuit,
    responderSlamRebid, opener2C2D, openerOtherMajor, openerSupport,
    openerNothingSpecial, openerFiveMinor, weakMajorRebid, weakSpadesHearts,
    weak2N, complete2NTransfer, invitational44Majors, invitationalMajor,
    invitational54Majors, invitationalMinorFit, invitational2N,
    invitationalSixMajor, openerPass, opener2NMaximumNoFit,
    openerGameMajor, opener44PassSpades, opener44ThreeHearts,
    opener44TwoNotrump, opener44ThreeNotrump, opener54TwoSpades,
    opener54PassHearts, opener44GameHearts, opener54GameHearts
) where

import Action(Action)
import EDSL(alternatives, balancedHand, forbid, forbidAll, makeAlertableCall,
            makeCall, makePass, maxSuitLength, minSuitLength, nameAction, pointRange,
            suitLength, longerThan)
import qualified Terminology as T

-- These ranges assume opener's 1NT rebid is the usual 12--14 HCP.  The
-- convention starts after 1m--1M--1NT, with no interference.
invitational, weak, gameForcing, slamInterest :: Action
invitational = pointRange 11 12
weak = pointRange 0 9
gameForcing = pointRange 13 40
slamInterest = pointRange 17 40

minimum, maximum :: Action
minimum = pointRange 12 12
maximum = pointRange 13 14

responder2C :: Action
responder2C = nameAction "twnm_2C" $ do
    alternatives [ invitational
                 , weak >> minSuitLength T.Diamonds 5
                 ]
    makeAlertableCall (T.Bid 2 T.Clubs)
        "two-way new minor: invitational or weak with long diamonds"

-- The game-forcing inquiry is used when its direction is not already clear:
-- a five-card major, four cards in the other major, or exceptional high-card
-- strength.
responder2D :: T.Suit -> T.Suit -> Action
responder2D ownMajor otherMajor = nameAction "twnm_2D" $ do
    gameForcing
    forbidAll [responder4N ownMajor otherMajor, responder6N ownMajor otherMajor]
    alternatives [ minSuitLength ownMajor 5
                 , suitLength otherMajor 4
                 , pointRange 19 40
                 ]
    makeAlertableCall (T.Bid 2 T.Diamonds) "two-way new minor: game forcing"

responderGame :: T.Suit -> Action
responderGame major = nameAction "twnm_game" $ do
    gameForcing
    forbid slamInterest
    minSuitLength major 6
    makeCall $ if major == T.Hearts then T.Bid 4 T.Hearts else T.Bid 4 T.Spades

-- A balanced game force with neither a known major fit nor slam interest
-- simply places the contract in 3NT.
responder3N :: T.Suit -> Action
responder3N major = nameAction "twnm_3N" $ do
    gameForcing
    forbid slamInterest
    balancedHand
    maxSuitLength major 4
    maxSuitLength (T.otherMajor major) 3
    makeCall $ T.Bid 3 T.Notrump

responder4N :: T.Suit -> T.Suit -> Action
responder4N ownMajor otherMajor = nameAction "twnm_4N" $ do
    balancedHand
    pointRange 19 20
    maxSuitLength ownMajor 4
    maxSuitLength otherMajor 3
    makeCall $ T.Bid 4 T.Notrump

responder6N :: T.Suit -> T.Suit -> Action
responder6N ownMajor otherMajor = nameAction "twnm_6N" $ do
    balancedHand
    pointRange 21 40
    maxSuitLength ownMajor 4
    maxSuitLength otherMajor 3
    makeCall $ T.Bid 6 T.Notrump

responderSlamTwoSuit :: T.Suit -> T.Suit -> Action
responderSlamTwoSuit first second = nameAction "twnm_slam_two_suit" $ do
    slamInterest
    minSuitLength first 5
    minSuitLength second 5
    makeAlertableCall (T.Bid 3 second) "slam interest with 5-5 shape"

responderSlamRebid :: T.Suit -> Action
responderSlamRebid major = nameAction "twnm_slam_rebid" $ do
    slamInterest
    minSuitLength major 6
    makeAlertableCall (T.Bid 3 major) "slam interest with a six-card major"

opener2C2D :: Action
opener2C2D = nameAction "twnm_2C_2D" $
    makeAlertableCall (T.Bid 2 T.Diamonds) "forced relay"

openerOtherMajor :: T.Suit -> T.Suit -> Action
openerOtherMajor responderMajor otherMajor = nameAction "twnm_other_major" $ do
    -- When responder's major is hearts, showing support with 2H is cheaper
    -- than showing the other major with 2S.  With a spade response, the
    -- other-major bid (2H) is already the cheaper call.
    if responderMajor == T.Hearts then forbid (openerSupport responderMajor)
                                   else return ()
    suitLength otherMajor 4
    makeAlertableCall (T.Bid 2 otherMajor) "four cards in the unbid major"

openerSupport :: T.Suit -> Action
openerSupport responderMajor = nameAction "twnm_support" $ do
    -- Over a spade response, 2H (the other major) must be shown first.
    if responderMajor == T.Spades
        then forbid (openerOtherMajor responderMajor T.Hearts)
        else return ()
    suitLength responderMajor 3
    makeAlertableCall (T.Bid 2 responderMajor) "three-card support"

openerNothingSpecial :: T.Suit -> T.Suit -> T.Suit -> Action
openerNothingSpecial responderMajor otherMajor openerMinor = nameAction "twnm_2N" $ do
    forbidAll [openerOtherMajor responderMajor otherMajor,
               openerSupport responderMajor]
    maxSuitLength openerMinor 4
    makeAlertableCall (T.Bid 2 T.Notrump) "nothing special"

openerFiveMinor :: T.Suit -> T.Suit -> T.Suit -> Action
openerFiveMinor responderMajor otherMajor openerMinor = nameAction "twnm_5m" $ do
    forbidAll [openerOtherMajor responderMajor otherMajor,
               openerSupport responderMajor]
    minSuitLength openerMinor 5
    makeAlertableCall (T.Bid 3 openerMinor) "five-card opened minor"

weakMajorRebid :: T.Suit -> Action
weakMajorRebid major = nameAction "twnm_weak_major" $ do
    weak
    minSuitLength major 5
    makeCall $ T.Bid 2 major

weakSpadesHearts :: Action
weakSpadesHearts = nameAction "twnm_weak_54_majors" $ do
    weak
    minSuitLength T.Spades 5
    minSuitLength T.Hearts 4
    T.Spades `longerThan` T.Hearts
    makeCall $ T.Bid 2 T.Hearts

weak2N :: Action
weak2N = nameAction "twnm_weak_2N" $ do
    weak
    minSuitLength T.Clubs 6
    makeAlertableCall (T.Bid 2 T.Notrump) "transfer to 3C; weak with 6+ clubs"

complete2NTransfer :: Action
complete2NTransfer = nameAction "twnm_2N_3C" $
    makeCall $ T.Bid 3 T.Clubs

invitational44Majors :: Action
invitational44Majors = nameAction "twnm_inv_44_majors" $ do
    invitational
    suitLength T.Hearts 4
    suitLength T.Spades 4
    makeAlertableCall (T.Bid 2 T.Spades) "invitational, 4-4 in the majors"

invitationalMajor :: T.Suit -> Action
invitationalMajor major = nameAction "twnm_inv_major" $ do
    invitational
    suitLength major 5
    makeAlertableCall (T.Bid 2 major) "invitational with a five-card major"

invitational54Majors :: Action
invitational54Majors = nameAction "twnm_inv_54_majors" $ do
    invitational
    minSuitLength T.Spades 5
    minSuitLength T.Hearts 4
    T.Spades `longerThan` T.Hearts
    makeAlertableCall (T.Bid 2 T.Hearts) "invitational, 5 spades and 4 hearts"

invitationalMinorFit :: T.Suit -> Action
invitationalMinorFit minor = nameAction "twnm_inv_minor_fit" $ do
    invitational
    minSuitLength minor 5
    makeAlertableCall (T.Bid 3 minor) "invitational with a good fit for opener's minor"

invitational2N :: T.Suit -> T.Suit -> Action
invitational2N ownMajor otherMajor = nameAction "twnm_inv_2N" $ do
    invitational
    balancedHand
    maxSuitLength ownMajor 4
    maxSuitLength otherMajor 3
    makeAlertableCall (T.Bid 2 T.Notrump) "balanced invitational hand"

invitationalSixMajor :: T.Suit -> Action
invitationalSixMajor major = nameAction "twnm_inv_six_major" $ do
    invitational
    minSuitLength major 6
    makeAlertableCall (T.Bid 3 major) "invitational with a good six-card major"

openerPass :: Action
openerPass = nameAction "twnm_pass" $ minimum >> makePass

opener2NMaximumNoFit :: [(T.Suit, Int)] -> Action
opener2NMaximumNoFit suitLimits = nameAction "twnm_max_no_fit_2N" $ do
    maximum
    mapM_ (uncurry maxSuitLength) suitLimits
    makeCall $ T.Bid 2 T.Notrump

openerGameMajor :: T.Suit -> Int -> Action
openerGameMajor major support = nameAction "twnm_max_fit_game" $ do
    maximum
    minSuitLength major support
    makeCall $ T.Bid 4 major

opener44PassSpades :: Action
opener44PassSpades = nameAction "twnm_44_pass_spades" $ do
    minimum
    suitLength T.Spades 4
    makePass

opener44ThreeHearts :: Action
opener44ThreeHearts = nameAction "twnm_44_3H" $ do
    minimum
    maxSuitLength T.Spades 3
    minSuitLength T.Hearts 4
    makeCall $ T.Bid 3 T.Hearts

opener44TwoNotrump :: Action
opener44TwoNotrump = nameAction "twnm_44_2N" $ do
    minimum
    maxSuitLength T.Hearts 3
    maxSuitLength T.Spades 3
    makeCall $ T.Bid 2 T.Notrump

opener44ThreeNotrump :: Action
opener44ThreeNotrump = nameAction "twnm_44_3N" $ do
    maximum
    maxSuitLength T.Hearts 3
    maxSuitLength T.Spades 3
    makeCall $ T.Bid 3 T.Notrump

opener44GameHearts :: Action
opener44GameHearts = nameAction "twnm_44_game_hearts" $ do
    maximum
    minSuitLength T.Hearts 4
    maxSuitLength T.Spades 3
    makeCall $ T.Bid 4 T.Hearts

opener54TwoSpades :: Action
opener54TwoSpades = nameAction "twnm_54_2S" $ do
    minimum
    minSuitLength T.Spades 2
    maxSuitLength T.Hearts 3
    makeCall $ T.Bid 2 T.Spades

opener54PassHearts :: Action
opener54PassHearts = nameAction "twnm_54_pass_hearts" $ do
    minimum
    minSuitLength T.Hearts 4
    makePass

opener54GameHearts :: Action
opener54GameHearts = nameAction "twnm_54_game_hearts" $ do
    maximum
    maxSuitLength T.Spades 1
    minSuitLength T.Hearts 4
    makeCall $ T.Bid 4 T.Hearts
