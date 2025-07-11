module Bids.OneNotrump(
    noInterference
  , b1N  -- re-exported from StandardOpenings
  , b1N2C
  , b1N2C2D
  , b1N2C2D2H
  , b1N2C2D2S
  , b1N2C2D2N
  , b1N2C2D3H
  , b1N2C2D3H3S
  , b1N2C2D3H3N
  , b1N2C2D3S
  , b1N2C2D3S3N
  , b1N2C2D3S4H
  , b1N2C2D3N
  , b1N2C2D4N
  , b1N2C2H
  , b1N2C2H3C
  , b1N2C2H3C3S
  , b1N2C2H3C4S
  , b1N2C2H3D
  , b1N2C2H3D3S
  , b1N2C2H3D4S
  , b1N2C2H3H
  , b1N2C2H3S
  , b1N2C2H3N
  , b1N2C2H3N4S
  , b1N2C2H4H
  , b1N2C2S
  , b1N2C2S3C
  , b1N2C2S3D
  , b1N2C2S3H
  , b1N2C2S3S
  , b1N2C2S3N
  , b1N2C2S4S
  , b1N2D
  , b1N2D2H
  , b1N2D2H2S
  , b1N2D2H3C
  , b1N2D2H3D
  , b1N2D2H3H
  , b1N2D2H4H
  , b1N2D3H
  , b1N2H
  , b1N2H2S
  , b1N2H2S3C
  , b1N2H2S3D
  , b1N2H2S3H
  , b1N2H2S3S
  , b1N2H2S4S
  , b1N2H3S
  , b1N4D
  , b1N4D4H
  , b1N4H
  , b1N4H4S
  -- Strengths
  , gameForcing  -- Game or stronger
  , slamInterest
  , slamInvite
  , gameNoSlam
  , invitational
  , lessThanInvitational
) where


import Action(Action, constrain)
import qualified Bids.Meckwell as MW
import Bids.StandardOpenings(b1N)
import CommonBids(cannotPreempt)
import EDSL(forbid, pointRange, suitLength, minSuitLength, maxSuitLength,
            makeCall, makeAlertableCall, alternatives, balancedHand,
            longerThan, atLeastAsLong, flatHand, loserCount, minLoserCount,
            forbidAll, impliesThat, forEach, hasTopN, nameAction)
import Output((.+))
import qualified Terminology as T

------------------
-- Point ranges --
------------------
-- Parameterize the strength so this is easy to adapt for other notrump ranges
minNotrumpStrength :: Int
minNotrumpStrength = 15
maxNotrumpStrength :: Int
maxNotrumpStrength = 17
strengthNeededForGame :: Int
strengthNeededForGame = 25
strengthNeededForSlam :: Int
strengthNeededForSlam = 30  -- TODO: is this the right number?

gameForcing :: Action  -- Could be stronger
gameForcing = pointRange (strengthNeededForGame - minNotrumpStrength) 40

slamInterest :: Action
slamInterest = pointRange (strengthNeededForSlam - minNotrumpStrength) 40

slamInvite :: Action
slamInvite = do
    forbid slamInterest
    pointRange (strengthNeededForSlam - maxNotrumpStrength) 40

gameNoSlam :: Action
gameNoSlam = do
    gameForcing
    forbid slamInterest
    forbid slamInvite

invitational :: Action
invitational = do
    forbid gameForcing
    pointRange (strengthNeededForGame - maxNotrumpStrength) 40

lessThanInvitational :: Action
lessThanInvitational = do
    forbidAll [invitational, gameForcing]


noInterference :: Action
noInterference = nameAction "no_interference" $ do
    cannotPreempt
    -- If the opponents can't bid Meckwell, they probably can't bid anything.
    forbidAll [MW.b1NoX, MW.b1No2C, MW.b1No2D, MW.b1No2H, MW.b1No2S, MW.b1No2N]
    makeCall T.Pass


texasTransfer_ :: T.Suit -> Action
texasTransfer_ suit = do
    alternatives [gameNoSlam, slamInterest]
    minSuitLength suit 6
    -- If you're 6-4, bid Stayman, and *then* make a Texas Transfer if necessary
    maxSuitLength (T.otherMajor suit) 3
    -- If you're 6-6, which suit to use is a matter of judgment, and you won't
    -- get practice here. Too bad.
    forEach (filter (/= suit) T.allSuits) (suit `longerThan`)
    makeAlertableCall (T.Bid 4 (transferSuit suit))
                      ("Transfer to " .+ show suit)
  where
    transferSuit T.Hearts = T.Diamonds
    transferSuit T.Spades = T.Hearts
    transferSuit _        = error "Texas transfer to non-major suit"

b1N4D :: Action
b1N4D = nameAction "b1N4D" (texasTransfer_ T.Hearts)

b1N4H :: Action
b1N4H = nameAction "b1N4H" (texasTransfer_ T.Spades)


-- Opener should always complete the Texas Transfer: no constraints on that.
b1N4D4H :: Action
b1N4D4H = nameAction "b1N4D4H" (makeCall $ T.Bid 4 T.Hearts)

b1N4H4S :: Action
b1N4H4S = nameAction "b1N4H4S" (makeCall $ T.Bid 4 T.Spades)


equalMajors_ :: Action
equalMajors_ = constrain "equal_majors" ["hearts(", ") == spades(", ")"]


b1N2D :: Action
b1N2D = nameAction "b1N2D" $ do
    minSuitLength T.Hearts 5
    forEach [T.Clubs, T.Diamonds, T.Spades] (T.Hearts `atLeastAsLong`)
    -- Prefer to bid Smolen or a Texas transfer instead.
    forbidAll [b1N2C2D3S, b1N4D]
    -- If you're 5-5 in the majors with invitational strength, transfer to
    -- hearts and then bid spades. If you're game forcing, transfer to spades
    -- and then bid hearts. If you're less than invitational, we're not going to
    -- practice being 5-5 in the majors: just transfer to the better one and
    -- call that good enough. It's not worth coding up how to pick the better of
    -- two 5-card majors.
    equalMajors_ `impliesThat` invitational
    makeAlertableCall (T.Bid 2 T.Diamonds) "Transfer to hearts"


b1N2H :: Action
b1N2H = nameAction "b1N2H" $ do
    minSuitLength T.Spades 5
    forEach [T.Clubs, T.Diamonds, T.Hearts] (T.Spades `atLeastAsLong`)
    -- Prefer to bid Smolen or a Texas transfer instead.
    forbidAll [b1N2C2D3H, b1N4H]
    -- If you're 5-5 in the majors with invitational strength, transfer to
    -- hearts and then bid spades. If you're game forcing, transfer to spades
    -- and then bid hearts. We don't practice times when we're 5-5 with less
    -- than invitational strength.
    equalMajors_ `impliesThat` gameForcing
    makeAlertableCall (T.Bid 2 T.Hearts) "Transfer to spades"


-- You can superaccept a Jacoby transfer with 4-card support and a maximum.
b1N2D3H :: Action
b1N2D3H = nameAction "b1N2D3H" $ do
    minSuitLength T.Hearts 4
    pointRange 17 17 -- Do it with a good 16, too, but defining "good" is hard
    forbid flatHand
    makeCall $ T.Bid 3 T.Hearts

b1N2H3S :: Action
b1N2H3S = nameAction "b1N2H3S" $ do
    minSuitLength T.Spades 4
    pointRange 17 17 -- Do it with a good 16, too, but defining "good" is hard
    forbid flatHand
    makeCall $ T.Bid 3 T.Spades


-- If you can't superaccept, just normal-accept
b1N2D2H :: Action
b1N2D2H = nameAction "b1N2D2H" $ do
    forbid b1N2D3H
    makeCall $ T.Bid 2 T.Hearts

b1N2H2S :: Action
b1N2H2S = nameAction "b1N2H2S" $ do
    forbid b1N2H3S
    makeCall $ T.Bid 2 T.Spades


-- A slam invite is a Jacoby transfer that is then raised immediately to game
b1N2D2H4H :: Action
b1N2D2H4H = nameAction "b1N2D2H4H" $ do
    minSuitLength T.Hearts 6
    slamInvite
    makeCall $ T.Bid 4 T.Hearts

b1N2H2S4S :: Action
b1N2H2S4S = nameAction "b1N2H2S4S" $ do
    minSuitLength T.Spades 6
    slamInvite
    makeCall $ T.Bid 4 T.Spades


-- This version of Stayman guarantees a 4-card major. If you use 4-way
-- transfers, use a Stayman that might be a notrump invite without a 4-card
-- major, so that 2N can be a transfer to diamonds instead of a natural invite.
b1N2C :: Action
b1N2C = nameAction "b1N2C" $ do
    -- For normal Stayman, you should have an exactly 4-card major: 4-2 is
    -- Stayman, 4-4 is Stayman, 5-4 is Stayman then Smolen, 6-4 is Stayman then
    -- Texas, 5-5 is a transfer, 5-3 is a transfer, 6-3 is a transfer.
    alternatives [ do pointRange 8 40  -- Normal Stayman
                      alternatives . map (`suitLength` 4) $ T.majorSuits
                 , do suitLength T.Hearts 4  -- Garbage Stayman
                      suitLength T.Spades 4
                      minSuitLength T.Diamonds 4]
    -- With 4333 shape, just stick with notrump.
    forbid flatHand
    -- With 4-3 in the majors GF, you might use Puppet Stayman instead
    forbid (do alternatives [ suitLength T.Hearts 3 >> suitLength T.Spades 4
                            , suitLength T.Spades 3 >> suitLength T.Hearts 4 ]
               pointRange 10 40)
    makeCall $ T.Bid 2 T.Clubs  -- Not alertable in the ACBL!


b1N2C2D :: Action
b1N2C2D = nameAction "b1N2C2D" $ do
    maxSuitLength T.Hearts 3
    maxSuitLength T.Spades 3
    makeCall $ T.Bid 2 T.Diamonds  -- Not alertable in the ACBL!


b1N2C2H :: Action
b1N2C2H = nameAction "b1N2C2H" $ do
    minSuitLength T.Hearts 4
    makeCall $ T.Bid 2 T.Hearts


b1N2C2S :: Action
b1N2C2S = nameAction "b1N2C2S" $ do
    minSuitLength T.Spades 4
    -- If you're 4-4 in the majors, bid 1H instead.
    maxSuitLength T.Hearts 3
    makeCall $ T.Bid 2 T.Spades


smolen_ :: T.Suit -> Action
smolen_ longMajor = do
    let shortMajor = T.otherMajor longMajor
    pointRange 10 40
    -- The suit lengths must be this exactly: with 5-5, make a Jacoby transfer
    -- then bid the other suit. With 6-4, bid Stayman and then a Texas transfer.
    suitLength longMajor 5
    suitLength shortMajor 4
    makeAlertableCall (T.Bid 3 shortMajor)
        ("5 " .+ show longMajor .+ ", 4 " .+ show shortMajor .+ ", GF")

b1N2C2D3H :: Action
b1N2C2D3H = nameAction "b1N2C2D3H" (smolen_ T.Spades)

b1N2C2D3S :: Action
b1N2C2D3S = nameAction "b1N2C2D3S" (smolen_ T.Hearts)


b1N2C2D3H3N :: Action
b1N2C2D3H3N = nameAction "b1N2C2D3H3N" $ do
    maxSuitLength T.Spades 2
    makeCall $ T.Bid 3 T.Notrump


b1N2C2D3S3N :: Action
b1N2C2D3S3N = nameAction "b1N2C2D3S3N" $ do
    maxSuitLength T.Hearts 2
    makeCall $ T.Bid 3 T.Notrump


b1N2C2D3H3S :: Action
b1N2C2D3H3S = nameAction "b1N2C2D3H3S" $ do
    forbid b1N2C2D3H3N
    makeCall $ T.Bid 3 T.Spades


b1N2C2D3S4H :: Action
b1N2C2D3S4H = nameAction "b1N2C2D3S4H" $ do
    forbid b1N2C2D3S3N
    makeCall $ T.Bid 4 T.Hearts


b1N2C2D2N :: Action
b1N2C2D2N = nameAction "b1N2C2D2N" $ do
    balancedHand  -- Is this right? What would you rebid with a 4135 invite?
    pointRange 8 9
    makeCall $ T.Bid 2 T.Notrump


b1N2C2D3N :: Action
b1N2C2D3N = nameAction "b1N2C2D3N" $ do
    balancedHand
    pointRange 10 14
    makeCall $ T.Bid 3 T.Notrump


b1N2C2D4N :: Action
b1N2C2D4N = nameAction "b1N2C2D4N" $ do
    balancedHand
    pointRange 15 16
    makeCall $ T.Bid 4 T.Notrump


inviteWithMajor :: T.Suit -> Action
inviteWithMajor suit = do
    minSuitLength suit 4
    pointRange 8 9
    minLoserCount 9
    makeCall $ T.Bid 3 suit

b1N2C2H3H :: Action
b1N2C2H3H = nameAction "b1N2C2H3H" (inviteWithMajor T.Hearts)

b1N2C2S3S :: Action
b1N2C2S3S = nameAction "b1N2C2S3S" (inviteWithMajor T.Spades)


gameForceWithMajor :: T.Suit -> Action
gameForceWithMajor suit = do
    minSuitLength suit 4
    pointRange 10 13
    minLoserCount 7
    makeCall $ T.Bid 4 suit

b1N2C2H4H :: Action
b1N2C2H4H = nameAction "b1N2C2H4H" (gameForceWithMajor T.Hearts)

b1N2C2S4S :: Action
b1N2C2S4S = nameAction "b1N2C2S4S" (gameForceWithMajor T.Spades)


slamWithMajor :: T.Suit -> T.Suit -> Action
slamWithMajor suit otherSuit = do
    minSuitLength suit 4
    pointRange 14 40
    makeAlertableCall (T.Bid 3 otherSuit) "slam interest in partner's major"

b1N2C2H3S :: Action
b1N2C2H3S = nameAction "b1N2C2H3S" (slamWithMajor T.Hearts T.Spades)

b1N2C2S3H :: Action
b1N2C2S3H = nameAction "b1N2C2S3H" (slamWithMajor T.Spades T.Hearts)


wrongMajorTo3N :: T.Suit -> Action
wrongMajorTo3N suit = do
    pointRange 10 13
    balancedHand
    maxSuitLength suit 3
    makeCall $ T.Bid 3 T.Notrump

b1N2C2H3N :: Action
b1N2C2H3N = nameAction "b1N2C2H3N" (wrongMajorTo3N T.Hearts)

b1N2C2S3N :: Action
b1N2C2S3N = nameAction "b1N2C2S3N" (wrongMajorTo3N T.Spades)


inv54 :: T.Suit -> Action
inv54 major = do
    pointRange 8 9
    suitLength major 5
    suitLength (T.otherMajor major) 4
    minLoserCount 7
    makeCall $ T.Bid 2 major

b1N2C2D2H :: Action
b1N2C2D2H = nameAction "b1N2C2D2H" (inv54 T.Hearts)

b1N2C2D2S :: Action
b1N2C2D2S = nameAction "b1N2C2D2S" (inv54 T.Spades)


gfNoFitUnbalanced :: T.Suit -> T.Suit -> Action
gfNoFitUnbalanced partnerMajor ourMinor = do
    pointRange 10 40
    maxSuitLength partnerMajor 3
    minSuitLength ourMinor 4
    -- We're going to skip 3-suited hands in here: deciding which of your 4-card
    -- minors to bid is more nuance than I want to figure out right now.
    maxSuitLength (T.otherMinor ourMinor) 3
    forbid balancedHand
    makeCall $ T.Bid 3 ourMinor

b1N2C2H3C :: Action
b1N2C2H3C = nameAction "b1N2C2H3C" (gfNoFitUnbalanced T.Hearts T.Clubs)

b1N2C2H3D :: Action
b1N2C2H3D = nameAction "b1N2C2H3D" (gfNoFitUnbalanced T.Hearts T.Diamonds)

b1N2C2S3C :: Action
b1N2C2S3C = nameAction "b1N2C2S3C" (gfNoFitUnbalanced T.Spades T.Clubs)

b1N2C2S3D :: Action
b1N2C2S3D = nameAction "b1N2C2S3D" (gfNoFitUnbalanced T.Spades T.Diamonds)


-- Times when opener is 4-4 in the majors, bids hearts over Stayman, and doesn't
-- yet find a fit.
b1N2C2H3C4S :: Action
b1N2C2H3C4S = nameAction "b1N2C2H3C4S" $ do
    suitLength T.Spades 4
    makeCall $ T.Bid 4 T.Spades

b1N2C2H3D4S :: Action
b1N2C2H3D4S = nameAction "b1N2C2H3D4S" $ do
    suitLength T.Spades 4
    makeCall $ T.Bid 4 T.Spades

b1N2C2H3N4S :: Action
b1N2C2H3N4S = nameAction "b1N2C2H3N4S" $ do
    suitLength T.Spades 4
    makeCall $ T.Bid 4 T.Spades

-- If responder is an unpassed hand, prefer to rebid 3S instead of 4S
b1N2C2H3C3S :: Action
b1N2C2H3C3S = nameAction "b1N2C2H3C3S" $ do
    suitLength T.Spades 4
    makeCall $ T.Bid 3 T.Spades

b1N2C2H3D3S :: Action
b1N2C2H3D3S = nameAction "b1N2C2H3D3S" $ do
    suitLength T.Spades 4
    makeCall $ T.Bid 3 T.Spades


-- Bids showing 5-5 in the majors

b1N2D2H2S :: Action
b1N2D2H2S = nameAction "b1N2D2H2S" $ do
    minSuitLength T.Spades 5
    invitational
    makeCall $ T.Bid 2 T.Spades

b1N2H2S3H :: Action
b1N2H2S3H = nameAction "b1N2H2S3H" $ do
    minSuitLength T.Hearts 5
    gameForcing
    makeCall $ T.Bid 3 T.Hearts

-- Unbalanced game-forcing rebids

b1N2D2H3C :: Action
b1N2D2H3C = nameAction "b1N2D2H3C" $ do
    gameForcing
    minSuitLength T.Clubs 4
    hasTopN T.Clubs 4 2  -- Don't do it with a bad side suit
    maxSuitLength T.Spades 3 -- With 4, you should have bid Smolen
    -- If you're 0544 shape, rebid your better minor. I didn't bother to build a
    -- way to find the better of two suits of equal length, so just pretend that
    -- can't happen.
    T.Clubs `longerThan` T.Diamonds
    forbid balancedHand  -- should be redundant, but just in case
    makeCall $ T.Bid 3 T.Clubs

b1N2D2H3D :: Action
b1N2D2H3D = nameAction "b1N2D2H3D" $ do
    gameForcing
    minSuitLength T.Diamonds 4
    hasTopN T.Diamonds 4 2  -- Don't do it with a bad side suit
    maxSuitLength T.Spades 3 -- With 4, you should have bid Smolen
    -- If you're 0544 shape, rebid your better minor. I didn't bother to build a
    -- way to find the better of two suits of equal length, so just pretend that
    -- can't happen.
    T.Diamonds `longerThan` T.Clubs
    forbid balancedHand  -- should be redundant, but just in case
    makeCall $ T.Bid 3 T.Diamonds

b1N2H2S3C :: Action
b1N2H2S3C = nameAction "b1N2H2S3C" $ do
    gameForcing
    minSuitLength T.Clubs 4
    hasTopN T.Clubs 4 2  -- Don't do it with a bad side suit
    maxSuitLength T.Hearts 3 -- With 4, you should have bid Smolen
    -- If you're 0544 shape, rebid your better minor. I didn't bother to build a
    -- way to find the better of two suits of equal length, so just pretend that
    -- can't happen.
    T.Clubs `longerThan` T.Diamonds
    forbid balancedHand  -- should be redundant, but just in case
    makeCall $ T.Bid 3 T.Clubs

b1N2H2S3D :: Action
b1N2H2S3D = nameAction "b1N2H2S3D" $ do
    gameForcing
    minSuitLength T.Diamonds 4
    hasTopN T.Diamonds 4 2  -- Don't do it with a bad side suit
    maxSuitLength T.Hearts 3 -- With 4, you should have bid Smolen
    -- If you're 0544 shape, rebid your better minor. I didn't bother to build a
    -- way to find the better of two suits of equal length, so just pretend that
    -- can't happen.
    T.Diamonds `longerThan` T.Clubs
    forbid balancedHand  -- should be redundant, but just in case
    makeCall $ T.Bid 3 T.Diamonds


b1N2D2H3H :: Action
b1N2D2H3H = nameAction "b1N2D2H3H" $ do
    minSuitLength T.Hearts 6
    invitational
    loserCount 8
    -- If you had a second suit, you might consider bidding that instead. That
    -- would be game-forcing instead of invitational, but with such a shapely
    -- hand you might upgrade to game-forcing anyway. Avoid these situations.
    forEach [T.Clubs, T.Diamonds, T.Spades] (`maxSuitLength` 3)
    makeCall $ T.Bid 3 T.Hearts


b1N2H2S3S :: Action
b1N2H2S3S = nameAction "b1N2H2S3S" $ do
    minSuitLength T.Spades 6
    invitational
    loserCount 8
    -- If you had a second suit, you might consider bidding that instead. That
    -- would be game-forcing instead of invitational, but with such a shapely
    -- hand you might upgrade to game-forcing anyway. Avoid these situations.
    forEach [T.Clubs, T.Diamonds, T.Hearts] (`maxSuitLength` 3)
    makeCall $ T.Bid 3 T.Spades
