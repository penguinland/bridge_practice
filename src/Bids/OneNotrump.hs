module Bids.OneNotrump(
    noInterference
  , b1N  -- Copied from StandardOpenings
  , b1N2C
  , b1N2C2D
  , b1N2C2D2H
  , b1N2C2D2S
  , b1N2C2D2N
  , b1N2C2D3H
  , b1N2C2D3S
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
  , b1N2D2H4H
  , b1N2D3H
  , b1N2H
  , b1N2H2S
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


import Action(Action)
import qualified Bids.Meckwell as MW
import CommonBids(cannotPreempt)
import EDSL(forbid, pointRange, suitLength, minSuitLength, maxSuitLength,
            makeCall, makeAlertableCall, alternatives, longerThan, balancedHand,
            flatHand, minLoserCount, forEach, forbidAll)
import Output((.+))
import StandardOpenings(b1N)
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
noInterference = do
    cannotPreempt
    -- If the opponents can't bid Meckwell, they probably can't bid anything.
    forbidAll [MW.b1NoX, MW.b1No2C, MW.b1No2D, MW.b1No2H, MW.b1No2S, MW.b1No2N]
    makeCall T.Pass


_texasTransfer :: T.Suit -> Action
_texasTransfer suit = do
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
b1N4D = _texasTransfer T.Hearts

b1N4H :: Action
b1N4H = _texasTransfer T.Spades


-- Opener should always complete the Texas Transfer: no constraints on that.
b1N4D4H :: Action
b1N4D4H = makeCall $ T.Bid 4 T.Hearts

b1N4H4S :: Action
b1N4H4S = makeCall $ T.Bid 4 T.Spades


_jacobyTransfer :: T.Suit -> Action
_jacobyTransfer suit = do
    alternatives [ suitLength suit 5
                 , minSuitLength suit 6 >> forbid (_texasTransfer suit)]
    -- If you're 6-6, which suit to use is a matter of judgment, and you won't
    -- get practice here. Too bad.
    forEach (filter (/= suit) T.allSuits) (suit `longerThan`)
    -- No matter what suit you're trying to transfer to, ensure that you
    -- shouldn't have bid Smolen instead.
    forbidAll [b1N2C2D3H, b1N2C2D3S]
    makeAlertableCall (T.Bid 2 (transferSuit suit))
                      ("Transfer to " .+ show suit)
  where
    transferSuit T.Hearts = T.Diamonds
    transferSuit T.Spades = T.Hearts
    transferSuit _        = error "Jacoby transfer to non-major suit"

b1N2D :: Action
b1N2D = _jacobyTransfer T.Hearts

b1N2H :: Action
b1N2H = _jacobyTransfer T.Spades


-- You can superaccept a Jacoby transfer with 4-card support and a maximum.
b1N2D3H :: Action
b1N2D3H = do
    minSuitLength T.Hearts 4
    pointRange 17 17 -- Do it with a good 16, too, but defining "good" is hard
    makeCall $ T.Bid 3 T.Hearts

b1N2H3S :: Action
b1N2H3S = do
    minSuitLength T.Spades 4
    pointRange 17 17 -- Do it with a good 16, too, but defining "good" is hard
    makeCall $ T.Bid 3 T.Spades


-- If you can't superaccept, just normal-accept
b1N2D2H :: Action
b1N2D2H = do
    forbid b1N2D3H
    makeCall $ T.Bid 2 T.Hearts

b1N2H2S :: Action
b1N2H2S = do
    forbid b1N2H3S
    makeCall $ T.Bid 2 T.Spades


-- A slam invite is a Jacoby transfer that is then raised immediately to game
b1N2D2H4H :: Action
b1N2D2H4H = do
    minSuitLength T.Hearts 6
    slamInvite
    makeCall $ T.Bid 4 T.Hearts

b1N2H2S4S :: Action
b1N2H2S4S = do
    minSuitLength T.Spades 6
    slamInvite
    makeCall $ T.Bid 4 T.Spades


-- This version of Stayman guarantees a 4-card major. If you use 4-way
-- transfers, use a Stayman that might be a notrump invite without a 4-card
-- major, so that 2N can be a transfer to diamonds instead of a natural invite.
b1N2C :: Action
b1N2C = do
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
b1N2C2D = do
    maxSuitLength T.Hearts 3
    maxSuitLength T.Spades 3
    makeCall $ T.Bid 2 T.Diamonds  -- Not alertable in the ACBL!


b1N2C2H :: Action
b1N2C2H = do
    minSuitLength T.Hearts 4
    makeCall $ T.Bid 2 T.Hearts


b1N2C2S :: Action
b1N2C2S = do
    minSuitLength T.Spades 4
    -- If you're 4-4 in the majors, bid 1H instead.
    maxSuitLength T.Hearts 3
    makeCall $ T.Bid 2 T.Spades


b1N2C2D3H :: Action
b1N2C2D3H = do
    pointRange 10 40
    suitLength T.Spades 5
    suitLength T.Hearts 4
    makeAlertableCall (T.Bid 3 T.Hearts) "Smolen: 5 spades, 4 hearts, GF"


b1N2C2D3S :: Action
b1N2C2D3S = do
    pointRange 10 40
    suitLength T.Hearts 5
    suitLength T.Spades 4
    makeAlertableCall (T.Bid 3 T.Spades) "Smolen: 5 hearts, 4 spades, GF"


b1N2C2D2N :: Action
b1N2C2D2N = do
    balancedHand  -- Is this right? What would you rebid with a 4135 invite?
    pointRange 8 9
    makeCall $ T.Bid 2 T.Notrump


b1N2C2D3N :: Action
b1N2C2D3N = do
    balancedHand
    pointRange 10 14
    makeCall $ T.Bid 3 T.Notrump


b1N2C2D4N :: Action
b1N2C2D4N = do
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
b1N2C2H3H = inviteWithMajor T.Hearts

b1N2C2S3S :: Action
b1N2C2S3S = inviteWithMajor T.Spades


gameForceWithMajor :: T.Suit -> Action
gameForceWithMajor suit = do
    minSuitLength suit 4
    pointRange 10 13
    minLoserCount 7
    makeCall $ T.Bid 4 suit

b1N2C2H4H :: Action
b1N2C2H4H = gameForceWithMajor T.Hearts

b1N2C2S4S :: Action
b1N2C2S4S = gameForceWithMajor T.Spades


slamWithMajor :: T.Suit -> T.Suit -> Action
slamWithMajor suit otherSuit = do
    minSuitLength suit 4
    pointRange 14 40
    makeAlertableCall (T.Bid 3 otherSuit) "slam interest in partner's major"

b1N2C2H3S :: Action
b1N2C2H3S = slamWithMajor T.Hearts T.Spades

b1N2C2S3H :: Action
b1N2C2S3H = slamWithMajor T.Spades T.Hearts


wrongMajorTo3N :: T.Suit -> Action
wrongMajorTo3N suit = do
    pointRange 10 13
    balancedHand
    maxSuitLength suit 3
    makeCall $ T.Bid 3 T.Notrump

b1N2C2H3N :: Action
b1N2C2H3N = wrongMajorTo3N T.Hearts

b1N2C2S3N :: Action
b1N2C2S3N = wrongMajorTo3N T.Spades


inv54 :: T.Suit -> Action
inv54 major = do
    pointRange 8 9
    suitLength major 5
    suitLength (T.otherMajor major) 4
    minLoserCount 7
    makeCall $ T.Bid 2 major

b1N2C2D2H :: Action
b1N2C2D2H = inv54 T.Hearts

b1N2C2D2S :: Action
b1N2C2D2S = inv54 T.Spades


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
b1N2C2H3C = gfNoFitUnbalanced T.Hearts T.Clubs

b1N2C2H3D :: Action
b1N2C2H3D = gfNoFitUnbalanced T.Hearts T.Diamonds

b1N2C2S3C :: Action
b1N2C2S3C = gfNoFitUnbalanced T.Spades T.Clubs

b1N2C2S3D :: Action
b1N2C2S3D = gfNoFitUnbalanced T.Spades T.Diamonds


-- Times when opener is 4-4 in the majors, bids hearts over Stayman, and doesn't
-- yet find a fit.
b1N2C2H3C4S :: Action
b1N2C2H3C4S = do
    suitLength T.Spades 4
    makeCall $ T.Bid 4 T.Spades

b1N2C2H3D4S :: Action
b1N2C2H3D4S = do
    suitLength T.Spades 4
    makeCall $ T.Bid 4 T.Spades

b1N2C2H3N4S :: Action
b1N2C2H3N4S = do
    suitLength T.Spades 4
    makeCall $ T.Bid 4 T.Spades

-- If responder is an unpassed hand, prefer to rebid 3S instead of 4S
b1N2C2H3C3S :: Action
b1N2C2H3C3S = do
    suitLength T.Spades 4
    makeCall $ T.Bid 3 T.Spades

b1N2C2H3D3S :: Action
b1N2C2H3D3S = do
    suitLength T.Spades 4
    makeCall $ T.Bid 3 T.Spades
