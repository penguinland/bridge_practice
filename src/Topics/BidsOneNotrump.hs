module Topics.BidsOneNotrump(
    b1N  -- Copied from StandardOpenings
  , b1N2C
  , b1N2D
  , b1N2H
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


import Auction(forbid, pointRange, suitLength, minSuitLength, maxSuitLength,
               Action, makeCall, makeAlertableCall, alternatives, longerThan)
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
strengthNeededForSlam = 31

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
    mapM_ forbid [invitational, gameForcing]


texasTransfer :: T.Suit -> Action
texasTransfer suit = do
    alternatives [gameNoSlam, slamInterest]
    minSuitLength suit 6
    -- If you're 6-4, bid Stayman, and *then* make a Texas Transfer if necessary
    maxSuitLength (otherMajor suit) 3
    -- If you're 6-6, which suit to use is a matter of judgment, and you won't
    -- get practice here. Too bad.
    mapM_ (suit `longerThan`) . filter (/= suit) $ T.allSuits
    makeAlertableCall (T.Bid 4 (transferSuit suit))
                      ("Transfer to " .+ show suit)
  where
    transferSuit T.Hearts = T.Diamonds
    transferSuit T.Spades = T.Hearts
    transferSuit _        = error "Texas transfer to non-major suit"
    otherMajor T.Hearts = T.Spades
    otherMajor T.Spades = T.Hearts
    otherMajor _        = error "other major of non-major suit"

b1N4D :: Action
b1N4D = texasTransfer T.Hearts

b1N4H :: Action
b1N4H = texasTransfer T.Spades

-- Opener should always complete the Texas Transfer: no constraints on that.
b1N4D4H :: Action
b1N4D4H = makeCall $ T.Bid 4 T.Hearts

b1N4H4S :: Action
b1N4H4S = makeCall $ T.Bid 4 T.Spades


jacobyTransfer :: T.Suit -> Action
jacobyTransfer suit = do
    alternatives [ suitLength suit 5
                 , minSuitLength suit 6 >> forbid (texasTransfer suit)]
    -- If you're 6-6, which suit to use is a matter of judgment, and you won't
    -- get practice here. Too bad.
    mapM_ (suit `longerThan`) . filter (/= suit) $ T.allSuits
    makeAlertableCall (T.Bid 2 (transferSuit suit))
                      ("Transfer to " .+ show suit)
  where
    transferSuit T.Hearts = T.Diamonds
    transferSuit T.Spades = T.Hearts
    transferSuit _        = error "Jacoby transfer to non-major suit"

b1N2D :: Action
b1N2D = jacobyTransfer T.Hearts

b1N2H :: Action
b1N2H = jacobyTransfer T.Spades


b1N2C :: Action
b1N2C = do
    alternatives [pointRange 8 40,  -- Normal Stayman
                  suitLength T.Hearts 4 >>  -- Garbage Stayman
                  suitLength T.Spades 4 >>
                  minSuitLength T.Diamonds 4]
    maxSuitLength T.Clubs 4 -- With more than that, raise partner's minor
    minSuitLength T.Diamonds 4
    -- If you've got a major, only respond 1D if you're game forcing.
    alternatives [maxSuitLength T.Hearts 3 >> maxSuitLength T.Spades 3,
                  pointRange 13 40]
    makeCall $ T.Bid 1 T.Diamonds

