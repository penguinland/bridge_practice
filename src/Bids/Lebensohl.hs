module Bids.Lebensohl(
    b1N  -- re-exported from StandardOpenings
    -- Systems on over X
  , b1NoX2C
  , b1NoX2D
  , b1NoX2H
    -- Systems on over 2C
  , b1No2CX
  , b1No2C2D
  , b1No2C2H
   -- Actual lebensohl bids
  , b1N2N3C
  , b1No2C3D
  , b1No2C3H
  , b1No2C3S
  , b1No2D2H
  , b1No2D2S
  , b1No2D2N
  , b1No2D2N3CP
  , b1No2D2N3C3D
  , b1No2D2N3C3H
  , b1No2D2N3C3S
  , b1No2D2N3C3N
  , b1No2D3C
  , b1No2D3D
  , b1No2D3H
  , b1No2D3S
  , b1No2D3N
  , b1No2H2S
  , b1No2H2N
  , b1No2H2N3CP
  , b1No2H2N3C3D
  , b1No2H2N3C3H
  , b1No2H2N3C3S
  , b1No2H2N3C3N
  , b1No2H3C
  , b1No2H3D
  , b1No2H3H
  , b1No2H3S
  , b1No2H3N
  , b1No2S2N
  , b1No2S2N3CP
  , b1No2S2N3C3D
  , b1No2S2N3C3H
  , b1No2S2N3C3S
  , b1No2S2N3C3N
  , b1No2S3C
  , b1No2S3D
  , b1No2S3H
  , b1No2S3S
  , b1No2S3N
  -- For when the opponents show both majors
  , b1NoBM2N
  , b1NoBM2N3C3N
  , b1NoBM3N
) where


import Control.Monad(when)

import Action(Action, withholdBid)
import qualified Bids.OneNotrump as NT
import EDSL(minSuitLength, makeCall, makeAlertableCall, pointRange, forEach,
            forbid, forbidAll, balancedHand, semibalancedHand, hasStopper,
            alternatives, soundHolding, longerThan, atLeastAsLong, suitLength,
            maxSuitLength)
import Output((.+))
import qualified Terminology as T


b1N :: Action
b1N = NT.b1N


-- Ignore the opponents' interference if it doesn't actually take up any bidding
-- space.
b1NoX2C :: Action
b1NoX2C = NT.b1N2C

b1NoX2D :: Action
b1NoX2D = NT.b1N2D

b1NoX2H :: Action
b1NoX2H = NT.b1N2H

b1No2CX :: Action
b1No2CX = do
    withholdBid NT.b1N2C
    makeCall T.Double

b1No2C2D :: Action
b1No2C2D = NT.b1N2D

b1No2C2H :: Action
b1No2C2H = NT.b1N2H

-- Signoffs
signoff_ :: Int -> T.Suit -> Action
signoff_ level suit = do
    NT.lessThanInvitational
    pointRange 5 40  -- We should have at least half the points to compete
    -- NOTE: if we're nonvul, we might compete with less than half the points if
    -- we're extra shapely. That's hard to encode here because we don't know the
    -- vulnerability yet, and annoying to code in a topic because we can't reuse
    -- this function for that situation. Just skip it; users won't get to
    -- practice but it's a rare situation that is hopefully obvious when it
    -- occurs.
    minSuitLength suit 5
    -- Have a good reason to bid this suit.
    alternatives [soundHolding suit, minSuitLength suit 6]
    -- If you're 5-5 in the majors, pick the better suit. I'm too lazy to add
    -- something to the EDSL to figure out which suit is better, so avoid it.
    forEach (filter (/= suit) T.allSuits) (suit `longerThan`)
    makeCall $ T.Bid level suit


b1No2D2H :: Action
b1No2D2H = signoff_ 2 T.Hearts


b1No2D2S :: Action
b1No2D2S = signoff_ 2 T.Spades


b1No2H2S :: Action
b1No2H2S = signoff_ 2 T.Spades


primarySuit_ :: T.Suit -> Action
primarySuit_ T.Clubs = do
    T.Clubs `longerThan` T.Diamonds
    T.Clubs `longerThan` T.Hearts
    T.Clubs `longerThan` T.Spades
primarySuit_ T.Diamonds = do
    T.Diamonds `atLeastAsLong` T.Clubs
    T.Diamonds `longerThan` T.Hearts
    T.Diamonds `longerThan` T.Spades
primarySuit_ T.Hearts = do
    T.Hearts `atLeastAsLong` T.Clubs
    T.Hearts `atLeastAsLong` T.Diamonds
    T.Hearts `longerThan` T.Spades
primarySuit_ T.Spades = do
    T.Spades `atLeastAsLong` T.Clubs
    T.Spades `atLeastAsLong` T.Diamonds
    T.Spades `atLeastAsLong` T.Hearts
primarySuit_ T.Notrump = error "notrump is not a primary suit"


gfWithSuit_ :: T.Suit -> T.Suit -> Action
gfWithSuit_ ourSuit oppsSuit = do
    NT.gameForcing
    minSuitLength ourSuit 5
    primarySuit_ ourSuit
    -- It's no fun when you bid a suit headed by the queen-nine. Probably do
    -- that at the table, but practice the more obvious holdings instead.
    soundHolding ourSuit
    -- If we're 5332 with a stopper in the opponent's suit, prefer notrump.
    forbid (balancedHand >> hasStopper oppsSuit)
    makeCall $ T.Bid 3 ourSuit


-- With 5-5 in two suits, bid the higher one, so you can rebid the lower one
-- later.
b1No2D3C :: Action
b1No2D3C = gfWithSuit_ T.Clubs T.Diamonds

b1No2H3C :: Action
b1No2H3C = gfWithSuit_ T.Clubs T.Hearts

b1No2S3C :: Action
b1No2S3C = gfWithSuit_ T.Clubs T.Spades

b1No2C3D :: Action
b1No2C3D = gfWithSuit_ T.Diamonds T.Clubs

b1No2H3D :: Action
b1No2H3D = gfWithSuit_ T.Diamonds T.Hearts

b1No2S3D :: Action
b1No2S3D = gfWithSuit_ T.Diamonds T.Spades

b1No2C3H :: Action
b1No2C3H = gfWithSuit_ T.Hearts T.Clubs

b1No2D3H :: Action
b1No2D3H = gfWithSuit_ T.Hearts T.Diamonds

b1No2S3H :: Action
b1No2S3H = gfWithSuit_ T.Hearts T.Spades

b1No2C3S :: Action
b1No2C3S = gfWithSuit_ T.Spades T.Clubs

b1No2D3S :: Action
b1No2D3S = gfWithSuit_ T.Spades T.Diamonds

b1No2H3S :: Action
b1No2H3S = gfWithSuit_ T.Spades T.Hearts


-- Signoffs that you couldn't bid at the 2 level
b1No2D2N3CP :: Action
b1No2D2N3CP = do
    withholdBid $ signoff_ 8 T.Clubs
    makeCall T.Pass

b1No2H2N3CP :: Action
b1No2H2N3CP = do
    withholdBid $ signoff_ 8 T.Clubs
    makeCall T.Pass

b1No2S2N3CP :: Action
b1No2S2N3CP = do
    withholdBid $ signoff_ 8 T.Clubs
    makeCall T.Pass

b1No2H2N3C3D :: Action
b1No2H2N3C3D = signoff_ 3 T.Diamonds

b1No2S2N3C3D :: Action
b1No2S2N3C3D = signoff_ 3 T.Diamonds

b1No2S2N3C3H :: Action
b1No2S2N3C3H = signoff_ 3 T.Hearts


-- Invites for higher suits
invite_ :: T.Suit -> Action
invite_ suit = do
    NT.invitational
    minSuitLength suit 5
    primarySuit_ suit
    -- It's no fun when you bid a suit headed by the queen-nine. Probably do
    -- that at the table, but practice the more obvious holdings instead.
    soundHolding suit
    makeCall $ T.Bid 3 suit


b1No2D2N3C3H :: Action
b1No2D2N3C3H = invite_ T.Hearts

b1No2D2N3C3S :: Action
b1No2D2N3C3S = invite_ T.Spades

b1No2H2N3C3S :: Action
b1No2H2N3C3S = invite_ T.Spades


bid3N_ :: [T.Suit] -> Bool -> Action
bid3N_ theirSuits shouldHaveStopper = do
    NT.gameForcing
    balancedHand
    forbidAll [b1No2D3H, b1No2D3S, cueBid_ (head theirSuits) shouldHaveStopper]
    when shouldHaveStopper (forEach theirSuits hasStopper)
    makeCall $ T.Bid 3 T.Notrump

b1No2D3N :: Action
b1No2D3N = bid3N_ [T.Diamonds] False

b1No2D2N3C3N :: Action
b1No2D2N3C3N = bid3N_ [T.Diamonds] True

b1No2H3N :: Action
b1No2H3N = bid3N_ [T.Hearts] False

b1No2H2N3C3N :: Action
b1No2H2N3C3N = bid3N_ [T.Hearts] True

b1No2S3N :: Action
b1No2S3N = bid3N_ [T.Spades] False

b1No2S2N3C3N :: Action
b1No2S2N3C3N = bid3N_ [T.Spades] True

b1NoBM3N :: Action
b1NoBM3N = do
    -- Even if we don't have stoppers in both majors, we should have a stopper
    -- in at least one of them to make this bid plausible.
    alternatives [hasStopper T.Hearts, hasStopper T.Spades]
    bid3N_ [T.Hearts, T.Spades] False

b1NoBM2N3C3N :: Action
b1NoBM2N3C3N = bid3N_ [T.Hearts, T.Spades] True


-- Stayman-like cue bids
cueBid_ :: T.Suit -> Bool -> Action
cueBid_ oppsSuit shouldHaveStopper = do
    NT.gameForcing
    semibalancedHand
    -- TODO: if the opponents overcall a natural 2D, do you need exactly 4-4 in
    -- the majors? What if you're 4-3? What if you're 5-4?
    forEach (filter (/= oppsSuit) T.majorSuits) (`suitLength` 4)
    when shouldHaveStopper (hasStopper oppsSuit)
    -- Simultaneously, make sure you don't want to just double the opponents
    -- because you've got their suit. This isn't so important when they made a
    -- 2-suited bid (they'd just run to the second suit), but this part of the
    -- code doesn't know about that. Assume it's just a natural bid.
    maxSuitLength oppsSuit 4
    makeAlertableCall (T.Bid 3 oppsSuit)
                      ("Stayman with" ++
                           (if shouldHaveStopper then "" else "out") ++
                           " a stopper")

b1No2D3D :: Action
b1No2D3D = cueBid_ T.Diamonds False

b1No2D2N3C3D :: Action
b1No2D2N3C3D = cueBid_ T.Diamonds True

b1No2H3H :: Action
b1No2H3H = cueBid_ T.Hearts False

b1No2H2N3C3H :: Action
b1No2H2N3C3H = cueBid_ T.Hearts True

b1No2S3S :: Action
b1No2S3S = cueBid_ T.Spades False

b1No2S2N3C3S :: Action
b1No2S2N3C3S = cueBid_ T.Spades True


-- Time for the actual lebensohl relays!
b1N2N3C :: Action
b1N2N3C = makeAlertableCall (T.Bid 3 T.Clubs) "relay completed"


b1No2D2N :: Action
b1No2D2N = do
    alternatives [ b1No2D2N3CP
                 , b1No2D2N3C3D
                 , b1No2D2N3C3H
                 , b1No2D2N3C3S
                 , b1No2D2N3C3N
                 ]
    makeAlertableCall (T.Bid 2 T.Notrump) ("relay to " .+ T.Bid 3 T.Clubs)


b1No2H2N :: Action
b1No2H2N = do
    alternatives [ b1No2H2N3CP
                 , b1No2H2N3C3D
                 , b1No2H2N3C3H
                 , b1No2D2N3C3S
                 , b1No2D2N3C3N
                 ]
    makeAlertableCall (T.Bid 2 T.Notrump) ("relay to " .+ T.Bid 3 T.Clubs)


b1No2S2N :: Action
b1No2S2N = do
    alternatives [ b1No2S2N3CP
                 , b1No2S2N3C3D
                 , b1No2S2N3C3H
                 , b1No2S2N3C3S
                 , b1No2S2N3C3N
                 ]
    makeAlertableCall (T.Bid 2 T.Notrump) ("relay to " .+ T.Bid 3 T.Clubs)


-- Have a special one for when the opponents show both majors
b1NoBM2N :: Action
b1NoBM2N = do
    alternatives [ b1No2S2N3CP
                 , b1No2S2N3C3D
                 , b1NoBM2N3C3N
                 -- Don't bother with a Stayman-like bid: RHO has both majors
                 ]
    makeAlertableCall (T.Bid 2 T.Notrump) ("relay to " .+ T.Bid 3 T.Clubs)
