module Bids.TwoOverOne(
  -- Opening bids are re-exported from StandardOpenings.hs
    b1D
  , b1D2C
  , b1H
  , b1H2C
  , b1H2D
  , b1S
  , b1S2C
  , b1S2D
  , b1S2H
) where

import Action(Action)
import qualified EDSL as E
import qualified Terminology as T

import Bids.StandardOpenings(b1D, b1H, b1S)


_gameForcing :: Action
_gameForcing = E.pointRange 12 40


b1D2C :: Action
b1D2C = E.nameAction "b1D2C" $ do
    _gameForcing
    E.forEach T.majorSuits (`E.maxSuitLength` 3)
    E.minSuitLength T.Clubs 5
    -- If you're 5-5 in the minors, probably support partner's suit
    T.Clubs `E.longerThan` T.Diamonds
    E.forbid E.balancedHand
    E.makeCall $ T.Bid 2 T.Clubs


b1H2C :: Action
b1H2C = E.nameAction "b1H2C" $ do
    _gameForcing
    E.maxSuitLength T.Spades 3
    E.maxSuitLength T.Hearts 3
    E.minSuitLength T.Clubs 4
    -- With 5-5 or even 4-4 in the minors, bid diamonds first
    T.Clubs `E.longerThan` T.Diamonds
    E.forbid E.balancedHand
    E.makeCall $ T.Bid 2 T.Clubs


b1H2D :: Action
b1H2D = E.nameAction "b1H2D" $ do
    _gameForcing
    E.maxSuitLength T.Spades 3
    E.maxSuitLength T.Hearts 3
    E.minSuitLength T.Diamonds 4
    T.Diamonds `E.atLeastAsLong` T.Clubs
    E.forbid E.balancedHand
    E.makeCall $ T.Bid 2 T.Diamonds


b1S2C :: Action
b1S2C = E.nameAction "b1S2C" $ do
    _gameForcing
    E.maxSuitLength T.Spades 3
    E.minSuitLength T.Clubs 4
    -- With 5-5 or even 4-4 in the minors, start with diamonds. No need to
    -- reverse if we're already in a GF auction.
    T.Clubs `E.longerThan` T.Diamonds
    T.Clubs `E.atLeastAsLong` T.Hearts
    -- With 5-5 in the rounded suits, start with the major. With 4-4, start with
    -- the cheaper suit. This way, bidding 2H always shows a 5-card suit.
    E.minSuitLength T.Clubs 5 `E.impliesThat`
        (T.Clubs `E.longerThan` T.Hearts)
    E.forbid E.balancedHand
    E.makeCall $ T.Bid 2 T.Clubs


b1S2D :: Action
b1S2D = E.nameAction "b1S2D" $ do
    _gameForcing
    E.maxSuitLength T.Spades 3
    E.minSuitLength T.Diamonds 4
    T.Diamonds `E.atLeastAsLong` T.Clubs
    T.Diamonds `E.atLeastAsLong` T.Hearts
    -- With 5-5 in the reds, prefer hearts. With 4-4, prefer the cheaper suit.
    -- This way, bidding 2H always shows a 5-card suit.
    E.minSuitLength T.Diamonds 5 `E.impliesThat`
        (T.Diamonds `E.longerThan` T.Hearts)
    E.forbid E.balancedHand
    E.makeCall $ T.Bid 2 T.Diamonds


b1S2H :: Action
b1S2H = E.nameAction "b1S2H" $ do
    _gameForcing
    E.maxSuitLength T.Spades 3
    E.minSuitLength T.Hearts 5
    -- If you're 5-5, definitely prefer showing the major first
    E.forEach T.minorSuits (T.Hearts `E.atLeastAsLong`)
    E.forbid E.balancedHand
    E.makeCall $ T.Bid 2 T.Hearts
