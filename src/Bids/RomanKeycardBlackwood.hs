module Bids.RomanKeycardBlackwood(
    b4N
  , b1430H5C
  , b1430H5D
  , b3014H5C
  , b3014H5D
  , bH5H
  , bH5S
  , bH5N
  , b1430S5C
  , b1430S5D
  , b3014S5C
  , b3014S5D
  , bS5H
  , bS5S
  , bS5N
) where


import Action(Action)
import qualified EDSL as E
import qualified Terminology as T


b4N :: Action
b4N = E.makeAlertableCall (T.Bid 4 T.Notrump) "(postalert) keycard ask"


b1430H5C :: Action
b1430H5C = E.nameAction "RKC1430_H_5C" $ do
    E.keycardCount T.Hearts 1 4
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 1 or 4 keycards"


b1430H5D :: Action
b1430H5D = E.nameAction "RKC1430_H_5D" $ do
    E.keycardCount T.Hearts 3 0
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 3 or 0 keycards"


bH5H :: Action
bH5H = E.nameAction "RKC_H_5H" $ do
    E.keycardCount T.Hearts 2 5
    E.forbid (E.hasCard T.Hearts 'Q')
    E.makeAlertableCall (T.Bid 5 T.Hearts) "(postalert) 2 or 5 keycards w/o Q"


bH5S :: Action
bH5S = E.nameAction "RKC_H_5S" $ do
    E.keycardCount T.Hearts 2 5
    E.hasCard T.Hearts 'Q'
    E.makeAlertableCall (T.Bid 5 T.Spades) "(postalert) 2 or 5 keycards with Q"


b1430S5C :: Action
b1430S5C = E.nameAction "RKC1430_S_5C" $ do
    E.keycardCount T.Spades 1 4
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 1 or 4 keycards"


b1430S5D :: Action
b1430S5D = E.nameAction "RKC1430_S_5D" $ do
    E.keycardCount T.Spades 3 0
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 3 or 0 keycards"


bS5H :: Action
bS5H = E.nameAction "RKC_S_5H" $ do
    E.keycardCount T.Spades 2 5
    E.forbid (E.hasCard T.Spades 'Q')
    E.makeAlertableCall (T.Bid 5 T.Hearts) "(postalert) 2 or 5 keycards w/o Q"


bS5S :: Action
bS5S = E.nameAction "RKC_S_5S" $ do
    E.keycardCount T.Spades 2 5
    E.hasCard T.Spades 'Q'
    E.makeAlertableCall (T.Bid 5 T.Spades) "(postalert) 2 or 5 keycards with Q"


b5N_ :: T.Suit -> Action
b5N_ trumpSuit = do
    -- With 0 keycards, unclear if you really want to show your void. Skip that
    -- situation entirely.
    -- TODO: What is the right choice there?
    E.keycardCount trumpSuit 2 4
    E.atLeastOneOf (filter (/= trumpSuit) T.allSuits) (`E.suitLength` 0)
    E.makeAlertableCall (T.Bid 5 T.Notrump)
        "(postalert) even number of keycards with a void"

bH5N :: Action
bH5N = E.nameAction "RKC_H_5N" (b5N_ T.Hearts)


bS5N :: Action
bS5N = E.nameAction "RKC_S_5N" (b5N_ T.Spades)


-- For RKC3014, reuse as much as possible. To keep changes to the dealer program
-- while ignoring the bids, forbid forbidding the original.

b3014H5C :: Action
b3014H5C = E.nameAction "RKC3014_H_5C" $ do
    E.forbid (E.forbid b1430H5D)
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 3 or 0 keycards"

b3014H5D :: Action
b3014H5D = E.nameAction "RKC3014_H_5D" $ do
    E.forbid (E.forbid b1430H5C)
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 1 or 4 keycards"

b3014S5C :: Action
b3014S5C = E.nameAction "RKC3014_S_5C" $ do
    E.forbid (E.forbid b1430S5D)
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 3 or 0 keycards"

b3014S5D :: Action
b3014S5D = E.nameAction "RKC3014_S_5D" $ do
    E.forbid (E.forbid b1430S5C)
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 1 or 4 keycards"
