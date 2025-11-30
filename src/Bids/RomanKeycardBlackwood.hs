module Bids.RomanKeycardBlackwood(
    bRKC4N
  , bRKC1430H5C
  , bRKC1430H5D
  , bRKC3014H5C
  , bRKC3014H5D
  , bRKCH5H
  , bRKCH5S
  , bRKC1430S5C
  , bRKC1430S5D
  , bRKC3014S5C
  , bRKC3014S5D
  , bRKCS5H
  , bRKCS5S
) where


import Action(Action)
import qualified EDSL as E
import qualified Terminology as T


bRKC4N :: Action
bRKC4N = E.makeAlertableCall (T.Bid 4 T.Notrump) "(postalert) Keycard Ask"


bRKC1430H5C :: Action
bRKC1430H5C = E.nameAction "RKC1430_H_5C" $ do
    E.keycardCount T.Hearts 1 4
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 1 or 4 keycards"


bRKC1430H5D :: Action
bRKC1430H5D = E.nameAction "RKC1430_H_5D" $ do
    E.keycardCount T.Hearts 3 0
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 3 or 0 keycards"


bRKCH5H :: Action
bRKCH5H = E.nameAction "RKC_H_5H" $ do
    E.keycardCount T.Hearts 2 5
    E.forbid (E.hasCard T.Hearts 'Q')
    E.makeAlertableCall (T.Bid 5 T.Hearts) "(postalert) 2 or 5 keycards w/o Q"


bRKCH5S :: Action
bRKCH5S = E.nameAction "RKC_H_5S" $ do
    E.keycardCount T.Hearts 2 5
    E.hasCard T.Hearts 'Q'
    E.makeAlertableCall (T.Bid 5 T.Spades) "(postalert) 2 or 5 keycards with Q"


bRKC1430S5C :: Action
bRKC1430S5C = E.nameAction "RKC1430_S_5C" $ do
    E.keycardCount T.Spades 1 4
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 1 or 4 keycards"


bRKC1430S5D :: Action
bRKC1430S5D = E.nameAction "RKC1430_S_5D" $ do
    E.keycardCount T.Spades 3 0
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 3 or 0 keycards"


bRKCS5H :: Action
bRKCS5H = E.nameAction "RKC_S_5H" $ do
    E.keycardCount T.Spades 2 5
    E.forbid (E.hasCard T.Spades 'Q')
    E.makeAlertableCall (T.Bid 5 T.Hearts) "(postalert) 2 or 5 keycards w/o Q"


bRKCS5S :: Action
bRKCS5S = E.nameAction "RKC_S_5S" $ do
    E.keycardCount T.Spades 2 5
    E.hasCard T.Spades 'Q'
    E.makeAlertableCall (T.Bid 5 T.Spades) "(postalert) 2 or 5 keycards with Q"


-- For RKC3014, reuse as much as possible. To keep changes to the dealer program
-- while ignoring the bids, forbid forbidding the original.

bRKC3014H5C :: Action
bRKC3014H5C = E.nameAction "RKC3014_H_5C" $ do
    E.forbid (E.forbid bRKC1430H5D)
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 3 or 0 keycards"

bRKC3014H5D :: Action
bRKC3014H5D = E.nameAction "RKC3014_H_5D" $ do
    E.forbid (E.forbid bRKC1430H5C)
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 1 or 4 keycards"

bRKC3014S5C :: Action
bRKC3014S5C = E.nameAction "RKC3014_S_5C" $ do
    E.forbid (E.forbid bRKC1430S5D)
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 3 or 0 keycards"

bRKC3014S5D :: Action
bRKC3014S5D = E.nameAction "RKC3014_S_5D" $ do
    E.forbid (E.forbid bRKC1430S5C)
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 1 or 4 keycards"
