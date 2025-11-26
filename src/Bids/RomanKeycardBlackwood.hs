module Bids.RomanKeycardBlackwood(
    bRKC4N
  , b1430RKCH5C
  , b1430RKCH5D
  , b3014RKCH5C
  , b3014RKCH5D
  , bRKCH5H
  , bRKCH5S
  , b1430RKCS5C
  , b1430RKCS5D
  , b3014RKCS5C
  , b3014RKCS5D
  , bRKCS5H
  , bRKCS5S
) where


import Action(Action)
import qualified EDSL as E
import qualified Terminology as T


bRKC4N :: Action
bRKC4N = E.makeAlertableCall (T.Bid 4 T.Notrump) "(postalert) Keycard Ask"


b1430RKCH5C :: Action
b1430RKCH5C = E.nameAction "RKC1430_H_5C" $ do
    E.keycardCount T.Hearts 1 4
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 1 or 4 keycards"


b1430RKCH5D :: Action
b1430RKCH5D = E.nameAction "RKC1430_H_5D" $ do
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


b1430RKCS5C :: Action
b1430RKCS5C = E.nameAction "RKC1430_S_5C" $ do
    E.keycardCount T.Spades 1 4
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 1 or 4 keycards"


b1430RKCS5D :: Action
b1430RKCS5D = E.nameAction "RKC1430_S_5D" $ do
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

b3014RKCH5C :: Action
b3014RKCH5C = E.nameAction "RKC3014_H_5C" $ do
    E.forbid (E.forbid b1430RKCH5D)
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 3 or 0 keycards"

b3014RKCH5D :: Action
b3014RKCH5D = E.nameAction "RKC3014_H_5D" $ do
    E.forbid (E.forbid b1430RKCH5C)
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 1 or 4 keycards"

b3014RKCS5C :: Action
b3014RKCS5C = E.nameAction "RKC3014_S_5C" $ do
    E.forbid (E.forbid b1430RKCS5D)
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 3 or 0 keycards"

b3014RKCS5D :: Action
b3014RKCS5D = E.nameAction "RKC3014_S_5D" $ do
    E.forbid (E.forbid b1430RKCS5C)
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 1 or 4 keycards"
