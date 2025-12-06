module Bids.RomanKeycardBlackwood(
    b4N
  , b1430H5C
  , b1430H5C5H
  , b1430H5D
  , b1430H5D5H
  , b3014H5C
  , b3014H5C5H
  , b3014H5D
  , b3014H5D5H
  , bH5H
  , bH5HP
  , bH5S
  , bH5N
  , bH6C
  , bH6D
  , bH6H
  , b1430S5C
  , b1430S5C5S
  , b1430S5D
  , b1430S5D5S
  , b3014S5C
  , b3014S5C5S
  , b3014S5D
  , b3014S5D5S
  , bS5H
  , bS5H5S
  , bS5S
  , bS5SP
  , bS5N
  , bS6C
  , bS6D
  , bS6H
) where


import Action(Action)
import qualified EDSL as E
import qualified Terminology as T


b4N :: Action
b4N = do
    E.forEach T.allSuits (`E.minSuitLength` 1)  -- Don't bid RKC with a void
    E.makeAlertableCall (T.Bid 4 T.Notrump) "(postalert) keycard ask"


b1430H5C :: Action
b1430H5C = E.nameAction "RKC1430_H_5C" $ do
    E.forbidAll [bH5N, bH6C, bH6D, bH6H]
    E.keycardCount T.Hearts 1 4
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 1 or 4 keycards"


b1430H5D :: Action
b1430H5D = E.nameAction "RKC1430_H_5D" $ do
    E.forbidAll [bH5N, bH6C, bH6D, bH6H]
    E.keycardCount T.Hearts 3 0
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 3 or 0 keycards"


bH5H :: Action
bH5H = E.nameAction "RKC_H_5H" $ do
    E.forbidAll [bH5N, bH6C, bH6D, bH6H]
    E.keycardCount T.Hearts 2 5
    E.forbid (E.hasCard T.Hearts 'Q')
    E.makeAlertableCall (T.Bid 5 T.Hearts) "(postalert) 2 or 5 keycards w/o Q"


bH5S :: Action
bH5S = E.nameAction "RKC_H_5S" $ do
    E.forbidAll [bH5N, bH6C, bH6D, bH6H]
    E.keycardCount T.Hearts 2 5
    E.hasCard T.Hearts 'Q'
    E.makeAlertableCall (T.Bid 5 T.Spades) "(postalert) 2 or 5 keycards with Q"


b1430S5C :: Action
b1430S5C = E.nameAction "RKC1430_S_5C" $ do
    E.forbidAll [bS5N, bS6C, bS6D, bS6H]
    E.keycardCount T.Spades 1 4
    E.makeAlertableCall (T.Bid 5 T.Clubs) "(postalert) 1 or 4 keycards"


b1430S5D :: Action
b1430S5D = E.nameAction "RKC1430_S_5D" $ do
    E.forbidAll [bS5N, bS6C, bS6D, bS6H]
    E.keycardCount T.Spades 3 0
    E.makeAlertableCall (T.Bid 5 T.Diamonds) "(postalert) 3 or 0 keycards"


bS5H :: Action
bS5H = E.nameAction "RKC_S_5H" $ do
    E.forbidAll [bS5N, bS6C, bS6D, bS6H]
    E.keycardCount T.Spades 2 5
    E.forbid (E.hasCard T.Spades 'Q')
    E.makeAlertableCall (T.Bid 5 T.Hearts) "(postalert) 2 or 5 keycards w/o Q"


bS5S :: Action
bS5S = E.nameAction "RKC_S_5S" $ do
    E.forbidAll [bS5N, bS6C, bS6D, bS6H]
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


b6x_ :: T.Suit -> T.Suit -> T.Suit -> Action
b6x_ trumpSuit voidSuit bidSuit = do
    E.alternatives [E.keycardCount trumpSuit 1 3, E.keycardCount trumpSuit 5 5]
    E.suitLength voidSuit 0
    E.makeAlertableCall (T.Bid 6 bidSuit)
        ("(postalert) odd number of keycards with a " ++
            (init . show $ voidSuit) ++ " void")

bH6C :: Action
bH6C = E.nameAction "RKC_H_6C" (b6x_ T.Hearts T.Clubs    T.Clubs)

bH6D :: Action
bH6D = E.nameAction "RKC_H_6D" (b6x_ T.Hearts T.Diamonds T.Diamonds)

bH6H :: Action
bH6H = E.nameAction "RKC_H_6H" (b6x_ T.Hearts T.Spades   T.Hearts)

bS6C :: Action
bS6C = E.nameAction "RKC_S_6C" (b6x_ T.Spades T.Clubs    T.Clubs)

bS6D :: Action
bS6D = E.nameAction "RKC_S_6D" (b6x_ T.Spades T.Diamonds T.Diamonds)

bS6H :: Action
bS6H = E.nameAction "RKC_S_6H" (b6x_ T.Spades T.Hearts   T.Hearts)


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


-- Signoffs
b1430H5C5H :: Action
b1430H5C5H = E.nameAction "RKC1430_H_5C5H" $ do
    E.keycardCount T.Hearts 2 5
    E.makeCall $ T.Bid 5 T.Hearts


b1430H5D5H :: Action
b1430H5D5H = E.nameAction "RKC1430_H_5D5H" $ do
    E.keycardCount T.Hearts 3 0
    E.makeCall $ T.Bid 5 T.Hearts


b3014H5C5H :: Action
b3014H5C5H = E.nameAction "RKC3014_H_5C5H" $ do
    E.keycardCount T.Hearts 3 0
    E.makeCall $ T.Bid 5 T.Hearts


b3014H5D5H :: Action
b3014H5D5H = E.nameAction "RKC3014_H_5D5H" $ do
    E.keycardCount T.Hearts 2 5
    E.makeCall $ T.Bid 5 T.Hearts


bH5HP :: Action
bH5HP = E.nameAction "RKC1430_H_5HP" $ do
    E.keycardCount T.Hearts 1 4
    E.makeCall $ T.Pass


b1430S5C5S :: Action
b1430S5C5S = E.nameAction "RKC1430_S_5C5S" $ do
    E.keycardCount T.Spades 2 5
    E.makeCall $ T.Bid 5 T.Spades


b1430S5D5S :: Action
b1430S5D5S = E.nameAction "RKC1430_S_5D5S" $ do
    E.keycardCount T.Spades 3 0
    E.makeCall $ T.Bid 5 T.Spades


b3014S5C5S :: Action
b3014S5C5S = E.nameAction "RKC3014_S_5C5S" $ do
    E.keycardCount T.Spades 3 0
    E.makeCall $ T.Bid 5 T.Spades


b3014S5D5S :: Action
b3014S5D5S = E.nameAction "RKC3014_S_5D5S" $ do
    E.keycardCount T.Spades 2 5
    E.makeCall $ T.Bid 5 T.Spades


bS5H5S :: Action
bS5H5S = E.nameAction "RKC1430_S_5H5S" $ do
    E.keycardCount T.Spades 1 4
    E.makeCall $ T.Bid 5 T.Spades


bS5SP :: Action
bS5SP = E.nameAction "RKC1430_S_5SP" $ do
    E.keycardCount T.Spades 1 4
    E.makeCall $ T.Pass
