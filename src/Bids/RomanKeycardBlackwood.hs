module Bids.RomanKeycardBlackwood(
    b4N

  , b1430H5C
  , b1430H5C5D
  , b1430H5C5H
  , b1430H5D
  , b1430H5D5H
  , b1430H5D5S
  , b3014H5C
  , b3014H5C5D
  , b3014H5C5H
  , b3014H5D
  , b3014H5D5H
  , b3014H5D5S

  -- Queen ask responses are the same for 1430 and 3014.
  , bH5C5D5H
  , bH5C5D5S
  --, bH5C5D5N  -- Too hard to define
  , bH5C5D6C
  , bH5C5D6D
  , bH5C5D6H
--  , bH5D5S5N
--  , bH5D5S6C
--  , bH5D5S6D
  , bH5D5S6H

  , bH5H
  , bH5HP
  , bH5S
  , bH5N
  , bH6C
  , bH6D
  , bH6H

  , b1430S5C
  , b1430S5C5D
  , b1430S5C5S
  , b1430S5D
  , b1430S5D5H
  , b1430S5D5S
  , b3014S5C
  , b3014S5C5D
  , b3014S5C5S
  , b3014S5D
  , b3014S5D5H
  , b3014S5D5S

  -- Queen ask responses
--  , bS5C5D5H
  , bS5C5D5S
  --, bS5C5D5N  -- Too hard to define
--  , bS5C5D6C
--  , bS5C5D6D
--  , bS5C5D6S
  , bS5D5H5S
  --, bS5D5H5N  -- Too hard to define
--  , bS5D5H6C
--  , bS5D5H6D
--  , bS5D5H6H
--  , bS5D5H6S

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
import Output((.+))
import qualified Terminology as T


b4N :: Action
b4N = E.nameAction "RKC_4N" $ do
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


-- Queen asks
queenAsk_ :: T.Suit -> Action -> T.Call -> Action
queenAsk_ trumpSuit signoff bid = do
    E.forbid signoff  -- We must be missing at most 1 keycard
    E.forbid (E.hasCard trumpSuit 'Q')  -- We must not know where the queen is
    -- NOTE: we should also not know we have a 10-card fit, but that's much
    -- harder to describe, and probably depends on the preceding bids.
    E.makeAlertableCall bid "(postalert) queen ask"

b1430H5C5D :: Action
b1430H5C5D = E.nameAction "RKC1430_H_5C5D" $ do
    queenAsk_ T.Hearts b1430H5C5H (T.Bid 5 T.Diamonds)

b1430H5D5S :: Action
b1430H5D5S = E.nameAction "RKC1430_H_5D5S" $ do
    -- This bid obligates us to slam even if partner is missing the queen. To be
    -- on the safe side, we should have all the keycards.
    E.keycardCount T.Hearts 2 5
    queenAsk_ T.Hearts b1430H5D5H (T.Bid 5 T.Spades)

b3014H5C5D :: Action
b3014H5C5D = E.nameAction "RKC3014_H_5C5D" $ do
    queenAsk_ T.Hearts b3014H5C5H (T.Bid 5 T.Diamonds)

b3014H5D5S :: Action
b3014H5D5S = E.nameAction "RKC3014_H_5D5S" $ do
    -- This bid obligates us to slam even if partner is missing the queen. To be
    -- on the safe side, we should have all the keycards.
    E.keycardCount T.Hearts 1 4
    queenAsk_ T.Hearts b3014H5D5H (T.Bid 5 T.Spades)

b1430S5C5D :: Action
b1430S5C5D = E.nameAction "RKC1430_S_5C5D" $ do
    queenAsk_ T.Spades b1430S5C5S (T.Bid 5 T.Diamonds)

b1430S5D5H :: Action
b1430S5D5H = E.nameAction "RKC1430_S_5D5H" $ do
    queenAsk_ T.Spades b1430S5D5S (T.Bid 5 T.Hearts)

b3014S5C5D :: Action
b3014S5C5D = E.nameAction "RKC3014_S_5C5D" $ do
    queenAsk_ T.Spades b3014S5C5S (T.Bid 5 T.Diamonds)

b3014S5D5H :: Action
b3014S5D5H = E.nameAction "RKC3014_S_5D5H" $ do
    queenAsk_ T.Spades b3014S5D5S (T.Bid 5 T.Hearts)


-- Queen ask responses

bH5C5D5H :: Action
bH5C5D5H = E.nameAction "RKC_H_5C5D5H" $ do
    E.forbid $ E.hasCard T.Hearts 'Q'
    E.makeAlertableCall (T.Bid 5 T.Hearts) "no queen of trump"

bH5D5S6H :: Action
bH5D5S6H = E.nameAction "RKC_H_5D5S6H" $ do
    E.forbid $ E.hasCard T.Hearts 'Q'
    E.makeAlertableCall (T.Bid 6 T.Hearts) "no queen of trump"

bS5C5D5S :: Action
bS5C5D5S = E.nameAction "RKC_S_5C5D5S" $ do
    E.forbid $ E.hasCard T.Spades 'Q'
    E.makeAlertableCall (T.Bid 5 T.Spades) "no queen of trump"

bS5D5H5S :: Action
bS5D5H5S = E.nameAction "RKC_S_5D5H5S" $ do
    E.forbid $ E.hasCard T.Spades 'Q'
    E.makeAlertableCall (T.Bid 5 T.Spades) "no queen of trump"


bH5C5D5S :: Action
bH5C5D5S = E.nameAction "RKC_H_5C5D5S" $ do
    E.hasCard T.Hearts 'Q'
    E.hasCard T.Spades 'K'
    E.makeAlertableCall (T.Bid 5 T.Spades)
                        (T.Hearts .+ "Q, " .+ T.Spades .+ "K")

bH5C5D6C :: Action
bH5C5D6C = E.nameAction "RKC_H_5C5D6C" $ do
    E.hasCard T.Hearts 'Q'
    E.forbid $ E.hasCard T.Spades 'K'
    E.hasCard T.Clubs 'K'
    E.makeAlertableCall (T.Bid 6 T.Clubs)
                        (T.Hearts .+ "Q, " .+ T.Clubs .+ "K, " .+
                         "no " .+ T.Spades .+ "K")

bH5C5D6D :: Action
bH5C5D6D = E.nameAction "RKC_H_5C5D6D" $ do
    E.hasCard T.Hearts 'Q'
    E.forbid $ E.hasCard T.Spades 'K'
    E.forbid $ E.hasCard T.Clubs 'K'
    E.hasCard T.Diamonds 'K'
    E.makeAlertableCall (T.Bid 6 T.Diamonds)
                        (T.Hearts .+ "Q, " .+ T.Diamonds .+ "K, no black king")

bH5C5D6H :: Action
bH5C5D6H = E.nameAction "RKC_H_5C5D6H" $ do
    E.hasCard T.Hearts 'Q'
    E.forbid $ E.hasCard T.Spades 'K'
    E.forbid $ E.hasCard T.Clubs 'K'
    E.forbid $ E.hasCard T.Diamonds 'K'
    E.makeAlertableCall (T.Bid 6 T.Hearts)
                        (T.Hearts .+ "Q, no side king")

