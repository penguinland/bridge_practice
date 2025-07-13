module Bids.MuppetStayman(
    noInterference  -- re-exported from PuppetStayman
  , b2N             -- re-exported from StandardOpenings and/or PuppetStayman
  , b2N3C
  , b2N3C3D         -- re-exported from PuppetStayman
  , b2N3C3D3H       -- re-exported from PuppetStayman
  , b2N3C3D3H3S     -- re-exported from PuppetStayman
  , b2N3C3D3H3N     -- re-exported from PuppetStayman
  , b2N3C3D3S       -- re-exported from PuppetStayman
  , b2N3C3D3S3N     -- re-exported from PuppetStayman
  , b2N3C3D3S4H     -- re-exported from PuppetStayman
  , b2N3C3D3N       -- re-exported from PuppetStayman
  , b2N3C3D4D       -- re-exported from PuppetStayman
  , b2N3C3D4D4H     -- re-exported from PuppetStayman
  , b2N3C3D4D4S     -- re-exported from PuppetStayman
  , b2N3C3H
  , b2N3C3H3S
  , b2N3C3H3S3N
  , b2N3C3H3N
  , b2N3C3H3NP
  , b2N3C3H3N4S
  , b2N3C3H4D
  , b2N3C3H4D4H
  , b2N3C3H4H
  , b2N3C3H4H4S
  , b2N3C3S         -- re-exported from PuppetStayman
  , b2N3C3S3N       -- re-exported from PuppetStayman
  , b2N3C3S4H       -- re-exported from PuppetStayman
  , b2N3C3S4S       -- re-exported from PuppetStayman
  , b2N3C3N
  , b2N3C3NP
  , b2N3C3N4D
  , b2N3C3N4D4H
) where


import Action(Action)
import qualified Bids.PuppetStayman as P
import Bids.NaturalOneNotrumpDefense(singleSuited, twoSuited)
import qualified EDSL as E
import Output((.+))
import qualified Terminology as T


-- Re-export the puppet Stayman bids: responses of 3D and 3S are unchanged.
noInterference :: Action
noInterference = P.noInterference
b2N         :: Action
b2N         = P.b2N
b2N3C3D     :: Action
b2N3C3D     = P.b2N3C3D
b2N3C3D3H   :: Action
b2N3C3D3H   = P.b2N3C3D3H
b2N3C3D3H3S :: Action
b2N3C3D3H3S = P.b2N3C3D3H3S
b2N3C3D3H3N :: Action
b2N3C3D3H3N = P.b2N3C3D3H3N
b2N3C3D3S   :: Action
b2N3C3D3S   = P.b2N3C3D3S
b2N3C3D3S3N :: Action
b2N3C3D3S3N = P.b2N3C3D3S3N
b2N3C3D3S4H :: Action
b2N3C3D3S4H = P.b2N3C3D3S4H
b2N3C3D3N   :: Action
b2N3C3D3N   = P.b2N3C3D3N
b2N3C3D4D   :: Action
b2N3C3D4D   = P.b2N3C3D4D
b2N3C3D4D4H :: Action
b2N3C3D4D4H = P.b2N3C3D4D4H
b2N3C3D4D4S :: Action
b2N3C3D4D4S = P.b2N3C3D4D4S
b2N3C3S     :: Action
b2N3C3S     = P.b2N3C3S
b2N3C3S3N   :: Action
b2N3C3S3N   = P.b2N3C3S3N
b2N3C3S4H   :: Action
b2N3C3S4H   = P.b2N3C3S4H
b2N3C3S4S   :: Action
b2N3C3S4S   = P.b2N3C3S4S


b2N3C :: Action
b2N3C = E.nameAction "muppet_b2N3C" $ do
    E.pointRange 5 40  -- game forcing
    E.alternatives [E.minSuitLength T.Hearts 3, E.minSuitLength T.Spades 3]
    -- If you could transfer to spades, bid Muppet Stayman if you have any
    -- interest in hearts at all. However, if you're 5-5 in the majors, transfer
    -- to one and then bid the other yourself, rather than looking for an
    -- unlikely 5-4 or 5-5 fit.
    E.minSuitLength T.Spades 5 `E.impliesThat` E.minSuitLength T.Hearts 3
    E.minSuitLength T.Spades 5 `E.impliesThat` E.maxSuitLength T.Hearts 4
    -- If you could transfer to hearts, prefer puppet Stayman only if you've
    -- got exactly 4 spades: with 5 or more, transfer to one and then bid the
    -- other, and with 3 or fewer, just show the hearts.
    E.minSuitLength T.Hearts 5 `E.impliesThat` E.suitLength T.Spades 4
    -- Don't be interested in a minor to bid this.
    E.forbidAll [ singleSuited T.Clubs
                , singleSuited T.Diamonds
                , twoSuited T.Clubs T.Diamonds
                ]
    -- TODO: would you bid Muppet Stayman with a void? Not sure...
    E.makeCall $ T.Bid 3 T.Clubs


b2N3C3H :: Action
b2N3C3H = E.nameAction "muppet_b2N3C3H" $ do
    E.forEach T.majorSuits (`E.maxSuitLength` 3)
    E.makeAlertableCall (T.Bid 3 T.Hearts) "no 4-card major"


b2N3C3H3S :: Action
b2N3C3H3S = E.nameAction "muppet_b2N3C3H3S" $ do
    E.maxSuitLength T.Spades 4
    E.maxSuitLength T.Hearts 5
    E.makeAlertableCall (T.Bid 3 T.Spades) ("relay to " .+ T.Bid 3 T.Notrump)


b2N3C3H3S3N :: Action
b2N3C3H3S3N = E.nameAction "muppet_b2N3C3H3S3N" $ do
    E.makeCall $ T.Bid 3 T.Notrump


b2N3C3H3N :: Action
b2N3C3H3N = E.nameAction "muppet_b2N3C3H3N" $ do
    E.suitLength T.Spades 5
    E.makeAlertableCall (T.Bid 3 T.Notrump) "5-card spade suit, pass or correct"


b2N3C3H3NP :: Action
b2N3C3H3NP = E.nameAction "muppet_b2N3C3H3NP" $ do
    E.maxSuitLength T.Spades 2
    E.makeCall T.Pass


b2N3C3H3N4S :: Action
b2N3C3H3N4S = E.nameAction "muppet_b2N3C3H3N4S" $ do
    E.minSuitLength T.Spades 3
    E.makeCall $ T.Bid 4 T.Spades


b2N3C3H4D :: Action
b2N3C3H4D = E.nameAction "muppet_b2N3C3H4D" $ do
    E.minSuitLength T.Hearts 6
    E.makeAlertableCall (T.Bid 4 T.Diamonds) "transfer to hearts"


b2N3C3H4D4H :: Action
b2N3C3H4D4H = E.nameAction "muppet_b2N3C3H4D4H" $ do
    E.makeCall $ T.Bid 4 T.Hearts


b2N3C3H4H :: Action
b2N3C3H4H = E.nameAction "muppet_b2N3C3H4H" $ do
    E.minSuitLength T.Spades 6
    E.makeAlertableCall (T.Bid 4 T.Hearts) "transfer to spades"


b2N3C3H4H4S :: Action
b2N3C3H4H4S = E.nameAction "muppet_b2N3C3H4H4S" $ do
    E.makeCall $ T.Bid 4 T.Spades


b2N3C3N :: Action
b2N3C3N = E.nameAction "muppet_b2N3C3N" $ do
    E.suitLength T.Hearts 5
    E.makeAlertableCall (T.Bid 3 T.Notrump) "5-card heart suit"


b2N3C3NP :: Action
b2N3C3NP = E.nameAction "muppet_b2N3C3NP" $ do
    E.forbid b2N3C3N4D
    E.makeCall $ T.Pass


b2N3C3N4D :: Action
b2N3C3N4D = E.nameAction "muppet_b2N3C3N4D" $ do
    E.minSuitLength T.Hearts 3
    E.makeAlertableCall (T.Bid 4 T.Diamonds) "transfer to hearts"


b2N3C3N4D4H :: Action
b2N3C3N4D4H = E.nameAction "muppet_b2N3C3N4D4H" $ do
    E.makeCall $ T.Bid 4 T.Hearts
