module Terminology (
  Direction(..)
, allDirections
, Suit(..)
, allSuits
, minorSuits
, majorSuits
, Call(..)
, Vulnerability(..)
, allVulnerabilities
) where

import Output(Showable, toLatex)


data Direction = North | East | South | West deriving Eq

instance Showable Direction where
    toLatex North = "N"
    toLatex East  = "E"
    toLatex South = "S"
    toLatex West  = "W"

-- We keep these lowercase because that's the format that dealer wants. You'll
-- have to upper-case them yourself to print them out.
instance Show Direction where
    show North = "north"
    show East  = "east"
    show South = "south"
    show West  = "west"

allDirections :: [Direction]
allDirections = [North, East, South, West]


data Suit = Clubs | Diamonds | Hearts | Spades | Notrump deriving Eq

instance Showable Suit where
    toLatex Clubs     = "\\c"
    toLatex Diamonds  = "\\d"
    toLatex Hearts    = "\\h"
    toLatex Spades    = "\\s"
    toLatex Notrump   = "\\nt"

instance Show Suit where
    show Clubs =    "clubs"
    show Diamonds = "diamonds"
    show Hearts =   "hearts"
    show Spades =   "spades"

allSuits :: [Suit]
allSuits = [Clubs, Diamonds, Hearts, Spades]
minorSuits :: [Suit]
minorSuits = [Clubs, Diamonds]
majorSuits :: [Suit]
majorSuits = [Hearts, Spades]


data Call = Pass | Double | Redouble | Bid Int Suit deriving Eq

instance Showable Call where
    toLatex Pass      = "Pass"
    toLatex Double    = "Dbl"
    toLatex Redouble  = "Rdb"
    toLatex (Bid l s) = show l ++ toLatex s


data Vulnerability = NS | EW | Both | Neither deriving Eq

instance Showable Vulnerability where
    toLatex NS      = "N/S"
    toLatex EW      = "E/W"
    toLatex Both    = "Both"
    toLatex Neither = "Neither"

allVulnerabilities :: [Vulnerability]
allVulnerabilities = [NS, EW, Both, Neither]
