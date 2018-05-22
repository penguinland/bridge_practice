module Terminology (
  Direction(..)
, Suit(..)
, Call(..)
, Vulnerability(..)
) where

import Output(Showable, toLatex)


data Direction = North | East | South | West

instance Showable Direction where
    toLatex North = "N"
    toLatex East  = "E"
    toLatex South = "S"
    toLatex West  = "W"

instance Show Direction where
    show North = "north"
    show East  = "east"
    show South = "south"
    show West  = "west"


data Suit = Clubs | Diamonds | Hearts | Spades | Notrump

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


data Call = Pass | Double | Redouble | Bid Int Suit

instance Showable Call where
    toLatex Pass      = "P"
    toLatex Double    = "X"
    toLatex Redouble  = "XX"
    toLatex (Bid l s) = show l ++ toLatex s


data Vulnerability = NS | EW | Both | Neither

instance Showable Vulnerability where
    toLatex NS      = "N/S"
    toLatex EW      = "E/W"
    toLatex Both    = "Both"
    toLatex Neither = "Neither"
