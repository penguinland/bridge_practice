module Terminology (
  Direction(..)
, Suit(..)
, Call(..)
) where

import Output(Showable, toLatex)


data Direction = North | East | South | West

instance Showable Direction where
    toLatex North = "N"
    toLatex East  = "E"
    toLatex South = "S"
    toLatex West  = "W"


data Suit = Clubs | Diamonds | Hearts | Spades | Notrump

instance Showable Suit where
    toLatex Clubs     = "\\c"
    toLatex Diamonds  = "\\d"
    toLatex Hearts    = "\\h"
    toLatex Spades    = "\\s"
    toLatex Notrump   = "\\nt"


data Call = Pass | Double | Redouble | Bid Int Suit

instance Showable Call where
    toLatex Pass      = "P"
    toLatex Double    = "X"
    toLatex Redouble  = "XX"
    toLatex (Bid l s) = show l ++ toLatex s
