module Terminology (
  Direction(..)
, allDirections
, next
, Suit(..)
, allSuits
, minorSuits
, majorSuits
, Call(..)
, CallExplanation(..)
, alertFor
, CompleteCall(..)
, Vulnerability(..)
, allVulnerabilities
) where

import Output(Showable, toLatex, toHtml)


data Direction = North | East | South | West deriving Eq

instance Showable Direction where
    toLatex North = "N"
    toLatex East  = "E"
    toLatex South = "S"
    toLatex West  = "W"
    toHtml North = "N"
    toHtml East  = "E"
    toHtml South = "S"
    toHtml West  = "W"

-- We keep these lowercase because that's the format that dealer wants. You'll
-- have to upper-case them yourself to print them out.
instance Show Direction where
    show North = "north"
    show East  = "east"
    show South = "south"
    show West  = "west"

allDirections :: [Direction]
allDirections = [North, East, South, West]


next :: Direction -> Direction
next North = East
next East  = South
next South = West
next West  = North


-- TODO: consider separating Suits from Strains, and only have Notrump in the
-- latter.
data Suit = Clubs | Diamonds | Hearts | Spades | Notrump deriving Eq

instance Showable Suit where
    toLatex Clubs     = "\\c{}"
    toLatex Diamonds  = "\\d{}"
    toLatex Hearts    = "\\h{}"
    toLatex Spades    = "\\s{}"
    toLatex Notrump   = "\\nt{}"
    toHtml Clubs     = "&clubs;"
    toHtml Diamonds  = "<span class='red'>&diams;</span>"
    toHtml Hearts    = "<span class='red'>&hearts;</span>"
    toHtml Spades    = "&spades;"
    toHtml Notrump   = "<span class='smallcaps'>NT</span>"

instance Show Suit where
    show Clubs    = "clubs"
    show Diamonds = "diamonds"
    show Hearts   = "hearts"
    show Spades   = "spades"
    show Notrump  = "notrump"

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
    toHtml Pass      = "Pass"
    toHtml Double    = "Dbl"
    toHtml Redouble  = "Rdb"
    toHtml (Bid l s) = show l ++ toHtml s

-- Calls might have alerted explanations. We always show an alert from the
-- opponents, but only show alerts from our side in the solutions.
data CallExplanation = Unalerted | OppsAlerted String | WeAlerted String

instance Showable CallExplanation where
    toLatex Unalerted = ""
    toLatex (OppsAlerted e) = "\\footnote{" ++ e ++ "}"
    toLatex (WeAlerted e) =
         "\\ifdefined\\showsolutions" ++ toLatex (OppsAlerted e) ++ "\\fi"

alertFor :: Direction -> String -> CallExplanation
alertFor North = WeAlerted
alertFor South = WeAlerted
alertFor East = OppsAlerted
alertFor West = OppsAlerted


data CompleteCall = CompleteCall Call CallExplanation

instance Showable CompleteCall where
    toLatex (CompleteCall c e) = toLatex c ++ toLatex e
    toHtml (CompleteCall c e) = toHtml c ++ toHtml e


data Vulnerability = NS | EW | Both | None deriving Eq

instance Showable Vulnerability where
    toLatex NS   = "N/S"
    toLatex EW   = "E/W"
    toLatex Both = "Both"
    toLatex None = "None"
    toHtml NS   = "N/S"
    toHtml EW   = "E/W"
    toHtml Both = "Both"
    toHtml None = "None"

allVulnerabilities :: [Vulnerability]
allVulnerabilities = [NS, EW, Both, None]
