module Terminology (
  Direction(..)
, allDirections
, next
, Suit(..)
, allSuits
, minorSuits
, majorSuits
, Call(..)
, CompleteCall(..)
, removeAlert
, Vulnerability(..)
, allVulnerabilities
) where

import Output(Showable, toLatex, toHtml, Commentary)


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


-- A complete call is a call with an optional alerted explanation.
data CompleteCall = CompleteCall Call (Maybe Commentary)

-- The Showable instance is for revealing what the correct answer is; during the
-- auction we need to do different things for our alerts and our opponents'
-- alerts, so that all gets packed into the Showable instance for the Bidding
-- datatype in Structures.hs instead.
instance Showable CompleteCall where
    toLatex (CompleteCall c a) =
        toLatex c ++ maybe "" (\x -> " (" ++ toLatex x ++ ")") a


removeAlert :: CompleteCall -> Call
removeAlert (CompleteCall c _) = c


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
