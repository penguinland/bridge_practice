module Output (
  Showable
, toLatex
, toHtml
, OutputType(..)
, output
, Punct(..)
, Commentary
) where

class Showable a where
    toLatex :: a -> String
    toHtml :: a -> String
    toHtml = undefined -- TODO: remove this later


data OutputType = LaTeX
                | Html


output :: (Showable a) => OutputType -> a -> String
output LaTeX = toLatex
output Html = toHtml


data Punct = NDash
           | MDash

instance Showable Punct where
    toLatex NDash = "--"
    toLatex MDash = "---"
    toHtml NDash = "&ndash;"
    toHtml MDash = "&mdash;"

type Commentary = OutputType -> String
