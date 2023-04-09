module Output (
  Showable
, toLatex
, toHtml
, OutputType(..)
, output
, Punct(..)
, Commentary(..)
) where

import Data.Function((&))
import Data.List.Utils(join)


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


data Commentary = Commentary [OutputType -> String]

instance Showable Commentary where
    toLatex (Commentary comments) = join "" . map (LaTeX &) $ comments

instance Semigroup Commentary where
    (Commentary as) <> (Commentary bs) = Commentary (as ++ bs)

instance Monoid Commentary where
    mempty = Commentary [const ""]
