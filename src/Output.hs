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

class Outputtable a where
    outputWrap :: OutputType -> a -> String


-- typeclass instances need to be a specific type constructor with arbitrary
-- type variables passed in. This is such a type.
newtype Wrap a = Wrap {unwrap :: a}


instance Showable a => Outputtable (Wrap a) where
    outputWrap LaTeX = toLatex . unwrap
    outputWrap Html = toHtml . unwrap


output :: (Showable a) => OutputType -> a -> String
output t = outputWrap t . Wrap


data Punct = NDash
           | MDash

instance Showable Punct where
    toLatex NDash = "--"
    toLatex MDash = "---"
    toHtml NDash = "&ndash;"
    toHtml MDash = "&mdash;"

type Commentary = OutputType -> String
