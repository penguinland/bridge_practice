-- In order to add the String type to the Showable typeclass, we need this
-- pragma. String is an alias for a list of characters, and the default compiler
-- doesn't let that be in a type class because not all its arguments are type
-- variables.
{-# LANGUAGE FlexibleInstances #-}

module Output (
  Showable
, toLatex
, toHtml
, OutputType(..)
, output
, Punct(..)
, Commentary(..)
, (.+)
) where

import Data.Function((&))
import Data.List.Utils(join)


class Showable a where
    -- The minimal definition is either `output` or both `toLatex` and `toHtml`.
    toLatex :: a -> String
    toLatex = output LaTeX
    toHtml :: a -> String
    toHtml = undefined -- TODO: change this to `output Html` later
    toCommentary :: a -> Commentary
    toCommentary a = Commentary [flip output a]
    output :: OutputType -> a -> String
    output LaTeX = toLatex
    output Html = toHtml


instance Showable String where
    toLatex = id
    toHtml = id


-- TODO: remove this when it's no longer needed
instance Showable (OutputType -> String) where
    toLatex = toLatex . toCommentary
    toHtml = toHtml . toCommentary
    toCommentary a = Commentary [a]


data OutputType = LaTeX
                | Html


data Punct = NDash
           | MDash

instance Showable Punct where
    toLatex NDash = "--"
    toLatex MDash = "---"
    toHtml NDash = "&ndash;"
    toHtml MDash = "&mdash;"


newtype Commentary = Commentary [OutputType -> String]

instance Showable Commentary where
    output o (Commentary c) = join "" . map (o &) $ c
    toCommentary = id

instance Semigroup Commentary where
    (Commentary as) <> (Commentary bs) = Commentary (as ++ bs)

instance Monoid Commentary where
    mempty = Commentary [const ""]


(.+) :: (Showable a, Showable b) => a -> b -> Commentary
a .+ b = toCommentary a <> toCommentary b
