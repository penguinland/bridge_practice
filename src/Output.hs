-- In order to add the String type to the Showable typeclass, we need this
-- pragma. String is an alias for a list of characters, and the default compiler
-- doesn't let that be in a typeclass because not all its arguments are type
-- variables.
{-# LANGUAGE FlexibleInstances #-}

module Output (
  OutputType(..)
, Commentary(..)
, toCommentary
, Showable(..)
, (.+)
, Punct(..)
) where

import Data.Function((&))


data OutputType = LaTeX
                | Html


newtype Commentary = Commentary [OutputType -> String]

instance Semigroup Commentary where
    (Commentary as) <> (Commentary bs) = Commentary (as ++ bs)

instance Monoid Commentary where
    mempty = Commentary [const ""]


toCommentary :: Showable a => a -> Commentary
toCommentary a = Commentary [flip output a]


class Showable a where
    -- The minimal definition is either `output` or both `toLatex` and `toHtml`.
    toLatex :: a -> String
    toLatex = output LaTeX
    toHtml :: a -> String
    toHtml = output Html
    output :: OutputType -> a -> String
    output LaTeX = toLatex
    output Html = toHtml

instance Showable String where
    output = flip const

instance Showable Commentary where
    output o (Commentary c) = concatMap (o &) $ c


(.+) :: (Showable a, Showable b) => a -> b -> Commentary
a .+ b = toCommentary a <> toCommentary b


data Punct = NDash
           | MDash

instance Showable Punct where
    toLatex NDash = "--"
    toLatex MDash = "---"
    toHtml NDash = "&ndash;"
    toHtml MDash = "&mdash;"
