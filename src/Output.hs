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

import Data.Aeson(ToJSON, toJSON)
import Data.Function((&))


data OutputType = LaTeX
                | Html


newtype Commentary = Commentary [OutputType -> String]

instance Semigroup Commentary where
    (Commentary as) <> (Commentary bs) = Commentary (as ++ bs)

instance Monoid Commentary where
    mempty = Commentary [const ""]

instance ToJSON Commentary where
    toJSON (Commentary c) = toJSON . concatMap (Html  &) $ c


class Showable a where
    toLatex :: a -> String
    -- This wasn't organized as well as I would have preferred: many types are
    -- supposed to be Showable but the HTML-like output is really JSON. Give a
    -- default "implementation" for those types, and clean this up eventually.
    toHtml :: a -> String
    toHtml _ = undefined

instance Showable String where
    toLatex = id
    toHtml = id

instance Showable Commentary where
    toLatex (Commentary c) = concatMap (LaTeX &) $ c
    toHtml  (Commentary c) = concatMap (Html  &) $ c


(.+) :: (Showable a, Showable b) => a -> b -> Commentary
a .+ b = toCommentary a <> toCommentary b


toCommentary :: Showable a => a -> Commentary
toCommentary a = Commentary [flip output a]
  where
    output LaTeX = toLatex
    output Html = toHtml


data Punct = NDash
           | MDash
           | OpenQuote
           | CloseQuote
           | EAcute

instance Showable Punct where
    toLatex NDash = "--"
    toLatex MDash = "---"
    toLatex OpenQuote = "``"
    toLatex CloseQuote = "''"
    toLatex EAcute = "\\'e"
    toHtml NDash = "&ndash;"
    toHtml MDash = "&mdash;"
    toHtml OpenQuote = "&#x201C;"
    toHtml CloseQuote = "&#x201D;"
    toHtml EAcute = "&eacute;"
