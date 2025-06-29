-- In order to add the String type to the Showable typeclass, we need this
-- pragma. String is an alias for a list of characters, and the default compiler
-- doesn't let that be in a typeclass because not all its arguments are type
-- variables.
{-# LANGUAGE FlexibleInstances #-}

module Output (
  OutputType(..)
, Description(..)
, toDescription
, Showable(..)
, (.+)
, Punct(..)
) where

import Data.Aeson(ToJSON, toJSON)
import Data.Function((&))


data OutputType = LaTeX
                | Html


newtype Description = Description [OutputType -> String]

instance Semigroup Description where
    (Description as) <> (Description bs) = Description (as ++ bs)

instance Monoid Description where
    mempty = Description []

instance ToJSON Description where
    toJSON (Description d) = toJSON . concatMap (Html  &) $ d


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

instance Showable Description where
    toLatex (Description d) = concatMap (LaTeX &) $ d
    toHtml  (Description d) = concatMap (Html  &) $ d


(.+) :: (Showable a, Showable b) => a -> b -> Description
a .+ b = toDescription a <> toDescription b


toDescription :: Showable a => a -> Description
toDescription a = Description [flip output a]
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
