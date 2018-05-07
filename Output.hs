module Output (Showable, toLatex) where

class Showable a where
  toLatex :: a -> String
  -- toHtml :: a -> String


