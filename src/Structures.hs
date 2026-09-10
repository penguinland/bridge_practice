module Structures (
  Hand(..)
, Deal(..)
) where

import Data.Aeson(ToJSON, toJSON)
import Data.Char(toUpper)
import Data.List.Utils(join, replace)
import Data.Map(fromList)

import Output(Showable(..), Punct(NDash))
import qualified Terminology as T


--               spades hearts diams. clubs
data Hand = Hand String String String String

instance Showable Hand where
    toLatex (Hand s h d c) =
        "\\hand{" ++
        join "}{" (map (replace "-" (toLatex NDash) .
                        replace "T" "10" .
                        replace " " "\\,") [s, h, d, c])
        ++ "}"
    toMonospace (Hand s h d c) =
        unlines $ zipWith formatSuit "SHDC" [s, h, d, c]
      where
        formatSuit name holding = (name : ": ") ++ (replace "T" "10" holding)

instance ToJSON Hand where
    toJSON (Hand s h d c) = toJSON . fmap formatHolding . fromList $
        [("spades", s), ("hearts", h), ("diamonds", d), ("clubs", c)]
      where
        formatHolding = replace "-" (toHtml NDash) .
                        replace "T" "10"


--                                           N    E    S    W
data Deal = Deal T.Direction T.Vulnerability Hand Hand Hand Hand

instance Showable Deal where
    toLatex (Deal d v n e s w) =
        "  \\deal{" ++ capitalize (show d) ++ "}{" ++
           join "}%\n    {" (toLatex v : map toLatex [n, e, s, w]) ++
           "%\n  }"
      where
        capitalize (h:t) = toUpper h : t
        capitalize _     = error "Attempt to capitalize empty direction!?"
    toMonospace (Deal d v n e s w) = let
        ns = lines . toMonospace $ n
        es = lines . toMonospace $ e
        ss = lines . toMonospace $ s
        ws = lines . toMonospace $ w
        indent = replicate 8 ' '
        formatNS = map (indent ++)
        formatEW = zipWith (\a b -> take 20 (a ++ replicate 20 ' ') ++ b)
        footer = "Dealer: " ++ toMonospace d ++ ", Vul: " ++ toMonospace v
      in
        unlines $ formatNS ns ++ [""] ++ formatEW ws es ++ [""] ++
                  formatNS ss ++ ["", footer]

instance ToJSON Deal where
    toJSON (Deal d v n e s w) = toJSON . fromList $
        [ ("dealer",        toJSON . toHtml $ d)
        , ("vulnerability", toJSON . toHtml $ v)
        , ("north_hand",    toJSON n)
        , ("east_hand",     toJSON e)
        , ("south_hand",    toJSON s)
        , ("west_hand",     toJSON w)
        ]
