module Structures (
  Hand(..)
, Bidding
, currentBidder
, startBidding
, addCall
, lastCall
, Deal(..)
) where

import Data.Aeson(ToJSON, toJSON, (.=), object)
import Data.Aeson.Key(fromString)
import Data.Char(toUpper)
import Data.List.Utils(join, replace)
import Data.Map(fromList)
import Data.Maybe(fromMaybe)
import Data.Semigroup(First(..), getFirst)

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

instance ToJSON Hand where
    toJSON (Hand s h d c) = toJSON . fmap formatHolding . fromList $
        [("spades", s), ("hearts", h), ("diamonds", d), ("clubs", c)]
      where
        formatHolding = replace "-" (toHtml NDash) .
                        replace "T" "10"


-- The direction is the next bidder
data Bidding = Bidding T.Direction [[Maybe T.CompleteCall]]

instance Showable Bidding where
    toLatex (Bidding r b) =
         "  \\begin{bidding}\n    " ++ rows ++ finish r ++
         "??\n  \\end{bidding}"
      where
        newRow = "\\\\\n    "  -- backslash, backslash, newline
        rows = join newRow . reverse . map formatRow $ b
        formatRow = join "&" .
                    zipWith formatMaybeBid (cycle ["oppsalert", "ouralert"]) .
                    reverse
        formatMaybeBid alertMacro = maybe "" $ formatBid alertMacro
        formatBid _          (T.CompleteCall c  Nothing) = toLatex c
        formatBid alertMacro (T.CompleteCall c (Just a)) =
            toLatex c ++ "\\" ++ alertMacro ++ "{" ++ toLatex a ++ "}"
        finish T.North = newRow
        finish _       = "&"

-- TODO: make good support for alerts in here. Currently they're all displayed
-- all the time.
instance ToJSON Bidding where
    toJSON (Bidding _ b) = toJSON . reverse . map reverse . appendPrompt .
                           map (map (fromMaybe (object []) . fmap toJSON)) $ b
      where
        challenge = object [fromString "call" .= "??"]
        appendPrompt []                        = [[challenge]]
        appendPrompt (row@([_, _, _, _]):rows) = [challenge] : row : rows
        appendPrompt (first:rest)              = (challenge : first) : rest


currentBidder :: Bidding -> T.Direction
currentBidder (Bidding d _) = d


startBidding :: T.Direction -> Bidding
startBidding T.West  = Bidding T.West  []
startBidding T.North = Bidding T.North [[Nothing]]
startBidding T.East  = Bidding T.East  [[Nothing, Nothing]]
startBidding T.South = Bidding T.South [[Nothing, Nothing, Nothing]]


addCall :: T.CompleteCall -> Bidding -> Bidding
addCall c (Bidding T.West    bs ) = Bidding T.North    ([Just c]  :bs)
addCall c (Bidding d      (b:bs)) = Bidding (T.next d) ((Just c:b):bs)
addCall _ _                       = error "malformed bidding"


lastCall :: Bidding -> T.CompleteCall
lastCall (Bidding _ calls) =
    -- Surely there's a better way to do this, but I couldn't figure it out.
    -- concat flattens the bidding to one long list (most recent bids first!).
    -- We then map the calls to First of these values, and use sequence to turn
    -- a (First Maybe CompleteCall) into a (Maybe First CompleteCall). Then
    -- mconcat bunches them all into a single (Maybe First CompleteCall), which
    -- we unwrap.
    getFirst . fromMaybe (error "Unable to get last call from empty bidding") .
        mconcat . map (sequence . First) . concat $ calls


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

instance ToJSON Deal where
    toJSON (Deal d v n e s w) = toJSON . fromList $
        [ ("dealer",        toJSON . toHtml $ d)
        , ("vulnerability", toJSON . toHtml $ v)
        , ("north_hand",    toJSON n)
        , ("east_hand",     toJSON e)
        , ("south_hand",    toJSON s)
        , ("west_hand",     toJSON w)
        ]
