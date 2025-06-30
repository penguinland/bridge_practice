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
import Data.Maybe(fromMaybe, catMaybes)
import Data.Semigroup(First(..), getFirst)

import Output(Showable(..), Description, Punct(NDash))
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
        join "\n" $ zipWith formatSuit "SHDC" [s, h, d, c]
      where
        formatSuit name holding = (name : ": ") ++ spaceCards holding
        spaceCards = replace "." (toMonospace NDash) . replace "T" "10"

instance ToJSON Hand where
    toJSON (Hand s h d c) = toJSON . fmap formatHolding . fromList $
        [("spades", s), ("hearts", h), ("diamonds", d), ("clubs", c)]
      where
        formatHolding = replace "-" (toHtml NDash) .
                        replace "T" "10"


-- The direction is the next bidder
data Bidding = Bidding T.Direction [[Maybe T.CompleteCall]]

instance Semigroup Bidding where
    biddingA <> (Bidding dirB callsB) = let
        (Bidding finalDir finalCalls) =
            foldl (flip addCall) biddingA (catMaybes . concat $ callsB)
      in
        if finalDir == dirB
        then Bidding finalDir finalCalls
        else error "cannot concatenate Bidding from wrong directions"


instance Showable Bidding where
    toLatex (Bidding p b) =
         "  \\begin{bidding}\n    " ++ rows ++ finish p ++
         "??\n  \\end{bidding}"
      where
        newRow = "\\\\\n    "  -- backslash, backslash, newline
        rows = join newRow . map formatRow . reverse $ b
        formatRow = join "&" .
                    zipWith formatMaybeBid (cycle ["oppsalert", "ouralert"]) .
                    reverse
        formatMaybeBid alertMacro = maybe "" $ formatBid alertMacro
        formatBid _          (T.CompleteCall c  Nothing) = toLatex c
        formatBid alertMacro (T.CompleteCall c (Just a)) =
            toLatex c ++ "\\" ++ alertMacro ++ "{" ++ toLatex a ++ "}"
        finish T.North = newRow
        finish _       = "&"
    toMonospace (Bidding _ b) =
        header ++ formatAuction b ++ "??\n\n" ++ formatAlerts b ++ "\n"
      where
        header = " West North  East South\n"
        formatAuction = join "\n" . reverse . fst . foldl formatAuction' ([], 1) . reverse . map catMaybes
        formatAuction' :: ([String], Int) -> [T.CompleteCall] -> ([String], Int)
        formatAuction' (outputRows, nextFootnote) row = let
            formatBid (rowSoFar, nextFootnote') (T.CompleteCall c a) = case a of
                Nothing -> (rowSoFar ++ toMonospace c ++ "    ", nextFootnote')
                Just _  -> (rowSoFar ++ toMonospace c ++
                                "[" ++ show nextFootnote' ++ "] "
                           , nextFootnote' + 1
                           )
            (rowOutput, nextFootnote'') =
                foldl formatBid ("", nextFootnote) . reverse $ row
          in
            (rowOutput : outputRows, nextFootnote'')
        formatAlerts :: [[Maybe T.CompleteCall]] -> String
        formatAlerts = join "\n" . fst . foldr formatAlerts' ([], 1)
        formatAlerts' :: [Maybe T.CompleteCall] -> ([String], Int) -> ([String], Int)
        formatAlerts' row (alerts, nextFootnote) = let
            (alertsForRow, nextFootnote') =
                foldr formatAlert ([], nextFootnote) . catMaybes $ row
          in
            (alertsForRow ++ alerts, nextFootnote')
        formatAlert :: T.CompleteCall -> ([String], Int) -> ([String], Int)
        formatAlert (T.CompleteCall _ ma) (alertsSoFar, nextFootnote) = case ma of
            Nothing -> (alertsSoFar, nextFootnote)
            Just a -> ((formatAlert' nextFootnote a) : alertsSoFar, nextFootnote + 1)
        formatAlert' :: Int -> Description -> String
        formatAlert' nextFootnote a =
            "[" ++ show nextFootnote++ "]: " ++ toMonospace a

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
    toMonospace (Deal d v n e s w) = let
        ns = lines . toMonospace $ n
        es = lines . toMonospace $ e
        ss = lines . toMonospace $ s
        ws = lines . toMonospace $ w
        indent = replicate 8 ' '
        formatNS = map (indent ++)
        formatEW = zipWith (\a b -> take 20 (a ++ replicate 20 ' ') ++ b)
        header = ["Dealer: " ++ toMonospace d ++ ", Vul: " ++ toMonospace v, ""]
      in
        join "\n" $ header ++ formatNS ns ++ [""] ++ formatEW es ws ++ [""] ++
                    formatNS ss ++ [""]

instance ToJSON Deal where
    toJSON (Deal d v n e s w) = toJSON . fromList $
        [ ("dealer",        toJSON . toHtml $ d)
        , ("vulnerability", toJSON . toHtml $ v)
        , ("north_hand",    toJSON n)
        , ("east_hand",     toJSON e)
        , ("south_hand",    toJSON s)
        , ("west_hand",     toJSON w)
        ]
