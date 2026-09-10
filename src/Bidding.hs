module Bidding (
  Bidding
, currentBidder
, startBidding
, addCall
, lastCall
) where

import Data.Aeson(ToJSON, toJSON, (.=), object)
import Data.Aeson.Key(fromString)
import Data.List.Utils(join)
import Data.Maybe(fromMaybe, catMaybes)
import Data.Semigroup(First(..), getFirst)

import Output(Showable(..))
import qualified Terminology as T



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

    toMonospace (Bidding _ auction) = let
        header = " West North  East South\n"
        foldFormatBid (T.CompleteCall b ma) (allBids, nextFootnote) = case ma of
            Nothing -> ((toMonospace b ++ "   ") : allBids, nextFootnote)
            Just _  ->
                ( (toMonospace b ++ "[" ++ show nextFootnote ++ "]") : allBids
                , nextFootnote + 1)
        foldFormatMaybeBid maybeBid (results, nextFootnote) =
            maybe ("     " : results, nextFootnote)
                  (flip foldFormatBid (results, nextFootnote))
                maybeBid
        foldFormatBidRow row (results, nextFootnote) = let
            (rowBids, nextFootnote') =
                foldr foldFormatMaybeBid ([], nextFootnote) row
          in
            ((join " " . reverse $ rowBids) : results, nextFootnote')
        -- `unlines` always puts a newline at the end. We remove it with `init`
        -- so we can append the " ??" on the end of the final line.
        formatAuction = init . unlines . reverse . fst .
                        foldr foldFormatBidRow ([], 1 :: Int)
        foldFormatAlert (T.CompleteCall _ m) (alerts, nextFootnote) = case m of
            Nothing -> (alerts, nextFootnote)
            Just a  ->
                ( ("[" ++ show nextFootnote ++ "]: " ++ toMonospace a) : alerts
                , nextFootnote + 1)
        formatAlerts = unlines . reverse . fst .
                       foldr foldFormatAlert ([], 1 :: Int) . catMaybes . concat
      in
        header ++ formatAuction auction ++ " ??\n\n" ++ formatAlerts auction


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
