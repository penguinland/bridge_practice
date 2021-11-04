module Topics.StandardModernPrecision.OpeningBids(
    topic
  , firstSeatOpener  -- Find a different place to put these.
  , oneClubOpener) where

import Output(output)
import Topic(Topic(..), wrap, Situations)
import Auction(forbid, pointRange, minSuitLength, Action, balancedHand,
               constrain)
import Situation(Situation, situation, base, (<~))
import qualified Terminology as T


-- Because we're not using the Rule of 20 and its ilk, we're going to skip the
-- auctions that start with other folks passing for now, and maybe come back to
-- those later.
-- TODO: figure out how to practice the auctions that start P-1C-1S (with 1S
-- being game forcing and natural 5-card spade suit, rather than slam forcing)
firstSeatOpener :: Action
firstSeatOpener = do
    pointRange 11 40  -- Open any good 10 count, too. but that's hard to codify


oneNotrumpOpener :: Action
oneNotrumpOpener = do
    pointRange 14 16
    balancedHand


twoNotrumpOpener :: Action
twoNotrumpOpener = do
    pointRange 19 21  -- A modification from Part 1: it's really 19 to a bad 21
    balancedHand


oneClubOpener :: Action
oneClubOpener = do
    pointRange 16 40
    forbid oneNotrumpOpener
    forbid twoNotrumpOpener


oneMajorOpener :: T.Suit -> Action
oneMajorOpener suit = do
    forbid oneClubOpener
    forbid oneNotrumpOpener
    minSuitLength suit 5


twoClubOpener :: Action
twoClubOpener = do
    forbid oneClubOpener
    forbid (oneMajorOpener T.Hearts)
    forbid (oneMajorOpener T.Spades)
    minSuitLength T.Clubs 6


twoDiamondOpener :: Action
twoDiamondOpener = do
    forbid oneClubOpener
    constrain "two_diamond_opener" ["shape(", ", 4414 + 4405 + 4315 + 3415)"]


oneDiamondOpener :: Action
oneDiamondOpener = do
    sequence_ . map forbid $ [ oneClubOpener
                             , oneNotrumpOpener
                             , oneMajorOpener T.Hearts
                             , oneMajorOpener T.Spades
                             , twoClubOpener
                             , twoDiamondOpener
                             ]
    -- The next line is commented out because if it can be violated, we're gonna
    -- have a bad day. Make sure that it's never violated in the results even if
    -- it's not explicitly required.
    --minSuitLength T.Diamonds 2


-- syntactic sugar: always make South the dealer
-- TODO: change this to let other folks be dealer, too
smpWrap :: (T.Vulnerability -> T.Direction -> Situation) -> Situations
smpWrap sit = wrap $ base sit <~ T.allVulnerabilities <~ [T.South]


oneClub :: Situations
oneClub = let
    action = do
        firstSeatOpener
        oneClubOpener
    explanation fmt =
        "With 16 or more points (17 or more when balanced), open a strong " ++
        output fmt (T.Bid 1 T.Clubs) ++ ". This is the hallmark of SMP."
  in
    smpWrap $ situation "1C" action (T.Bid 1 T.Clubs) explanation


oneDiamond :: Situations
oneDiamond = let
    action = do
        firstSeatOpener
        oneDiamondOpener
    explanation fmt =
        "With opening strength but the wrong strength/shape for any other\
      \ opening bid, start with " ++ output fmt (T.Bid 1 T.Diamonds) ++ ".\
      \ Partner will announce that it ``could be as short as 2.''"
  in
    smpWrap $ situation "1D" action (T.Bid 1 T.Diamonds) explanation


oneMajor :: Situations
oneMajor = let
    sit suit = let
        action = do
            firstSeatOpener
            oneMajorOpener suit
            -- TODO: What if you're 5-5?
        explanation fmt =
            "With opening strength but not enough for a strong " ++
            output fmt (T.Bid 1 T.Clubs) ++ " or " ++
            output fmt (T.Bid 1 T.Notrump) ++ ", open a 5-card major suit."
      in
        situation "1M" action (T.Bid 1 suit) explanation
  in
    -- TODO: figure out some syntactic sugar for this, too
    wrap $ base sit <~ T.majorSuits <~ T.allVulnerabilities <~ [T.South]


oneNotrump :: Situations
oneNotrump = let
    action = do
        firstSeatOpener
        oneNotrumpOpener
    explanation fmt =
        "With a balanced hand and 14-16 HCP, open " ++
        output fmt (T.Bid 1 T.Notrump) ++ "."
  in
    smpWrap $ situation "1N" action (T.Bid 1 T.Notrump) explanation


twoClubs :: Situations
twoClubs = let
    action = do
        firstSeatOpener
        twoClubOpener
    explanation fmt =
        "With a 6-card club suit, no 5-card major, an opening hand but not a\
       \ hand strong enough to open " ++ output fmt (T.Bid 1 T.Clubs) ++ ",\
       \ open " ++ output fmt (T.Bid 2 T.Clubs) ++ "."
  in
    smpWrap $ situation "2C" action (T.Bid 2 T.Clubs) explanation


twoDiamonds :: Situations
twoDiamonds = let
    action = do
        firstSeatOpener
        twoDiamondOpener
    explanation fmt =
        "The " ++ output fmt (T.Bid 2 T.Diamonds) ++ " hand is that 3-suited\
       \ hand without diamonds, which can be thought of as a 14-card hand with\
       \ 4415 shape but missing any single card."
  in
    smpWrap $ situation "2D" action (T.Bid 2 T.Diamonds) explanation


twoNotrump :: Situations
twoNotrump = let
    action = do
        firstSeatOpener
        twoNotrumpOpener
    explanation fmt =
        "With a balanced hand and 19 to a bad 21 HCP, open " ++
        output fmt (T.Bid 2 T.Notrump) ++ "."
  in
    smpWrap $ situation "2N" action (T.Bid 2 T.Notrump) explanation


topic :: Topic
topic = Topic "SMP opening bids" "SmpOpen" situations
  where
    situations = wrap [ oneClub
                      , oneDiamond
                      , oneMajor
                      , oneNotrump
                      , twoClubs
                      , twoDiamonds
                      , twoNotrump
                      ]
