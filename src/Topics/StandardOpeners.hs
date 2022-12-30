module Topics.StandardOpeners(topic) where

import Output(output)
import Topic(Topic(..), wrap, stdWrap, wrapVulDlr, Situations)
import Auction(forbid, suitLength, minSuitLength, maxSuitLength, withholdBid)
import Situation(situation, (<~))
import qualified Terminology as T
import qualified CommonBids as B
import qualified StandardOpenings as SO


-- Yes, I realize that many of these Situations lack the nuance of planning your
-- second bid before making the first one. That'll come eventually as this
-- codebase gets more fleshed out.


oneNotrump :: Situations
oneNotrump = let
    action = do
        B.setOpener T.South
        withholdBid SO.b1n
    explanation fmt =
        "With 15 to 17 HCP and a balanced hand, open a strong " ++
        output fmt (T.Bid 1 T.Notrump) ++ ". No need to plan a second bid;\
      \ partner is now captain of the auction and will take over for you."
  in
    stdWrap $ situation "1N" action SO.b1n explanation


twoNotrump :: Situations
twoNotrump = let
    action = do
        B.setOpener T.South
        withholdBid SO.b2n
    explanation fmt =
        "With 20 to 21 HCP and a balanced hand, open " ++
        output fmt (T.Bid 2 T.Notrump) ++ ". No need to plan a second bid;\
      \ partner is now captain of the auction and will take over for you."
  in
    stdWrap $ situation "2N" action SO.b2n explanation


oneSpade :: Situations
oneSpade = let
    action = do
        B.setOpener T.South
        forbid $ minSuitLength T.Spades 5 >> minSuitLength T.Hearts 5
        withholdBid SO.b1s
    explanation fmt =
        "With a 5-card spade suit and a hand unsuitable for opening \
      \ notrump, open " ++ output fmt (T.Bid 1 T.Spades) ++ "."
  in
    stdWrap $ situation "1S" action SO.b1s explanation


oneHeart :: Situations
oneHeart = let
    action = do
        B.setOpener T.South
        forbid $ minSuitLength T.Spades 5 >> minSuitLength T.Hearts 5
        withholdBid SO.b1h
    explanation fmt =
        "With a 5-card heart suit and a hand unsuitable for opening \
      \ notrump, open " ++ output fmt (T.Bid 1 T.Hearts) ++ "."
  in
    stdWrap $ situation "1H" action SO.b1h explanation


bothMajorsReverse :: Situations
bothMajorsReverse = let
    action = do
        B.setOpener T.South
        minSuitLength T.Spades 5
        minSuitLength T.Hearts 5
        withholdBid SO.b1h
    explanation fmt =
        "With at least 5-5 in the majors and enough strength to reverse,\
      \ open " ++ output fmt (T.Bid 1 T.Hearts) ++ ", planning to rebid " ++
        output fmt (T.Bid 2 T.Spades) ++ " next turn."
  in
    stdWrap $ situation "MajRev" action SO.b1h explanation


bothMajorsNoReverse :: Situations
bothMajorsNoReverse = let
    action = do
        B.setOpener T.South
        minSuitLength T.Spades 5
        minSuitLength T.Hearts 5
        withholdBid SO.b1s
    explanation fmt =
        "With at least 5-5 in the majors and not enough strength to\
      \ reverse, open " ++ output fmt (T.Bid 1 T.Spades) ++ ", planning\
      \ to rebid " ++ output fmt (T.Bid 2 T.Hearts) ++ " next turn."
  in
    stdWrap $ situation "MajNoRev" action SO.b1s explanation


oneDiamond :: Situations
oneDiamond = let
    action = do
        B.setOpener T.South
        maxSuitLength T.Clubs 3
        minSuitLength T.Diamonds 4
        withholdBid SO.b1d
    explanation fmt =
        "With no 5-card major and a hand unsuitable for opening \
      \ notrump, open " ++ output fmt (T.Bid 1 T.Diamonds) ++ " when \
      \ diamonds is your only minor."
  in
    stdWrap $ situation "1D1Suit" action SO.b1d explanation


oneDiamond3Cards :: Situations
oneDiamond3Cards = let
    action = do
        B.setOpener T.South
        suitLength T.Spades   4
        suitLength T.Hearts   4
        suitLength T.Diamonds 3
        suitLength T.Clubs    2
        withholdBid SO.b1d
    explanation fmt =
        "With no 5-card major and a hand unsuitable for opening \
      \ notrump, open " ++ output fmt (T.Bid 1 T.Diamonds) ++ " when \
      \ diamonds is your only minor, even if you only have 3 of them. The \
      \ only time you'd open 1D with a 3-card suit in standard openings is \
      \ when your shape is exactly 4=4=3=2."
  in
    stdWrap $ situation "1D4432" action SO.b1d explanation


oneClub :: Situations
oneClub = let
    action = do
        B.setOpener T.South
        maxSuitLength T.Diamonds 3
        minSuitLength T.Clubs 4
        withholdBid SO.b1c
    explanation fmt =
        "With no 5-card major and a hand unsuitable for opening \
      \ notrump, open " ++ output fmt (T.Bid 1 T.Clubs) ++ " when \
      \ clubs is your only minor."
  in
    stdWrap $ situation "1C1Suit" action SO.b1c explanation


oneClubEqualMinors :: Situations
oneClubEqualMinors = let
    sit len = let
        action = do
            B.setOpener T.South
            suitLength T.Diamonds len
            suitLength T.Clubs len
            withholdBid SO.b1c
        explanation fmt =
            "With no 5-card major and a hand unsuitable for opening \
          \ notrump, open " ++ output fmt (T.Bid 1 T.Clubs) ++ " when \
          \ your minors are of equal, short length. As the saying goes, \
          \ ``up the line with 3s and 4s, from the top with 5s or mores.''"
      in
        situation "1C1Suit" action SO.b1c explanation
  in
    wrapVulDlr $ return sit <~ [3, 4]


bothMinorsNoReverse :: Situations
bothMinorsNoReverse = let
    action = do
        B.setOpener T.South
        minSuitLength T.Diamonds 5
        minSuitLength T.Clubs 5
        withholdBid SO.b1d
    explanation fmt =
        "With both minors but not enough strength to reverse, open, " ++
        output fmt (T.Bid 1 T.Diamonds) ++ ", planning to rebid " ++
        output fmt (T.Bid 2 T.Clubs) ++ " next turn."
  in
    stdWrap $ situation "MinNoRev" action SO.b1d explanation


bothMinorsNoReverseShortD :: Situations
bothMinorsNoReverseShortD = let
    action = do
        B.setOpener T.South
        suitLength T.Diamonds 4
        suitLength T.Clubs 5
        withholdBid SO.b1d
    explanation fmt =
        "With both minors but not enough strength to reverse, open " ++
        output fmt (T.Bid 1 T.Diamonds) ++ ", planning to rebid " ++
        output fmt (T.Bid 2 T.Clubs) ++ " next turn. This is even the case\
      \ when your clubs are longer than your diamonds! Bidding clubs first\
      \ and diamonds second is a reverse (responder can't go back to your\
      \ first suit without going to the 3 level), and cannot be bid without\
      \ enough strength to make a 3-level contract viable when partner has\
      \ a minimum."
  in
    stdWrap $ situation "MinNoRv4D" action SO.b1d explanation


bothMinorsReverse :: Situations
bothMinorsReverse = let
    action = do
        B.setOpener T.South
        minSuitLength T.Diamonds 5
        minSuitLength T.Clubs 5
        withholdBid SO.b1c
    explanation fmt =
        "With both minors and enough strength to reverse, open, " ++
        output fmt (T.Bid 1 T.Clubs) ++ ", planning to rebid " ++
        output fmt (T.Bid 2 T.Diamonds) ++ " next turn."
  in
    stdWrap $ situation "MinRev55" action SO.b1c explanation



topic :: Topic
topic = Topic "Standard 1-level opening bids" "StdOpen" situations
  where
    situations = wrap [ oneNotrump
                      , twoNotrump
                      , oneSpade
                      , oneHeart
                      , wrap [bothMajorsReverse, bothMajorsNoReverse]
                      , wrap [oneDiamond, oneDiamond3Cards]
                      , wrap [oneClub, oneClubEqualMinors]
                      , wrap [bothMinorsNoReverse, bothMinorsNoReverseShortD,
                              bothMinorsReverse]
                      ]
