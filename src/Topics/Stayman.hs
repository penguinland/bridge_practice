module Topics.Stayman(topic) where

-- TODO: replace makePass with something more intelligent
import Auction(makePass, pointRange, suitLength, maxSuitLength)
import CommonBids(setOpener)
import Output((.+), Punct(..))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, stdWrap, wrap, wrapVulDlr, Situations, makeTopic)
import qualified Bids.OneNotrump as B


garbageStayman :: Situations
garbageStayman = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            pointRange 0 7
        explanation =
            "Partner has opened a strong " .+ T.Bid 1 T.Notrump .+ ". We " .+
            "have less than invitational strength, but we have both majors " .+
            "and diamonds. Bid Garbage Stayman! Partner will think it's " .+
            OpenQuote .+ "normal" .+ CloseQuote .+ " Stayman, and bid " .+
            "accordingly. Pass whatever partner does: if you find an 8-card " .+
            "major fit, that will likely play better than stopping in " .+
            T.Bid 1 T.Notrump .+ ". and if partner has neither major, " .+
            "they're likely to have diamonds for you. It's possible partner " .+
            "has 3325 shape and gets stuck without a trump fit, but that's " .+
            "unlikely. On average, this will be better than passing " .+
            T.Bid 1 T.Notrump .+ "."
      in situation "garb" action B.b1N2C explanation
  in
    stdWrap sit


nongarbageStayman :: Situations
nongarbageStayman = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            makePass
            pointRange 8 40
        explanation =
            "Partner has opened a strong " .+ T.Bid 1 T.Notrump .+ ". We " .+
            "have a 4-card major and at least invitational strength. Bid " .+
            "Stayman, and see if you've got a fit with partner."
      in situation "stmn" action B.b1N2C explanation
  in
    stdWrap sit


noMajor :: Situations
noMajor = let
    sit = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            B.b1N2C
            makePass
        explanation =
            "We opened a strong " .+ T.Bid 1 T.Notrump .+ ", and partner " .+
            "has bid Stayman. We don't have a 4-card major, so bid " .+
            T.Bid 2 T.Diamonds .+ " to convey that. Partner is captain of " .+
            "the auction; they'll know what to do next."
      in situation "noMaj" action B.b1N2C2D explanation
  in
    stdWrap sit


oneMajor :: Situations
oneMajor = let
    sit (bid, shortSuit) = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            B.b1N2C
            makePass
            maxSuitLength shortSuit 3
        explanation =
            "We opened a strong " .+ T.Bid 1 T.Notrump .+ ", and partner " .+
            "has bid Stayman, asking whether we have any major suits. " .+
            "We've got one, so bid it. Partner is captain of " .+
            "the auction; they'll know what to do next."
      in situation "1Maj" action bid explanation
  in
    wrapVulDlr $ return sit <~ [(B.b1N2C2H, T.Spades), (B.b1N2C2S, T.Hearts)]


bothMajors :: Situations
bothMajors = let
    sit = let
        action = do
            setOpener T.South
            B.b1N
            makePass
            B.b1N2C
            makePass
            mapM_ (`suitLength` 4) T.majorSuits
        explanation =
            "We opened a strong " .+ T.Bid 1 T.Notrump .+ ", and partner " .+
            "has bid Stayman, asking whether we have any major suits. " .+
            "We've got both, so bid the cheaper one, hearts. If partner " .+
            "doesn't like that suit, they must have spades, and we'll " .+
            "bid those later."
      in situation "2Maj" action B.b1N2C2H explanation
  in
    stdWrap sit


topic :: Topic
topic = makeTopic "Stayman" "Stmn" situations
  where
    situations = wrap [ wrap [garbageStayman]
                      , nongarbageStayman
                      , noMajor
                      , oneMajor
                      , bothMajors
                      ]
