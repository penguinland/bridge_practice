module Topics.MinorTransfersScott(topic) where

import Data.List.Utils(join)

import Output(output, Punct(NDash))
import Topic(Topic(..), Situations, base, (<~), wrap)
import Auction(forbid, makeCall, makePass, pointRange, suitLength,
               minSuitLength, maxSuitLength, Action, balancedHand, withholdBid,
               hasTopN, constrain)
import Situation(situation, Situation)
import qualified Terminology as T
import qualified CommonBids as B


-- Note that the person initiating the transfer shouldn't have had an
-- opportunity to open, because they're very likely to have tried to open 2D or
-- 3C. Instead, force the notrump opener to be in either first or second seat.


-- syntactic sugar
oneNT :: T.Call
oneNT = T.Bid 1 T.Notrump


transferSuit :: T.Suit -> T.Suit
transferSuit T.Clubs    = T.Spades
transferSuit T.Diamonds = T.Notrump
transferSuit _          = error "Minor-like transfer of non-minor suit!"


superacceptBid :: T.Suit -> T.Call
superacceptBid T.Clubs    = T.Bid 2 T.Notrump
superacceptBid T.Diamonds = T.Bid 3 T.Clubs
superacceptBid _          = error "Attempted superaccept of non-minor"


minorTransfer :: T.Suit -> Action
minorTransfer suit = do
    minSuitLength suit 6
    forbid (minSuitLength T.Spades 5)
    forbid (minSuitLength T.Hearts 5)
    -- If you've got minor-suit-game-going values, bid 3S to invoke minor-suit
    -- Smolen instead.
    pointRange 5 12
    makeCall (T.Bid 2 $ transferSuit suit)


setUpTransfer :: T.Direction -> T.Suit -> Action
setUpTransfer dealer suit = do
    B.setDealerAndOpener dealer T.North
    B.strong1NT
    B.cannotPreempt >> makePass
    withholdBid (minorTransfer suit)


setUpCompletion :: T.Direction -> T.Suit -> Action
setUpCompletion dealer suit = do
    B.setDealerAndOpener dealer T.South
    B.strong1NT
    B.cannotPreempt >> makePass
    minorTransfer suit
    B.cannotPreempt >> makePass  -- TODO: Allow overcalls of lower suits


canSuperaccept :: T.Suit -> Action
canSuperaccept suit = do
    minSuitLength suit 3
    constrain ("has_control_" ++ (show suit))
        ["top2(", ", " ++ show suit ++ ") >= 1"]


setUpSuperacceptCompletion :: T.Direction -> T.Suit -> Action
setUpSuperacceptCompletion dealer suit = do
    B.setDealerAndOpener dealer T.North
    B.strong1NT
    B.cannotPreempt >> makePass
    minorTransfer suit
    B.cannotPreempt >> makePass  -- TODO: Allow overcalls of lower suits
    canSuperaccept suit
    makeCall (superacceptBid suit)
    makePass  -- Still can't do anything just like the previous round


initiateTransfer :: Situations
initiateTransfer = let
    sit dealer suit vul = let
        action = do
            setUpTransfer dealer suit
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ". You have\
           \ an unbalaned hand with a long minor. Bid 2 steps below your suit\
           \ to transfer into it and make your partner declarer so their\
           \ stronger hand stays hidden."
      in
        situation dealer vul action (T.Bid 2 $ transferSuit suit) explanation
  in
    wrap $ base sit <~ [T.West, T.North] <~ T.minorSuits <~ T.allVulnerabilities


completeTransfer :: Situations
completeTransfer = let
    sit dealer suit vul = let
        action = do
            setUpCompletion dealer suit
            forbid (canSuperaccept suit)
        explanation fmt =
            "You have opened a strong " ++ output fmt oneNT ++ ", and partner\
          \ has made a minor suit transfer into " ++ show suit ++ ". You need\
          \ at least 3-card support including 1 of the top 2 cards to\
          \ superaccept, so just accept the transfer regularly and bid the\
          \ suit."
      in
        situation dealer vul action (T.Bid 3 suit) explanation
  in
    wrap $ base sit <~ [T.East, T.South] <~ T.minorSuits <~ T.allVulnerabilities


superacceptTransfer :: Situations
superacceptTransfer = let
    sit dealer suit vul = let
        action = do
            setUpCompletion dealer suit
            canSuperaccept suit
        explanation fmt =
            "You have opened a strong " ++ output fmt oneNT ++ ", and partner\
          \ has made a minor suit transfer into " ++ show suit ++ ". You have\
          \ at least 3-card support including 1 of the top 2 honors,\
          \ so superaccept the transfer by bidding 1 step up from\
          \ the transfer (1 step under the intended suit). This gives partner\
          \ the option of bidding " ++ output fmt (T.Bid 3 T.Notrump) ++ " if\
          \ they think the suit will run, and wrong-siding the " ++
            output fmt (T.Bid 3 suit) ++ " contract if it won't."
      in
        situation dealer vul action (superacceptBid suit) explanation
  in
    wrap $ base sit <~ [T.East, T.South] <~ T.minorSuits <~ T.allVulnerabilities


completeSuperacceptAKQ :: Situations
completeSuperacceptAKQ = let
    sit dealer suit vul = let
        action = do
            setUpSuperacceptCompletion dealer suit
            hasTopN suit 3 2
            pointRange 0 7  -- Not a situation where you'd naively bid 3N!
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ", you have\
          \ made a minor suit transfer in " ++ show suit ++ ", and partner has\
          \ superaccepted the transfer, showing at least 3-card support and\
          \ either the ace or king of the suit. The suit is running, so bid\
          \ a thin " ++ output fmt (T.Bid 3 T.Notrump) ++ ". If opener has a\
          \ stopper in every other suit, the game is likely be makable,\
          \ despite your lack of points."
      in
        situation dealer vul action (T.Bid 3 T.Notrump) explanation
  in
    wrap $ base sit <~ [T.North, T.West] <~ T.minorSuits <~ T.allVulnerabilities


completeSuperacceptKQJ10 :: Situations
completeSuperacceptKQJ10 = let
    sit dealer suit vul = let
        action = do
            setUpSuperacceptCompletion dealer suit
            forbid (hasTopN suit 3 2)
            hasTopN suit 5 3
            pointRange 0 7  -- Not a situation where you'd naively bid 3N!
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ", you have\
          \ made a minor suit transfer in " ++ show suit ++ ", and partner has\
          \ superaccepted the transfer, showing at least 3-card support and\
          \ either the ace or king of the suit. Although you don't have all\
          \ three top honors, you've got 4 of the top 5, so the suit is very\
          \ likely to run.  Bid a thin " ++ output fmt (T.Bid 3 T.Notrump) ++
            ". If opener has a stopper in every other suit, the game is likely\
          \ be makable, despite your lack of points."
      in
        situation dealer vul action (T.Bid 3 T.Notrump) explanation
  in
    wrap $ base sit <~ [T.North, T.West] <~ T.minorSuits <~ T.allVulnerabilities


completeSuperaccept10CardFit :: Situations
completeSuperaccept10CardFit = let
    sit dealer suit vul = let
        action = do
            setUpSuperacceptCompletion dealer suit
            hasTopN suit 2 1
            pointRange 0 7  -- Not a situation where you'd normally bid 3N!
            minSuitLength suit 7
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ", you have\
          \ made a minor suit transfer in " ++ show suit ++ ", and partner has\
          \ superaccepted the transfer, showing at least 3-card support and\
          \ either the ace or king of the suit. With your extra length and\
          \ both top honors, the suit should be running. Bid the notrump\
          \ game."
      in
        situation dealer vul action (T.Bid 3 T.Notrump) explanation
  in
    wrap $ base sit <~ [T.North, T.West] <~ T.minorSuits <~ T.allVulnerabilities


failSuperaccept :: Situations
failSuperaccept = let
    sit dealer suit vul = let
        action = do
            setUpSuperacceptCompletion dealer suit
            forbid (hasTopN suit 2 1)
            forbid (hasTopN suit 5 3)
            pointRange 0 7  -- SKip invitational hands; there's too much nuance
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ", you have\
          \ made a minor suit transfer in " ++ show suit ++ ", and partner has\
          \ superaccepted the transfer, showing at least 3-card support and\
          \ either the ace or king of the suit. Despite the fit, it doesn't\
          \ look like the suit is going to run, so a notrump game probably\
          \ won't make. Sign off in partscore, even though you're wrong-siding\
          \ the contract."
      in
        situation dealer vul action (T.Bid 3 suit) explanation
  in
    wrap $ base sit <~ [T.North, T.West] <~ T.minorSuits <~ T.allVulnerabilities


garbageTransfer :: Situations
garbageTransfer = let
    sit dealer vul = let
        action = do
            B.setDealerAndOpener dealer T.North
            B.strong1NT
            B.cannotPreempt >> makePass
            minSuitLength T.Clubs 5
            minSuitLength T.Diamonds 5
            pointRange 0 3  -- Very weak
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ". You are\
          \ weak enough that it's unlikely to make, but you are also at least\
          \ 5" ++ output fmt NDash ++ "5 in the minors. Pretend to make a\
          \ transfer into diamonds. Regardless of whether partner superaccepts\
          \ or completes the transfer normally, you can pass, knowing you're\
          \ in at least a 7-card fit and likely an 8-card fit, which is likely\
          \ to play better than staying in " ++ output fmt oneNT ++ ", even at\
          \ the 3 level."
      in
        situation dealer vul action (T.Bid 2 T.Notrump) explanation
  in
    wrap $ base sit <~ [T.North, T.West] <~ T.allVulnerabilities


notrumpInvite :: Situations
notrumpInvite = let
    sit dealer vul = let
        action = do
            B.setDealerAndOpener dealer T.North
            B.strong1NT
            B.cannotPreempt >> makePass
            balancedHand
            maxSuitLength T.Spades 3
            maxSuitLength T.Hearts 3
            pointRange 8 9
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ". You have\
          \ a balanced hand with invitational strength. Even though you don't\
          \ have a 4-card major, bid " ++ output fmt (T.Bid 2 T.Clubs) ++ "\
          \ as Stayman, intending to rebid " ++ output fmt (T.Bid 2 T.Notrump)
            ++ " regardless of the reply. Note that the " ++
            output fmt (T.Bid 2 T.Notrump) ++ " bid is alertable and should\
          \ be described as indicating that you might not have a 4-card major.\
          \ The Stayman bid is not alertable. This frees up a direct " ++
            output fmt (T.Bid 2 T.Notrump) ++ " reply to be a minor suit\
          \ transfer."
      in
        situation dealer vul action (T.Bid 2 T.Clubs) explanation
  in
    wrap $ base sit <~ [T.North, T.West] <~ T.allVulnerabilities


topic :: Topic
topic = Topic "Scott's version of minor suit transfers" "MinTransScott" $
        wrap [ initiateTransfer
             , completeTransfer
             , superacceptTransfer
             , wrap [ completeSuperacceptAKQ
                    , completeSuperacceptKQJ10
                    , completeSuperaccept10CardFit
                    ]
             , failSuperaccept
             -- , garbageTransfer
             , notrumpInvite
             ]
