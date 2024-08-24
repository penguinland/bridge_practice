module Topics.MinorTransfersScott(topic) where

import Action(Action, constrain, withholdBid)
import qualified CommonBids as B
import EDSL(forbid, makeCall, makeAlertableCall, makePass, pointRange,
            hasTopN, minSuitLength, maxSuitLength, balancedHand)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, makeTopic, wrap, wrapVulNW, wrapVulSE, stdWrapNW,
             Situations)


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
    makeAlertableCall (T.Bid 2 $ transferSuit suit)
                      ("Transfer to " ++ show suit)


setUpTransfer :: T.Suit -> Action
setUpTransfer suit = do
    B.setOpener T.North
    B.strong1NT
    B.cannotPreempt >> makePass
    withholdBid (minorTransfer suit)


setUpCompletion :: T.Suit -> Action
setUpCompletion suit = do
    B.setOpener T.South
    B.strong1NT
    B.cannotPreempt >> makePass
    minorTransfer suit
    B.cannotPreempt >> makePass  -- TODO: Allow overcalls of lower suits


canSuperaccept :: T.Suit -> Action
canSuperaccept suit = do
    minSuitLength suit 3
    constrain ("has_control_" ++ show suit)
        ["top2(", ", " ++ show suit ++ ") >= 1"]


setUpSuperacceptCompletion :: T.Suit -> Action
setUpSuperacceptCompletion suit = do
    B.setOpener T.North
    B.strong1NT
    B.cannotPreempt >> makePass
    minorTransfer suit
    B.cannotPreempt >> makePass  -- TODO: Allow overcalls of lower suits
    canSuperaccept suit
    makeAlertableCall (superacceptBid suit) ("Super-accept in " ++ show suit)
    makePass  -- Still can't do anything just like the previous round


initiateTransfer :: Situations
initiateTransfer = let
    sit suit = let
        action = do
            setUpTransfer suit
        explanation =
            "Partner has opened a strong " .+ oneNT .+ ". We have\
           \ an unbalaned hand with a long minor. Bid 2 steps below our suit\
           \ to transfer into it and make our partner declarer so their\
           \ stronger hand stays hidden."
        bid = minorTransfer suit
      in
        situation "Init" action bid explanation
  in
    wrapVulNW $ return sit <~ T.minorSuits


completeTransfer :: Situations
completeTransfer = let
    sit suit = let
        action = do
            setUpCompletion suit
            forbid (canSuperaccept suit)
        explanation =
            "We have opened a strong " .+ oneNT .+ ", and partner\
          \ has made a minor suit transfer into " .+ suit .+ ". We need\
          \ at least 3-card support including 1 of the top 2 cards to\
          \ superaccept, so just accept the transfer regularly and bid the\
          \ suit."
        bid = makeCall $ T.Bid 3 suit
      in
        situation "Complete" action bid explanation
  in
    wrapVulSE $ return sit <~ T.minorSuits


superacceptTransfer :: Situations
superacceptTransfer = let
    sit suit = let
        action = do
            setUpCompletion suit
            canSuperaccept suit
        explanation =
            "We have opened a strong " .+ oneNT .+ ", and partner\
          \ has made a minor suit transfer into " .+ suit .+ ". We have\
          \ at least 3-card support including 1 of the top 2 honors,\
          \ so superaccept the transfer by bidding 1 step up from\
          \ the transfer (1 step under the intended suit). This gives partner\
          \ the option of bidding " .+ T.Bid 3 T.Notrump .+ " if\
          \ they think the suit will run, and wrong-siding the " .+
            T.Bid 3 suit .+ " contract if it won't."
        bid = setUpSuperacceptCompletion suit
      in
        situation "SupAcc" action bid explanation
  in
    wrapVulSE $ return sit <~ T.minorSuits


completeSuperacceptAKQ :: Situations
completeSuperacceptAKQ = let
    sit suit = let
        action = do
            setUpSuperacceptCompletion suit
            hasTopN suit 3 2
            pointRange 0 7  -- Not a situation where you'd naively bid 3N!
        explanation =
            "Partner has opened a strong " .+ oneNT .+ ", we have\
          \ made a minor suit transfer in " .+ suit .+ ", and partner has\
          \ superaccepted the transfer, showing at least 3-card support and\
          \ either the ace or king of the suit. The suit is running, so bid\
          \ a thin " .+ T.Bid 3 T.Notrump .+ ". If opener has a\
          \ stopper in every other suit, the game is likely be makable,\
          \ despite your lack of points."
        bid = makeCall $ T.Bid 3 T.Notrump
      in
        situation "3NTAKQ" action bid explanation
  in
    wrapVulNW $ return sit <~ T.minorSuits


completeSuperacceptKQJ10 :: Situations
completeSuperacceptKQJ10 = let
    sit suit = let
        action = do
            setUpSuperacceptCompletion suit
            forbid (hasTopN suit 3 2)
            hasTopN suit 5 3
            pointRange 0 7  -- Not a situation where you'd naively bid 3N!
        explanation =
            "Partner has opened a strong " .+ oneNT .+ ", we have\
          \ made a minor suit transfer in " .+ suit .+ ", and partner has\
          \ superaccepted the transfer, showing at least 3-card support and\
          \ either the ace or king of the suit. Although we don't have all\
          \ three top honors, you've got 4 of the top 5, so the suit is very\
          \ likely to run.  Bid a thin " .+ T.Bid 3 T.Notrump .+
            ". If opener has a stopper in every other suit, the game is likely\
          \ be makable, despite your lack of points."
        bid = makeCall $ T.Bid 3 T.Notrump
      in
        situation "3NTKQJ" action bid explanation
  in
    wrapVulNW $ return sit <~ T.minorSuits


completeSuperaccept10CardFit :: Situations
completeSuperaccept10CardFit = let
    sit suit = let
        action = do
            setUpSuperacceptCompletion suit
            hasTopN suit 2 1
            pointRange 0 7  -- Not a situation where you'd normally bid 3N!
            minSuitLength suit 7
        explanation =
            "Partner has opened a strong " .+ oneNT .+ ", we have\
          \ made a minor suit transfer in " .+ suit .+ ", and partner has\
          \ superaccepted the transfer, showing at least 3-card support and\
          \ either the ace or king of the suit. With your extra length and\
          \ both top honors, the suit should be running. Bid the notrump\
          \ game."
        bid = makeCall $ T.Bid 3 T.Notrump
      in
        situation "3NT10Fit" action bid explanation
  in
    wrapVulNW $ return sit <~ T.minorSuits


failSuperaccept :: Situations
failSuperaccept = let
    sit suit = let
        action = do
            setUpSuperacceptCompletion suit
            forbid (hasTopN suit 2 1)
            forbid (hasTopN suit 5 3)
            pointRange 0 7  -- SKip invitational hands; there's too much nuance
        explanation =
            "Partner has opened a strong " .+ oneNT .+ ", we have\
          \ made a minor suit transfer in " .+ suit .+ ", and partner has\
          \ superaccepted the transfer, showing at least 3-card support and\
          \ either the ace or king of the suit. Despite the fit, it doesn't\
          \ look like the suit is going to run, so a notrump game probably\
          \ won't make. Sign off in partscore, even though you're wrong-siding\
          \ the contract."
        bid = makeCall $ T.Bid 3 suit
      in
        situation "SupFail" action bid explanation
  in
    wrapVulNW $ return sit <~ T.minorSuits


notrumpInvite :: Situations
notrumpInvite = let
    sit = let
        action = do
            B.setOpener T.North
            B.strong1NT
            B.cannotPreempt >> makePass
            balancedHand
            maxSuitLength T.Spades 3
            maxSuitLength T.Hearts 3
            pointRange 8 9
        explanation =
            "Partner has opened a strong " .+ oneNT .+ ". We have\
          \ a balanced hand with invitational strength. Even though we don't\
          \ have a 4-card major, bid " .+ T.Bid 2 T.Clubs .+ "\
          \ as Stayman, intending to rebid " .+ T.Bid 2 T.Notrump
            .+ " regardless of the reply. Note that the " .+
            T.Bid 2 T.Notrump .+ " bid is alertable and should\
          \ be described as indicating that we might not have a 4-card major.\
          \ The Stayman bid is not alertable. This frees up a direct " .+
            T.Bid 2 T.Notrump .+ " reply to be a minor suit\
          \ transfer."
        bid = makeCall $ T.Bid 2 T.Clubs
      in
        situation "NTInv" action bid explanation
  in
    stdWrapNW sit


topic :: Topic
topic = makeTopic "Scott's version of minor suit transfers" "MTScott" $
        wrap [ initiateTransfer
             , completeTransfer
             , superacceptTransfer
             , wrap [ completeSuperacceptAKQ
                    , completeSuperacceptKQJ10
                    , completeSuperaccept10CardFit
                    ]
             , failSuperaccept
             , notrumpInvite
             ]
