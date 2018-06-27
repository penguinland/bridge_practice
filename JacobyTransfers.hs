module JacobyTransfers(topic) where

import Data.List.Utils(join)

import Output(output, Punct(NDash))
import Topic(Topic(..), Situations, base, (<~), wrap)
import Auction(forbid, makeCall, makePass, pointRange, suitLength,
               minSuitLength, maxSuitLength, Action, balancedHand, withholdBid,
               constrain)
import Situation(situation, Situation)
import qualified Terminology as T
import qualified CommonBids as B


-- syntactic sugar
oneNT :: T.Call
oneNT = T.Bid 1 T.Notrump


transferSuit :: T.Suit -> T.Suit
transferSuit T.Hearts = T.Diamonds
transferSuit T.Spades = T.Hearts
transferSuit _        = error "Jacoby-like transfer of non-major suit!"


otherMajor :: T.Suit -> T.Suit
otherMajor T.Hearts = T.Spades
otherMajor T.Spades = T.Hearts
otherMajor _        = error "Other major of non-major suit!"


-- Although this topic is about Jacoby transfers, we exclude auctions that would
-- be better served by a Texas transfer so as not to confuse the learner.
texasTransfer :: T.Suit -> Action
texasTransfer suit = do
    minSuitLength suit 6
    pointRange 10 15
    makeCall (T.Bid 4 $ transferSuit suit)


equalMajors :: Action
equalMajors = constrain "equal_majors" ["hearts(", ") == spades(", ")"]


smolen :: T.Suit -> Action  -- The suit is the longer major.
smolen suit = do
    minSuitLength suit 5
    minSuitLength (otherMajor suit) 4
    pointRange 10 40
    -- With 5-5 in the majors, make a Jacoby transfer then bid the other suit.
    -- With 6-6, I guess you do the same? but it never comes up.
    forbid equalMajors


prepareJacobyTransfer :: T.Suit -> Action
prepareJacobyTransfer suit = do
    minSuitLength suit 5
    forbid (texasTransfer suit)
    forbid (smolen suit)


jacobyTransfer :: T.Suit -> Action
jacobyTransfer suit = do
    prepareJacobyTransfer suit
    -- Make this simple by leaving out 5-5 hands. They go in another situation.
    forbid equalMajors
    makeCall (T.Bid 2 $ transferSuit suit)

-- TODO: Add separate commentary for 5-4 non-gf hands. Alternately, forbid 5-4
-- non-gf hands, and add that situation into the Smolen topic.


setUpTransfer :: T.Direction -> T.Suit -> Action
setUpTransfer dealer suit = do
    B.setDealerAndOpener dealer T.North
    B.strong1NT
    B.cannotPreempt >> makePass
    prepareJacobyTransfer suit


setUpCompletion :: T.Direction -> T.Suit -> Action
setUpCompletion dealer suit = do
    B.setDealerAndOpener dealer T.South
    B.strong1NT
    B.cannotPreempt >> makePass
    jacobyTransfer suit
    B.cannotPreempt >> makePass  -- TODO: Allow overcalls of lower suits


initiateTransferWeak :: Situations
initiateTransferWeak = let
    sit dealer suit vul = let
        action = do
            setUpTransfer dealer suit
            pointRange 0 7
            forbid equalMajors  -- With 5-5 in the majors, pick the better one.
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ". You have\
           \ such a weak hand that you have no interest in game, but you do have\
           \ a 5-card major. Playing in it, even if it's a 5-2 fit, is more\
           \ likely to succeed than playing in notrump. Make a Jacoby transfer\
           \ into the suit, then pass and leave partner at the 2 level."
      in
        situation dealer vul action (T.Bid 2 $ transferSuit suit) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.majorSuits <~ T.allVulnerabilities


initiateTransferBInv :: Situations
initiateTransferBInv = let
    sit dealer suit vul = let
        action = do
            setUpTransfer dealer suit
            pointRange 8 9
            forbid equalMajors
            balancedHand
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ". You have\
           \ a balanced hand with invitational strength, and a 5-card major.\
           \ Make a Jacoby transfer into the suit, then bid " ++
             output fmt (T.Bid 2 T.Notrump) ++ ". This gives partner the\
           \ options of playing in notrump with 2-card " ++ init (show suit) ++
             " support or in " ++ show suit ++ " with a fit, and the option of\
           \ playing in partscore with a minimum hand and game with a maximum."
      in
        situation dealer vul action (T.Bid 2 $ transferSuit suit) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.majorSuits <~ T.allVulnerabilities


initiateTransferBGf :: Situations
initiateTransferBGf = let
    sit dealer suit vul = let
        action = do
            setUpTransfer dealer suit
            pointRange 10 14
            forbid equalMajors
            balancedHand
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ". You have\
           \ a balanced hand with game-going but not slam-going strength, and a\
           \ 5-card major. Make a Jacoby transfer into the suit, then bid " ++
             output fmt (T.Bid 3 T.Notrump) ++ ". This gives partner the\
           \ options of passing and playing in notrump with 2-card " ++
             init (show suit) ++ " support or correcting to " ++
             output fmt (T.Bid 4 suit) ++ " with a fit, knowing that you\
           \ belong in game but not slam."
      in
        situation dealer vul action (T.Bid 2 $ transferSuit suit) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.majorSuits <~ T.allVulnerabilities


completeTransfer :: Situations
completeTransfer = let
    sit dealer suit vul = let
        higherSuits = if suit == T.Spades then [] else [T.Spades]
        action = do
            setUpCompletion dealer suit
            minSuitLength suit 3
        explanation fmt =
            "You have opened a strong " ++ output fmt oneNT ++ ", and partner\
          \ has made a Jacoby transfer. Complete the transfer by bidding the\
          \ next higher suit. Partner promises at least 5 cards in that major,\
          \ but wants you to be declarer so your stronger hand stays hidden."
      in
        situation dealer vul action (T.Bid 2 suit) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.majorSuits <~ T.allVulnerabilities


completeTransferShort :: Situations
completeTransferShort = let
    sit dealer suit vul = let
        higherSuits = if suit == T.Spades then [] else [T.Spades]
        action = do
            setUpCompletion dealer suit
            suitLength suit 2
        explanation fmt =
            "You have opened a strong " ++ output fmt oneNT ++ ", and partner\
          \ has made a Jacoby transfer, indicating they have at least 5 " ++
            show suit ++ ". Even though you only have 2-card support, complete\
          \ the transfer by bidding the next higher suit. You have at least a\
          \ 7-card fit. If partner is very weak, " ++
            output fmt (T.Bid 2 suit) ++ " rates to play better than notrump,\
          \ and you want to be declarer so that your strong hand stays hidden.\
          \ If partner has at least invitational strength, he will make\
          \ another bid to give you options of where to play."
      in
        situation dealer vul action (T.Bid 2 suit) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.majorSuits <~ T.allVulnerabilities


majors55inv :: Situations
majors55inv = let
    sit dealer vul = let
        action = do
            B.setDealerAndOpener dealer T.North
            B.strong1NT
            B.cannotPreempt >> makePass
            suitLength T.Hearts 5
            suitLength T.Spades 5
            pointRange 7 9
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ". With 5" ++
            output fmt NDash ++ "5 in\
          \ the majors and invitational strength, first make a Jacoby transfer\
          \ into hearts, and then bid " ++ output fmt (T.Bid 2 T.Spades) ++ "\
          \ afterwards. Partner will then have the options of passing " ++
            output fmt (T.Bid 2 T.Spades) ++ " with a minimum hand and a spade\
          \ fit, bidding " ++ output fmt (T.Bid 3 T.Hearts) ++ " with a minimum\
          \ hand and no spade fit (in which case a heart fit is guaranteed), or\
          \ bidding one of the majors at the 4 level with a maximum. This\
          \ wrong-sides the contract when the " ++ output fmt oneNT ++ " bidder\
          \ has a doubleton heart."
      in
        situation dealer vul action (T.Bid 2 T.Diamonds) explanation
  in
    wrap $ base sit <~ T.allDirections <~ T.allVulnerabilities


majors55gf :: Situations
majors55gf = let
    sit dealer vul = let
        action = do
            B.setDealerAndOpener dealer T.North
            B.strong1NT
            B.cannotPreempt >> makePass
            suitLength T.Hearts 5
            suitLength T.Spades 5
            pointRange 10 14
        explanation fmt =
            "Partner has opened a strong " ++ output fmt oneNT ++ ". With 5" ++
            output fmt NDash ++ "5 in\
            \ the majors and\
            \ game-forcing strength, first make a Jacoby transfer into spades,\
            \ and then bid " ++ output fmt (T.Bid 3 T.Hearts) ++ " afterwards.\
            \ Partner will then have the options of which game to bid.\
            \ This wrong-sides the contract when the " ++ output fmt oneNT ++ "\
            \ bidder has a doubleton spade."
      in
        situation dealer vul action (T.Bid 2 T.Hearts) explanation
  in
    -- Note that with 5-5 and game-going strength, you would have opened the
    -- bidding if you had a chance. So, you must not have had a chance to bid
    -- before your partner.
    wrap $ base sit <~ [T.West, T.North] <~ T.allVulnerabilities


topic :: Topic
topic = Topic "Jacoby transfers" $ wrap [ initiateTransferWeak
                                        , initiateTransferBInv
                                        , initiateTransferBGf
                                        , completeTransfer
                                        , completeTransferShort
                                        -- Combine the rare situations together
                                        , wrap [majors55gf, majors55inv]
                                        ]
