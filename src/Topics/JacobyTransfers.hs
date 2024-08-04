module Topics.JacobyTransfers(topic) where

import Action(Action, constrain)
import qualified Bids.OneNotrump as B
import CommonBids(setOpener)
import EDSL(forbid, makeCall, makeAlertableCall,
            pointRange, suitLength, minSuitLength, balancedHand)
import Output(Punct(NDash), (.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, Situations, wrap, stdWrap, wrapVulDlr, makeTopic)


-- syntactic sugar for writing descriptions of solutions
equalMajors :: Action
equalMajors = constrain "equal_majors" ["hearts(", ") == spades(", ")"]


jacobyTransfer :: T.Suit -> Action
jacobyTransfer T.Hearts = B.b1N2D
jacobyTransfer T.Spades = B.b1N2H
jacobyTransfer _        = error "jacoby transfer to non-major suit"


-- TODO: Add separate commentary for 5-4 non-gf hands. Alternately, forbid 5-4
-- non-gf hands, and add that situation into the Smolen topic.


setUpCompletion :: T.Suit -> Action
setUpCompletion suit = do
    setOpener T.South
    B.b1N
    B.noInterference
    forbid equalMajors
    jacobyTransfer suit
    B.noInterference  -- TODO: Allow overcalls of lower suits


initiateTransferWeak :: Situations
initiateTransferWeak = let
    sit bid = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            pointRange 0 7
            forbid equalMajors  -- With 5-5 in the majors, pick the better one.
        explanation =
            "Partner has opened a strong " .+ B.b1N .+ ". You have " .+
            "such a weak hand that you have no interest in game, but you do " .+
            "have a 5-card major. Playing in it, even if it's a 5-2 fit, is " .+
            "more likely to succeed than playing in notrump. Make a Jacoby " .+
            "transfer into the suit, then pass and leave partner at the 2 " .+
            "level."
      in
        situation "InitWeak" action bid explanation
  in
    wrapVulDlr $ return sit <~ [B.b1N2D, B.b1N2H]


initiateTransferBInv :: Situations
initiateTransferBInv = let
    sit (bid, suit) = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            pointRange 8 9
            forbid equalMajors
            balancedHand
        explanation =
            "Partner has opened a strong " .+ B.b1N .+ ". You have " .+
            "a balanced hand with invitational strength, and a 5-card " .+
            "major. Make a Jacoby transfer into the suit, then bid " .+
            T.Bid 2 T.Notrump .+ ". This gives partner the " .+
            "options of playing in notrump with 2-card " .+ init (show suit) .+
            " support or in " .+ suit .+ " with a fit, and the option " .+
            "of playing in partscore with a minimum hand and game with a " .+
            "maximum."
      in
        situation "InitBInv" action bid explanation
  in
    wrapVulDlr $ return sit <~ [(B.b1N2D, T.Hearts), (B.b1N2H, T.Spades)]


initiateTransferBGf :: Situations
initiateTransferBGf = let
    sit (bid, suit) = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            pointRange 10 14
            balancedHand
        explanation =
            "Partner has opened a strong " .+ B.b1N .+ ". You have " .+
            "a balanced hand with game-going but not slam-going strength, " .+
            "and a 5-card major. Make a Jacoby transfer into the suit, " .+
            "then bid " .+ T.Bid 3 T.Notrump .+ ". This gives partner the " .+
            "options of passing and playing in notrump with 2-card " .+
            init (show suit) .+ " support or correcting to " .+
            T.Bid 4 suit .+ " with a fit, knowing that you " .+
            "belong in game but not slam."
      in
        situation "InitBGF" action bid explanation
  in
    wrapVulDlr $ return sit <~ [(B.b1N2D, T.Hearts), (B.b1N2H, T.Spades)]


completeTransfer :: Situations
completeTransfer = let
    sit suit = let
        action = do
            setUpCompletion suit
            minSuitLength suit 3
        explanation =
            "You have opened a strong " .+ B.b1N .+ ", and partner\
          \ has made a Jacoby transfer. Complete the transfer by bidding the\
          \ next higher suit. Partner promises at least 5 cards in that major,\
          \ and will describe their hand further (possibly by passing with a\
          \ weak hand) afterward."
        bid = makeCall $ T.Bid 2 suit
      in
        situation "Complete" action bid explanation
  in
    wrapVulDlr $ return sit <~ T.majorSuits


completeTransferShort :: Situations
completeTransferShort = let
    sit suit = let
        action = do
            setUpCompletion suit
            suitLength suit 2
        explanation =
            "You have opened a strong " .+ B.b1N .+ ", and partner\
          \ has made a Jacoby transfer, indicating they have at least 5 " .+
            show suit .+ ". Even though you only have 2-card support, complete\
          \ the transfer by bidding the next higher suit. You have at least a\
          \ 7-card fit. If partner is very weak, " .+
            T.Bid 2 suit .+ " rates to play better than notrump,\
          \ and you want to be declarer so that your strong hand stays hidden.\
          \ If partner has at least invitational strength, he will make\
          \ another bid to give you options of where to play."
        bid = makeCall $ T.Bid 2 suit
      in
        situation "Short" action bid explanation
  in
    wrapVulDlr $ return sit <~ T.majorSuits


majors55inv :: Situations
majors55inv = let
    action = do
        setOpener T.North
        B.b1N
        B.noInterference
        suitLength T.Hearts 5
        suitLength T.Spades 5
        pointRange 7 9
    explanation =
        "Partner has opened a strong " .+ B.b1N .+ ". With 5" .+ NDash .+ "5 in\
      \ the majors and invitational strength, first make a Jacoby transfer\
      \ into hearts, and then bid " .+ T.Bid 2 T.Spades .+ "\
      \ afterwards. Partner will then have the options of passing " .+
        T.Bid 2 T.Spades .+ " with a minimum hand and a spade\
      \ fit, bidding " .+ T.Bid 3 T.Hearts .+ " with a minimum\
      \ hand and no spade fit (in which case a heart fit is guaranteed), or\
      \ bidding one of the majors at the 4 level with a maximum. This\
      \ wrong-sides the contract if we end up playing in spades."
    bid = makeAlertableCall (T.Bid 2 T.Diamonds)
                            ("Transfer to " ++ show T.Hearts)
  in
    stdWrap $ situation "55Inv" action bid explanation


majors55gf :: Situations
majors55gf = let
    sit = let
        action = do
            setOpener T.North
            B.b1N
            B.noInterference
            suitLength T.Hearts 5
            suitLength T.Spades 5
            pointRange 10 14
        explanation =
            "Partner has opened a strong " .+ B.b1N .+ ". With 5" .+
            NDash .+ "5 in the majors and\
            \ game-forcing strength, first make a Jacoby transfer into spades,\
            \ and then bid " .+ T.Bid 3 T.Hearts .+ " afterwards.\
            \ Partner will then have the options of which game to bid.\
            \ This wrong-sides the contract if we end up playing in hearts."
        bid = makeAlertableCall (T.Bid 2 T.Hearts)
                                ("Transfer to " ++ show T.Spades)
      in
        situation "55GF" action bid explanation
  in
    -- Note that with 5-5 and game-going strength, you would have opened the
    -- bidding if you had a chance. So, you must not have had a chance to bid
    -- before your partner.
    wrap $ return sit <~ T.allVulnerabilities <~ [T.West, T.North]


topic :: Topic
topic = makeTopic "Jacoby transfers"  "JacTrans" $
    wrap [ initiateTransferWeak
         , initiateTransferBInv
         , initiateTransferBGf
         , completeTransfer
         , completeTransferShort
         , wrap [majors55gf, majors55inv]
         ]
