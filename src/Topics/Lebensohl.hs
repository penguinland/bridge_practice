module Topics.Lebensohl(topic) where

import qualified Bids.Cappelletti as Capp
import qualified Bids.DONT as DONT
import qualified Bids.Lebensohl as Leb
import qualified Bids.Meckwell as MW
import qualified Bids.NaturalOneNotrumpDefense as Nat
import CommonBids(setOpener)
--import EDSL(makePass, pointRange, suitLength, maxSuitLength, forEach)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, Situations, makeTopic)


ignoreOpps :: Situations
ignoreOpps = let
    sit (overcall, response, overcallIs2C) = let
        action = do
            setOpener T.North
            Leb.b1N
            overcall
        explanation =
            "Partner opened a strong " .+ Leb.b1N .+ ", and RHO interfered " .+
            "with the auction. However, their call didn't actually take up " .+
            "any of our bidding room. Ignore it, and use our usual systems " .+
            "over notrump" .+
            (if overcallIs2C then " (double of " .+ overcall .+ " is Stayman)"
                            else mempty) .+ "."
      in situation "ignr" action response explanation
  in
    -- East should be an unpassed hand to interfere over North's notrump.
    wrap $ return sit <~ [ (DONT.b1NoX,  Leb.b1NoX2C,  False)
                         , (DONT.b1NoX,  Leb.b1NoX2D,  False)
                         , (DONT.b1NoX,  Leb.b1NoX2H,  False)
                         , (MW.b1NoX,    Leb.b1NoX2C,  False)
                         , (MW.b1NoX,    Leb.b1NoX2D,  False)
                         , (MW.b1NoX,    Leb.b1NoX2H,  False)
                         , (Capp.b1NoX,  Leb.b1NoX2C,  False)
                         , (Capp.b1NoX,  Leb.b1NoX2D,  False)
                         , (Capp.b1NoX,  Leb.b1NoX2H,  False)
                         , (DONT.b1No2C, Leb.b1No2CX,  True)
                         , (DONT.b1No2C, Leb.b1No2C2D, True)
                         , (DONT.b1No2C, Leb.b1No2C2H, True)
                         , (MW.b1No2C,   Leb.b1No2CX,  True)
                         , (MW.b1No2C,   Leb.b1No2C2D, True)
                         , (MW.b1No2C,   Leb.b1No2C2H, True)
                         , (Capp.b1No2C, Leb.b1No2CX,  True)
                         , (Capp.b1No2C, Leb.b1No2C2D, True)
                         , (Capp.b1No2C, Leb.b1No2C2H, True)
                         , (Nat.b1No2C,  Leb.b1No2CX,  True)
                         , (Nat.b1No2C,  Leb.b1No2C2D, True)
                         , (Nat.b1No2C,  Leb.b1No2C2H, True)
                         ]
                      <~ [T.North, T.South, T.West]
                      <~ T.allVulnerabilities


signoff :: Situations
signoff = let
    sit (overcall, response) = let
        action = do
            setOpener T.North
            Leb.b1N
            overcall
        explanation =
            "Partner opened a strong " .+ Leb.b1N .+ ", and RHO interfered " .+
            "with the auction. We're so weak we don't even want to invite " .+
            "to game, but we do have enough strength to suspect this is " .+
            "our contract. Bid our suit at the 2 level, as signoff."
      in situation "snof" action response explanation
  in
    wrap $ return sit <~ [ (DONT.b1No2D, Leb.b1No2D2H)
                         , (DONT.b1No2D, Leb.b1No2D2S)
                         -- If RHO shows both majors, don't bid a major!
                         --, (DONT.b1No2H, Leb.b1No2H2S)
                         , (MW.b1No2D,   Leb.b1No2D2H)
                         , (MW.b1No2D,   Leb.b1No2D2S)
                         , (MW.b1No2H,   Leb.b1No2H2S)
                         -- If RHO shows both majors, don't bid a major!
                         --, (Capp.b1No2D, Leb.b1No2D2H)
                         --, (Capp.b1No2D, Leb.b1No2D2S)
                         , (Capp.b1No2H, Leb.b1No2H2S)
                         , (Nat.b1No2D,  Leb.b1No2D2H)
                         , (Nat.b1No2D,  Leb.b1No2D2S)
                         , (Nat.b1No2H,  Leb.b1No2H2S)
                         ]
                      -- East should be an unpassed hand to interfere.
                      <~ [T.North, T.South, T.West]
                      <~ T.allVulnerabilities


gameForce :: Situations
gameForce = let
    sit (overcall, response) = let
        action = do
            setOpener T.North
            Leb.b1N
            overcall
        explanation =
            "Partner opened a strong " .+ Leb.b1N .+ ", and RHO interfered " .+
            "with the auction. We're at least game-forcing, so should " .+
            "bid our suit at the 3 level. Partner will bid naturally, and " .+
            "we'll find a game (likely either our suit or notrump)."
      in situation "gfnat" action response explanation
  in
    wrap $ return sit <~ [
                         ]
                      -- East should be an unpassed hand to interfere.
                      <~ [T.North, T.South, T.West]
                      <~ T.allVulnerabilities


-- TODO:
-- natural GF bids at the 3 level
-- jump to 3N
-- relay to 3N (answer should be 2N planning to rebid 3N)
-- cue bid for Stayman
-- relay to cue bid (answer should be 2N planning to rebid the cue)
-- complete the relay
-- Texas transfers over interference (in the Texas Transfers topic)
-- make the opponents sometimes use natural overcalls


topic :: Topic
topic = makeTopic
    ("lebensohl after interference over our " .+ T.Bid 1 T.Notrump)
    "leb1N" situations
  where
    situations = wrap [ ignoreOpps
                      , signoff
                      , gameForce
                      ]
