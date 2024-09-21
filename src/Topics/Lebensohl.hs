module Topics.Lebensohl(topic) where

import Control.Monad(join)

import Action(extractLastCall, withholdBid)
import qualified Bids.Cappelletti as Capp
import qualified Bids.DONT as DONT
import qualified Bids.Lebensohl as Leb
import qualified Bids.Meckwell as MW
import qualified Bids.NaturalOneNotrumpDefense as Nat
import CommonBids(setOpener)
--import EDSL(makePass, pointRange, suitLength, maxSuitLength, forEach)
import Output(Description, (.+))
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


signoff2 :: Situations
signoff2 = let
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
      in situation "so2" action response explanation
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
    sit (overcall, responses) dlr vul = let
        action = do
            setOpener T.North
            Leb.b1N
            overcall
        explanation =
            "Partner opened a strong " .+ Leb.b1N .+ ", and RHO interfered " .+
            "with the auction. We're at least game-forcing, so should " .+
            "bid our suit at the 3 level. Partner will bid naturally, and " .+
            "we'll find a game (likely either our suit or notrump)."
        inner response = situation "gfnat" action response explanation dlr vul
      in return inner <~ responses
  in
    wrap . join $ return sit
        <~ [ (Nat.b1No2D,  [Leb.b1No2D3C, Leb.b1No2D3H, Leb.b1No2D3S])
           , (Nat.b1No2H,  [Leb.b1No2H3C, Leb.b1No2H3D, Leb.b1No2H3S])
           , (Nat.b1No2S,  [Leb.b1No2S3C, Leb.b1No2S3D, Leb.b1No2S3H])
           , (DONT.b1No2D, [Leb.b1No2D3C, Leb.b1No2D3H, Leb.b1No2D3S])
           -- Don't bid either major when the opponents have shown both
           , (DONT.b1No2H, [Leb.b1No2H3C, Leb.b1No2H3D])
           , (DONT.b1No2S, [Leb.b1No2S3C, Leb.b1No2S3D, Leb.b1No2S3H])
           , (MW.b1No2D,   [Leb.b1No2D3C, Leb.b1No2D3H, Leb.b1No2D3S])
           , (MW.b1No2H,   [Leb.b1No2H3C, Leb.b1No2H3D, Leb.b1No2H3S])
           , (MW.b1No2S,   [Leb.b1No2S3C, Leb.b1No2S3D, Leb.b1No2S3H])
           -- Again, don't bid a major when RHO has them both. 3D should be
           -- natural and not a cue bid, because you'd never want to have a
           -- Stayman-like bid when RHO has shown both majors.
           , (Capp.b1No2D, [Leb.b1No2D3C, Leb.b1No2H3D])
           , (Capp.b1No2H, [Leb.b1No2H3C, Leb.b1No2H3D, Leb.b1No2H3S])
           , (Capp.b1No2S, [Leb.b1No2S3C, Leb.b1No2S3D, Leb.b1No2S3H])
           ]
        -- East should be an unpassed hand to interfere.
        <~ [T.North, T.South, T.West]
        <~ T.allVulnerabilities


signoff3 :: Situations
signoff3 = let
    sit (overcall, responses) dlr vul = let
        inner response = let
            action = do
                setOpener T.North
                Leb.b1N
                _ <-overcall
                withholdBid response
            responseDescription :: Description
            responseDescription =
                if (T.removeAlert . extractLastCall $ response) == T.Pass
                then "pass" .+ ""
                else "bid " .+ response
            explanation =
                "Partner opened a strong " .+ Leb.b1N .+ ", and RHO " .+
                "interfered with the auction. We're less-than-invitational, " .+
                "and just want to sign off in partscore. However, we can't " .+
                "do that at the 2 level any more. Make a lebensohl relay, " .+
                "planning to " .+ responseDescription .+ " afterwards."
          in situation "so3" action Leb.bLebensohl2N explanation dlr vul
      in return inner <~ responses
  in
    wrap . join $ return sit
        <~ [ (Nat.b1No2D,  [Leb.b1No2D2N3CP])
           , (Nat.b1No2H,  [Leb.b1No2H2N3CP, Leb.b1No2H2N3C3D])
           , (Nat.b1No2S,  [Leb.b1No2S2N3CP, Leb.b1No2S2N3C3D, Leb.b1No2S2N3C3H])
           , (DONT.b1No2D, [Leb.b1No2D2N3CP])
           -- Don't bid either major when the opponents have shown both
           , (DONT.b1No2H, [Leb.b1No2H2N3CP, Leb.b1No2H2N3C3D])
           , (DONT.b1No2S, [Leb.b1No2S2N3CP, Leb.b1No2S2N3C3D, Leb.b1No2S2N3C3H])
           , (MW.b1No2D,   [Leb.b1No2D2N3CP])
           , (MW.b1No2H,   [Leb.b1No2H2N3CP, Leb.b1No2H2N3C3D])
           , (MW.b1No2S,   [Leb.b1No2S2N3CP, Leb.b1No2S2N3C3D, Leb.b1No2S2N3C3H])
           -- Again, don't bid a major when RHO has them both. 3D should be
           -- natural and not a cue bid, because you'd never want to have a
           -- Stayman-like bid when RHO has shown both majors.
           , (Capp.b1No2D, [Leb.b1No2D2N3CP, Leb.b1No2H2N3C3D])
           , (Capp.b1No2H, [Leb.b1No2H2N3CP, Leb.b1No2H2N3C3D])
           , (Capp.b1No2S, [Leb.b1No2S2N3CP, Leb.b1No2S2N3C3D, Leb.b1No2S2N3C3H])
           ]
        -- East should be an unpassed hand to interfere.
        <~ [T.North, T.South, T.West]
        <~ T.allVulnerabilities


-- TODO:
-- jump to 3N
-- relay to 3N (answer should be 2N planning to rebid 3N)
-- cue bid for Stayman
-- relay to cue bid (answer should be 2N planning to rebid the cue)
-- complete the relay
-- pass after relay and signoff
-- pass or bid game after relay and invite


topic :: Topic
topic = makeTopic
    ("lebensohl after interference over our " .+ T.Bid 1 T.Notrump)
    "leb1N" situations
  where
    situations = wrap [ ignoreOpps
                      , signoff2
                      , signoff3
                      , gameForce
                      ]
