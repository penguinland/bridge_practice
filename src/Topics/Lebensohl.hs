module Topics.Lebensohl(topic) where

import qualified Bids.Lebensohl as B
import qualified Bids.Meckwell as MW
import qualified Bids.Cappelletti as Capp
import qualified Bids.DONT as DONT
import CommonBids(setOpener)
--import EDSL(makePass, pointRange, suitLength, maxSuitLength, forEach)
import Output((.+))
import Situation(situation, (<~))
import qualified Terminology as T
import Topic(Topic, wrap, wrapVulDlr, Situations, makeTopic)


ignoreOpps :: Situations
ignoreOpps = let
    sit (overcall, response, overcallIs2C) = let
        action = do
            setOpener T.North
            B.b1N
            _ <- overcall
        explanation =
            "Partner opened a strong " .+ B.b1N .+ ", and RHO interfered " .+
            "with the auction. However, their call didn't actually take up " .+
            "any of our bidding room. Ignore it, and use our usual systems " .+
            "over notrump" .+
            (if overcallIs2C then " (double of " .+ overcall .+ " is Stayman)"
                            else "") .+ "."
      in situation "ignr" action response explanation
  in
    wrapVulDlr $ return sit <~ [ (DONT.b1NoX,  B.b1NoX2C,  False)
                               , (DONT.b1NoX,  B.b1NoX2D,  False)
                               , (DONT.b1NoX,  B.b1NoX2H,  False)
                               , (MW.b1NoX,    B.b1NoX2C,  False)
                               , (MW.b1NoX,    B.b1NoX2D,  False)
                               , (MW.b1NoX,    B.b1NoX2H,  False)
                               , (Capp.b1NoX,  B.b1NoX2C,  False)
                               , (Capp.b1NoX,  B.b1NoX2D,  False)
                               , (Capp.b1NoX,  B.b1NoX2H,  False)
                               , (DONT.b1No2C, B.b1No2CX,  True)
                               , (DONT.b1No2C, B.b1No2C2D, True)
                               , (DONT.b1No2C, B.b1No2C2H, True)
                               , (MW.b1No2C,   B.b1No2CX,  True)
                               , (MW.b1No2C,   B.b1No2C2D, True)
                               , (MW.b1No2C,   B.b1No2C2H, True)
                               , (Capp.b1No2C, B.b1No2CX,  True)
                               , (Capp.b1No2C, B.b1No2C2D, True)
                               , (Capp.b1No2C, B.b1No2C2H, True)
                               ]


topic :: Topic
topic = makeTopic
    ("lebensohl after interference over our " .+ T.Bid 1 T.Notrump)
    "leb1N" situations
  where
    situations = wrap [ ignoreOpps
                      , ignoreOpps
                      ]
