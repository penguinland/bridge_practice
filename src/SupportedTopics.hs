module SupportedTopics(topicList) where

import Topic(Topic)

import qualified Topics.StandardOpeners as StandardOpeners
import qualified Topics.TwoOverOne as TwoOverOne
import qualified Topics.MajorSuitRaises as MajorSuitRaises
import qualified Topics.Overcalls as Overcalls
import qualified Topics.TakeoutDoubles as TakeoutDoubles
import qualified Topics.TransfersOver1MX as TransfersOver1MX
import qualified Topics.ForcingOneNotrump as ForcingOneNotrump
import qualified Topics.JacobyTransfers as JacobyTransfers
import qualified Topics.Stayman as Stayman
import qualified Topics.PuppetStayman as PuppetStayman
import qualified Topics.MuppetStayman as MuppetStayman
import qualified Topics.TexasTransfers as TexasTransfers
import qualified Topics.Smolen as Smolen
import qualified Topics.ThreeLevelResponsesTo1N as B1N3X
import qualified Topics.Meckwell as Meckwell
import qualified Topics.DONT as DONT
import qualified Topics.Cappelletti as Cappelletti
import qualified Topics.Woolsey as Woolsey
import qualified Topics.Lebensohl as Lebensohl
import qualified Topics.Jacoby2NT as Jacoby2NT
--import qualified Topics.RomanKeycardBlackwood as RKC

import qualified Topics.StandardModernPrecision.OpeningBids as SmpOpenings
import qualified Topics.StandardModernPrecision.OneClubResponses as Smp1CResponses
import qualified Topics.StandardModernPrecision.OneDiamondResponses as Smp1DResponses
import qualified Topics.StandardModernPrecision.Mafia as Mafia
import qualified Topics.StandardModernPrecision.MafiaResponses as MafiaResponses
--import qualified Topics.StandardModernPrecision.Lampe as Lampe
import qualified Topics.StandardModernPrecision.TwoDiamondOpeners as TwoDiamondOpeners
import qualified Topics.StandardModernPrecision.TripleFourOne as TripleFourOne

-- I don't think I ever finished making these topics...
--import qualified Topics.MinorTransfersScott as MinorTransfers


-- The list is the order to display topics on the website. The int is a unique
-- ID for the topic when requesting a situation. That way, you can add new
-- topics to the middle of the list without messing up anyone using the website.
-- The boolean is whether this topic should be selected by default. If topic
-- indices aren't unique, we're gonna have really subtle bugs that will be hard
-- to figure out, but we check for this in Assertions.hs.
topicList :: [(Int, Bool, Topic)]
topicList = [ (10, True,  StandardOpeners.topic)
            , (29, True,  TwoOverOne.topic)
            , (11, True,  MajorSuitRaises.topic)
            , (26, True,  Overcalls.topic)
            , (24, True,  TakeoutDoubles.topic)
            , (12, True,  ForcingOneNotrump.topic)
            , (13, True,  JacobyTransfers.topic)
            , (14, True,  Stayman.topic)
            , (15, False, TexasTransfers.topic)
            , (23, False, Smolen.topic)
            , (28, False, B1N3X.topic)
            , (22, False, PuppetStayman.topic)
            , (27, False, MuppetStayman.topic)
            , (16, False, Meckwell.topic)
            , (19, False, DONT.topic)
            , (18, False, Cappelletti.topic)
            , (21, False, Woolsey.topic)
            , (20, False, Lebensohl.topic)
            , (17, True,  Jacoby2NT.topic)
            --, (30, False, RKC.topic1430)
            --, (31, False, RKC.topic3014)
            , (25, False, TransfersOver1MX.topic)
            , (50, False, SmpOpenings.topic)
            , (51, False, Smp1CResponses.topic)
            , (52, False, Smp1CResponses.topicExtras)
            , (53, False, Mafia.topic)
            , (54, False, MafiaResponses.topic)
            , (57, False, TripleFourOne.topic)
            , (55, False, Smp1DResponses.topic)
            --, (70, False, Lampe.topic)
            , (56, False, TwoDiamondOpeners.topic)
            ]
