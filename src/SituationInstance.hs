module SituationInstance (
  SituationInstance(..)
, debugString
, instantiate
) where


import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict(state)
import Data.Aeson(ToJSON, toJSON)
import Data.Bifunctor(first)
import Data.List.Utils(join)
import Data.Map(fromList)
import System.Random(genWord64)

import DealerProg(eval)
import Output(Showable(..), Description)
import Situation(Situation(..))
import Structures(Bidding, Deal)
import Terminology(CompleteCall)
import Types(StIO)


data SituationInstance =
    SituationInstance Bidding CompleteCall Description Deal String


instance Showable SituationInstance where
    toLatex (SituationInstance bidding answer explanation deal debugStr) =
        "\\problem{%\n" ++
            join "%\n}{%\n" [ toLatex deal
                            , toLatex bidding
                            , toLatex answer
                            , toLatex explanation
                            , debugStr
                            ] ++
            "%\n}"
    toHtml _ = "todo"
    toMonospace (SituationInstance bidding call solution deal _) =
        unlines [ toMonospace deal
                , ""
                , toMonospace bidding
                , ""
                , "Answer: " ++ toMonospace call
                , toMonospace solution
                ]

instance ToJSON SituationInstance where
    toJSON (SituationInstance b c s d ds) = toJSON . fromList $
        [ ("bidding",      toJSON b)
        , ("answer",       toJSON . toHtml $ c)
        , ("explanation",  toJSON . toHtml $ s)
        , ("deal",         toJSON d)
        , ("debug_string", toJSON ds)
        ]


debugString :: SituationInstance -> String
debugString (SituationInstance _ _ _ _ ds) = ds


instantiate :: String -> Situation -> StIO (Maybe SituationInstance)
instantiate reference (Situation _ b dl c s dn v) = do
    n <- state (first fromIntegral . genWord64)
    maybeDeal <- lift $ eval reference dn v dl n
    return (SituationInstance b c s <$> maybeDeal <*> pure reference)
