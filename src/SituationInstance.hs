module SituationInstance (
  SituationInstance(..)
, instantiate
) where


import Control.Monad.Trans.State.Strict(State, state)
import Data.Aeson(ToJSON, toJSON)
import Data.Bifunctor(first)
import Data.List.Utils(join)
import Data.Map(fromList)
import System.Random(StdGen, genWord64)

import DealerProg(eval)
import Output(Showable(..), Commentary)
import Situation(Situation(..))
import Structures(Bidding, Deal)
import Terminology(CompleteCall)


data SituationInstance =
    SituationInstance Bidding CompleteCall Commentary Deal String


instance Showable SituationInstance where
    toLatex (SituationInstance b c s d ds) =
        "\\problem{%\n" ++
            join "%\n}{%\n" [toLatex d, toLatex b, toLatex c, toLatex s, ds] ++
            "%\n}"
    toHtml _ = "todo"

instance ToJSON SituationInstance where
    toJSON (SituationInstance b c s d ds) = toJSON . fromList $
        [ ("bidding",      toJSON b)
        , ("answer",       toJSON . toHtml $ c)
        , ("explanation",  toJSON . toHtml $ s)
        , ("deal",         toJSON d)
        , ("debug_string", toJSON ds)
        ]


-- TODO: Find a way to make this cleaner. Monad transformers might be a relevant
-- thing here?
instantiate :: String -> Situation ->
        State StdGen (IO (Maybe SituationInstance))
instantiate reference (Situation _ b dl c s v dn) = do
    n <- state (first fromIntegral . genWord64)
    let instantiate' :: IO (Maybe SituationInstance)
        instantiate' = do
            maybeDeal <- eval dn v dl n
            -- This do notation takes care of the IO monad, and the binds take
            -- care of the Maybe monad.
            return (SituationInstance b c s <$> maybeDeal <*> Just reference)
    return instantiate'
