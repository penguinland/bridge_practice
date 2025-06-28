module DealerDefs(
  CondName
, CondDefn
, DealerDefs
, addDefinition
, toProgDefs
) where

import qualified Data.Map.Strict as Map
import Data.Maybe(fromMaybe)


-- TODO: Make these (String, Direction) and [String]. This is difficult at the
-- moment because `DealerProg.invert` prematurely casts some of these to string,
-- meaning the separate direction is lost within inversions before it is lost
-- elsewhere, and thus rotating the auction doesn't work properly. Better might
-- be to make a free monad with an interpreter at the end which casts it to a
-- dealer program?
type CondName = String
type CondDefn = String


-- The list is the reverse order in which the definitions were created. This
-- ordering is important because definitions must be added to the final Dealer
-- program so that new definitions depend on others above them but never below.
data DealerDefs = DealerDefs (Map.Map CondName CondDefn) [CondName]


instance Semigroup DealerDefs where
    definitionsA <> (DealerDefs defsB orderB) =
        foldl combine definitionsA (reverse orderB)
      where
        combine allDefs newKey =
          let
            newMaybeVal = Map.lookup newKey defsB
            newError = error "right DealerDefs has unknown definition!?"
            newVal = fromMaybe newError newMaybeVal
          in
            addDefinition newKey newVal allDefs


instance Monoid DealerDefs where
    mempty = DealerDefs Map.empty []


addDefinition :: CondName -> CondDefn -> DealerDefs -> DealerDefs
addDefinition name defn (DealerDefs defs order) =
  case Map.lookup name defs of
    Nothing    -> DealerDefs (Map.insert name defn defs) (name : order)
    Just defn' -> if defn == defn'
                  then DealerDefs defs order
                  else error $ "2 definitions for " ++ name ++ ": '" ++
                               defn ++ "' vs '" ++ defn' ++ "'"


toProgDefs :: DealerDefs -> [String]
toProgDefs (DealerDefs defs order) =
    map formatDefn (reverse order)
  where
    formatDefn name = case Map.lookup name defs of
        Nothing  -> error "cannot format nonexistent definition"
        Just def -> "    " ++ name ++ " = " ++ def
