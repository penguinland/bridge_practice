module DealerDefs(
  DealerDefs
, addDefinition
, toProgDefs
) where

import qualified Data.Map.Strict as Map
import Data.Maybe(fromMaybe)


-- TODO: Make these (String, Direction) and [String]. This is difficult at the
-- moment because `invert` prematurely casts some of these to string, meaning
-- the separate direction is lost within inversions before it is lost elsewhere,
-- and thus rotating the auction doesn't work properly. Better might be to make
-- a free monad with an interpreter at the end which casts it to a dealer
-- program?
type CondName = String
type CondDefn = String


-- The list is the reverse order in which the definitions were created.
data DealerDefs = DealerDefs (Map.Map CondName CondDefn) [CondName]


instance Semigroup DealerDefs where
    definitionsB <> (DealerDefs defsB orderB) =
        foldl combine definitionsB (reverse orderB)
      where
        combine allDefs newKey =
          let
            newMaybeVal = Map.lookup newKey defsB
            newError = error "right DealerDefs has unknown definition"
            newVal = fromMaybe newError newMaybeVal
          in
            addDefinition newKey newVal allDefs


instance Monoid DealerDefs where
    mempty = DealerDefs Map.empty []


addDefinition :: CondName -> CondDefn -> DealerDefs -> DealerDefs
addDefinition name defn (DealerDefs defs order) =
  case Map.lookup name defs of
    Nothing    -> DealerDefs (Map.insert name defn defs) (name : order)
    Just defn' -> if defn == defn' then DealerDefs defs order
                                   else error $ "2 defintions for " ++ name


toProgDefs :: DealerDefs -> [String]
toProgDefs (DealerDefs defs order) =
    map formatDefn (reverse order)
  where
    formatDefn name = case Map.lookup name defs of
        Nothing  -> error "cannot format nonexistent definition"
        Just def -> "    " ++ name ++ " = " ++ def
