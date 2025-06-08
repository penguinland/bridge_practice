module ConstraintDefs(
  ConstraintDefs
, addConstraint
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


-- The list is the reverse order in which the constraints were defined.
data ConstraintDefs = ConstraintDefs (Map.Map CondName CondDefn) [CondName]


instance Semigroup ConstraintDefs where
    constraintDefsA <> (ConstraintDefs defsB orderB) =
        foldl combine constraintDefsA (reverse orderB)
      where
        combine allConstraints newKey =
          let
            newMaybeVal = Map.lookup newKey defsB
            newError = error "right ConstraintDefs has unknown constraint"
            newVal = fromMaybe newError newMaybeVal
          in
            addConstraint newKey newVal allConstraints


instance Monoid ConstraintDefs where
    mempty = ConstraintDefs Map.empty []


addConstraint :: CondName -> CondDefn -> ConstraintDefs -> ConstraintDefs
addConstraint name defn (ConstraintDefs defs order) =
  case Map.lookup name defs of
    Nothing    -> ConstraintDefs (Map.insert name defn defs) (name : order)
    Just defn' -> if defn == defn' then ConstraintDefs defs order
                                   else error $ "2 defintions for " ++ name


toProgDefs :: ConstraintDefs -> [String]
toProgDefs (ConstraintDefs defs order) =
    map formatDefn (reverse order)
  where
    formatDefn name = case Map.lookup name defs of
        Nothing  -> error "cannot format nonexistent constraint"
        Just def -> "    " ++ name ++ " = " ++ def
