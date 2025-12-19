-- In order to add the State monad to the Collectable typeclass, we need this
-- pragma. State is an alias for a trivial version of a more complicated monad,
-- and the default compiler doesn't let that be in a type class because not all
-- its arguments are type variables.
{-# LANGUAGE FlexibleInstances #-}
-- The Collectable typeclass takes 2 type arguments: the thing being collected
-- and the thing doing the collecting. Consequently, we need to allow multiple
-- parameters in typeclasses.
{-# LANGUAGE MultiParamTypeClasses #-}


module Collection(
    Collection
  , choose
  , Collectable(..)
  , WeightedList
  , weightedList
  , survey
) where

import Control.Monad.Trans.State.Strict(State, state, runState)
import Data.Tuple.Extra(second)
import System.Random(StdGen, randomR, mkStdGen)

import Random(pickItem)


data Collection a = CollectionRaw a
                  | CollectionList [Collection a]
                  | CollectionState (State StdGen (Collection a))
                  | CollectionWL (WeightedList (Collection a))


choose :: Collection a -> State StdGen a
choose (CollectionRaw a)                         = return a
choose (CollectionList aa)                       = pickItem aa >>= choose
choose (CollectionState s)                       = s >>= choose
choose (CollectionWL (WeightedList items total)) = let
    getItemAtWeight        _            []      _ = error "weight over maximum"
    getItemAtWeight subtotal ((w, v):rest) target
      | target < subtotal + w = v
      | otherwise             = getItemAtWeight (subtotal + w) rest target
  in
    do randomWeight <- state $ randomR (0, total - 1)
       choose $ getItemAtWeight 0 items randomWeight


class Collectable r c where  -- c is a collection of raw r values
    collect :: c -> Collection r

instance Collectable r r where
    collect = CollectionRaw

instance (Collectable r c) => Collectable r [c] where
    collect = CollectionList . map collect

instance (Collectable r c) => Collectable r (State StdGen c) where
    collect = CollectionState . fmap collect

instance (Collectable r c) => Collectable r (WeightedList c) where
    collect (WeightedList items total) =
        CollectionWL (WeightedList (map (second collect) items) total)

instance Collectable r (Collection r) where
    collect = id


-- The final Int is the total weight
data WeightedList a = WeightedList [(Int, a)] Int


weightedList :: [(Int, a)] -> WeightedList a
weightedList items = WeightedList items total
  where
    total = sum . map fst $ items


-- This is used during compile-time assertions to ensure that every Situation
-- within a Topic has a unique debug string.
survey :: (r -> a) -> Collection r -> [a]
survey f (CollectionRaw val) = [f val]
survey f (CollectionList l) = concatMap (survey f) l
-- We assume that all values you could generate from a CollectionState have the
-- same value within. If this changes, revisit this.
survey f (CollectionState s) = survey f . fst . flip runState (mkStdGen 0) $ s
survey f (CollectionWL (WeightedList wl _)) = concatMap (survey f . snd) wl
