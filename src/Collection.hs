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
  , weightedList
) where

import Control.Monad.Trans.State.Strict(State, state)
import System.Random(StdGen, randomR)

import Random(pickItem)


data Collection a = CollectionRaw a
                  | CollectionList [Collection a]
                  | CollectionState (State StdGen (Collection a))
                  | CollectionWL (WeightedList a)


choose :: Collection a -> State StdGen a
choose (CollectionRaw a)                         = return a
choose (CollectionList aa)                       = pickItem aa >>= choose
choose (CollectionState s)                       = s >>= choose
choose (CollectionWL (WeightedList items total)) = let
    getItemAtWeight = getItemAtWeight' 0
      where
        getItemAtWeight' _ _ [] = error "got weight over max for WeightedList"
        getItemAtWeight' subtotal x ((w, v):rest)
            | x < subtotal + w = v
            | otherwise        = getItemAtWeight' (x + w) x rest
  in
    do randomWeight <- state $ randomR (0, total - 1)
       return $ getItemAtWeight randomWeight items


class Collectable r c where  -- c is a collection of raw r values
    collect :: c -> Collection r

instance Collectable r r where
    collect = CollectionRaw
instance (Collectable r c) => Collectable r [c] where
    collect = CollectionList . map collect
instance (Collectable r c) => Collectable r (State StdGen c) where
    collect = CollectionState . fmap collect
instance Collectable r (Collection r) where
    collect = id


-- The final Int is the total weight
data WeightedList a = WeightedList [(Int, a)] Int

weightedList :: [(Int, a)] -> WeightedList a
weightedList items = WeightedList items total
  where
    total = sum . map fst $ items
