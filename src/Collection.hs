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
) where

import Control.Monad.Trans.State.Strict(State)
import System.Random(StdGen)

import Random(pickItem)


data Collection a = CollectionRaw a
                  | CollectionList [Collection a]
                  | CollectionState (State StdGen (Collection a))


choose :: Collection a -> State StdGen a
choose (CollectionRaw a)   = return a
choose (CollectionList aa) = pickItem aa >>= choose
choose (CollectionState f) = f >>= choose


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
