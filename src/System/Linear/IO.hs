{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module defines a resource-safe linear IO monad. It provide facilities
-- to add in your own resources.
--
-- Functions in this module are meant to be qualified
--
-- @
--      import qualified System.Linear.IO as Resource
-- @

-- XXX: This would be better as a multiplicity-parametric relative monad, but
-- until we have multiplicity polymorphism, we use a linear monad.

module System.Linear.IO where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Linear.Prelude hiding (IO)
import qualified System.Linear.Naked.IO as Naked

newtype IO a = IO (ReleaseMap -> Naked.IO (a, Unrestricted ReleaseMap))

type ReleaseMap = Map Int (Naked.IO ())

-- * Creating new resources

-- | The type of resources. Each safe resource is implemented as an abstract
-- newtype wrapper around @Resource R@ where @R@ is the unsafe variant of the
-- resource.
data UnsafeResource a where
  UnsafeResource :: Int -> a -> Resource a
  -- Note that both components are unrestricted.

unsafeRelease :: Resource a -> IO ()
unsafeRelease (Resource key _) = IO $ \ releaseMap -> releaseWith key releaseMap
  where
    releaseWith key releaseMap = do
        releaser
        Naked.return ((), Unrestricted nextMap)
      where
        Naked.Builder {..} = Naked.builder -- used in the do-notation
        releaser = releaseMap Map.! key
        nextMap = Map.delete key releaseMap
