{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Effectful.Crypto.RNG.Effect where

import Data.ByteString (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic
import System.Random
import Crypto.RNG.Class

-- | Provide the ability to generate random numbers.
data RNG :: Effect where
  RandomBytes :: Int -> RNG m ByteString
  Random      :: Uniform a => RNG m a
  RandomR     :: UniformRange a => (a, a) -> RNG m a

type instance DispatchOf RNG = 'Dynamic

instance RNG :> es => CryptoRNG (Eff es) where
  randomBytes = send . RandomBytes
  random      = send Random
  randomR     = send . RandomR
