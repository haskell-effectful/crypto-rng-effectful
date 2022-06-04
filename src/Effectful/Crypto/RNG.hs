-- | Generation of random numbers via "Crypto.RNG".
module Effectful.Crypto.RNG
  ( -- * Effect
    RNG(..)
  , CryptoRNG(..)

    -- ** Handlers
  , runCryptoRNG

    -- * Instantiation of the initial RNG state
  , CryptoRNGState
  , newCryptoRNGState
  , newCryptoRNGStateSized
  ) where

import Control.Monad.IO.Class
import Crypto.RNG
import Effectful
import Effectful.Dispatch.Dynamic
import qualified System.Random.Stateful as R

import Effectful.Crypto.RNG.Effect

-- | Generate cryptographically secure random numbers.
runCryptoRNG :: IOE :> es => CryptoRNGState -> Eff (RNG : es) a -> Eff es a
runCryptoRNG rng = interpret $ \_ -> \case
  RandomBytes n  -> liftIO $ randomBytesIO n rng
  Random         -> liftIO $ R.uniformM rng
  RandomR bounds -> liftIO $ R.uniformRM bounds rng
