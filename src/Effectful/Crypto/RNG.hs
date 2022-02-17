{-# LANGUAGE TypeFamilies #-}
{-|
  Module      : Effectful.Crypto.RNG
  Copyright   : © Hécate Moonlight, 2021
                  Dominik Peteler, 2021
  License     : MIT
  Maintainer  : hecate@glitchbra.in
  Stability   : stable

  An effect wrapper around Crypto.RNG for the Effectful ecosystem
-}
module Effectful.Crypto.RNG
  ( -- * CryptoRNG Effect
    CryptoRNG(..)

    -- * Runner
  , runCryptoRNG

    -- * CryptorRNG functions
  , CryptoRNGState
  , randomString
  , randomBytes
  , randomR
  , newCryptoRNGState
  , unsafeCryptoRNGState

    -- * Re-exports from Crypto.RNG
  , C.mapCryptoRNGT
  , C.runCryptoRNGT
  , C.withCryptoRNGState
  ) where

import Crypto.Classes (ByteLength)
import Crypto.RNG (CryptoRNGState)
import Data.ByteString (ByteString)
import Effectful.Internal.Effect
import Effectful.Internal.Monad
import qualified Crypto.RNG as C
import qualified Crypto.RNG.Utils as C
import Control.Monad.IO.Class
import Effectful.Dispatch.Dynamic
import System.Random (UniformRange)

  -- CryptoRNG :: CryptoRNGState -> CryptoRNG m a

-- | An effect for the cryptographic random generator provided by the DRBG package.
--
-- @since 0.0.1.0
data CryptoRNG :: Effect where
  RandomBytes :: ByteLength -> CryptoRNG m ByteString
  RandomString :: ByteLength -> String -> CryptoRNG m String
  RandomR :: (UniformRange a) => (a, a) -> CryptoRNG m a

-- | @since 0.0.1.0
type instance DispatchOf CryptoRNG = 'Dynamic

-- | The default Effect handler
--
--  @since 0.0.1.0
runCryptoRNG :: forall (es :: [Effect]) (a :: Type)
                . (IOE :> es)
                => CryptoRNGState
                -> Eff (CryptoRNG : es) a
                -> Eff es a
runCryptoRNG rngState = interpret $ \_ -> \case
  RandomBytes n -> liftIO $ C.randomBytesIO n rngState
  RandomString len allowedChars -> C.runCryptoRNGT rngState (C.randomString len allowedChars)
  RandomR (low, high) -> C.runCryptoRNGT rngState $ C.randomR (low, high)

-- | Create a new 'CryptoRNGState', based on system entropy.
--
--  @since 0.0.1.0
newCryptoRNGState :: IOE :> es => Eff es CryptoRNGState
newCryptoRNGState = C.newCryptoRNGState

-- | Create a new 'CryptoRNGState', based on a bytestring seed.
-- Should only be used for testing.
--
--  @since 0.0.1.0
unsafeCryptoRNGState :: IOE :> es => [ByteString] -> Eff es CryptoRNGState
unsafeCryptoRNGState = C.unsafeCryptoRNGState

-- | Generate given number of cryptographically secure random bytes.
--
--  @since 0.0.1.0
randomBytes :: (CryptoRNG :> es) => ByteLength -> Eff es ByteString
randomBytes len = send $ RandomBytes len

-- | Generate random string of specified length that contains allowed chars.
--
--  @since 0.0.1.0
randomString :: (CryptoRNG :> es) => Int -> String -> Eff es String
randomString len allowedChars = send $ RandomString len allowedChars

-- | Generate a cryptographically secure random number in given,
-- closed range.
--
--  @since 0.0.1.0
randomR :: (CryptoRNG :> es, UniformRange a) => (a, a) -> Eff es a
randomR = send . RandomR
