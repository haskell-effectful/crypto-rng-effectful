module Main where

import Control.Monad
import Crypto.RNG
import Data.ByteString
import Effectful
import Effectful.Reader.Static
import Test.Tasty
import Test.Tasty.HUnit

import Effectful.Crypto.RNG

main :: IO ()
main = do
  defaultMain $ testGroup "Crypto-RNG Bindings"
    [ testCase "Genering random bytes wrapped in a newtype" testRandomBytes
    , testCase "Generating a random number within a range from Reader" testRandomNumber
    ]

testRandomNumber :: Assertion
testRandomNumber = runEff $ do
  cryptoState <- newCryptoRNGState
  void . runCryptoRNG cryptoState
       . runReader ((10, 20) :: (Int, Int))
       $ generatingRandomNumber

generatingRandomNumber :: (RNG :> es, Reader (Int, Int) :> es) => Eff es Int
generatingRandomNumber = do
  bounds <- ask
  randomR bounds

---

testRandomBytes :: Assertion
testRandomBytes = runEff $ do
  cryptoState <- newCryptoRNGState
  void $ runCryptoRNG cryptoState generateUID

newtype UID = UID ByteString
  deriving newtype (Show, Eq, Ord)

generateUID :: RNG :> es => Eff es UID
generateUID = do
  bytes <- randomBytes 8
  pure $ UID bytes
