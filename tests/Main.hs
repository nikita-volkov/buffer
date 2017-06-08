module Main where

import Prelude
import Bug
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances
import qualified ByteRingBuffer as A
import qualified PtrMagic.Push as B
import qualified PtrMagic.Pull as C
import qualified Data.ByteString as D


main =
  defaultMain $
  testGroup "All tests"
  [
    testProperty "Concatenation of pushed bytestrings equals the byte representation" $ \(inputs :: [ByteString]) ->
      unsafePerformIO $ do
        buffer <- A.new (2 ^ 8)
        forM_ inputs $ \input -> A.push buffer (B.bytes (D.length input)) input
        let concattedInputs = mconcat inputs
        output <- A.pull buffer (C.bytes (D.length concattedInputs)) ($bug "") return
        return (concattedInputs === output)
    ,
    testProperty "Numbers" $ \(inputs :: [Word64]) ->
      unsafePerformIO $ do
        buffer <- A.new 2
        forM_ inputs $ \input -> A.push buffer B.beWord64 input
        outputs <- A.pull buffer (traverse (const C.beWord64) inputs) ($bug "") return
        return (inputs === outputs)
    ,
    testCase "Interleaving" $ do
      let (input1, input2, input3) = (1, 2, 3)
      buffer <- A.new 2
      A.push buffer B.beWord64 input1
      A.push buffer B.beWord64 input2
      output1 <- A.pull buffer C.beWord64 ($bug "") return
      A.push buffer B.beWord64 input3
      output2 <- A.pull buffer C.beWord64 ($bug "") return
      output3 <- A.pull buffer C.beWord64 ($bug "") return
      assertEqual (show output1) input1 output1
      assertEqual (show output2) input2 output2
      assertEqual (show output3) input3 output3
  ]

inspect :: A.Buffer -> IO ()
inspect buffer =
  print . D.unpack =<< A.getBytes buffer
