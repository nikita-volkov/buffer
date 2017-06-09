module Main where

import Prelude
import Bug
import Criterion.Main
import qualified ByteRingBuffer as A
import qualified PtrMagic.Pull as B
import qualified PtrMagic.Push as C
import qualified Data.ByteString as D


main =
  defaultMain =<< sequence
  [
    benchPush "1" (2^8) 1
    ,
    benchPush "10" (2^8) 10
    ,
    benchPush "100" (2^8) 100
  ]

benchOnBuffer name size io =
  do
    buffer <- A.new size
    return (bench name (whnfIO (io buffer)))

benchPush name bufferSize factor =
  benchOnBuffer name bufferSize $ 
  let !bytes = fromString (replicate 10000 'a')
      !length = D.length bytes
    in \buffer -> replicateM factor $ A.push buffer (C.bytes length) bytes
