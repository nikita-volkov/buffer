module ByteRingBuffer
(
  Buffer,
  new,
  push,
  pull,
  getBytes,
  getSpace,
)
where

import ByteRingBuffer.Prelude hiding (State, Buffer, push, pull)
import Foreign.C
import qualified PtrMagic.Pull as A
import qualified PtrMagic.Push as B
import qualified Data.ByteString.Internal as C



foreign import ccall unsafe "memmove"
  memmove :: Ptr a {-^ Destination -} -> Ptr a {-^ Source -} -> CSize {-^ Count -} -> IO (Ptr a) {-^ Destination -}

foreign import ccall unsafe "memcpy"
  memcpy :: Ptr a {-^ Destination -} -> Ptr a {-^ Source -} -> CSize {-^ Count -} -> IO (Ptr a) {-^ Destination -}


newtype Buffer =
  Buffer (IORef State)

data State =
  {-|
  * Buffer pointer
  * Start offset
  * End offset
  * Max amount
  -}
  State !(ForeignPtr Word8) !Int !Int !Int

new :: Int -> IO Buffer
new capacity =
  do
    fptr <- mallocForeignPtrBytes capacity
    stateIORef <- newIORef (State fptr 0 0 capacity)
    return (Buffer stateIORef)

{-|
Fill the buffer with the specified amount of bytes.

Aligns or grows the buffer if required.
-}
push :: Buffer -> B.Push pushed -> pushed -> IO ()
push (Buffer stateIORef) (B.Push space ptrIO) value =
  do
    State fptr start end capacity <- readIORef stateIORef
    let
      remainingSpace = capacity - end
      capacityDelta = space - remainingSpace
      occupiedSpace = end - start
      in 
        if capacityDelta <= 0 -- Doesn't need more space?
          then 
            do
              withForeignPtr fptr $ \ptr -> ptrIO (plusPtr ptr end) value
              writeIORef stateIORef (State fptr start (end + space) capacity)
          else 
            if capacityDelta > start -- Needs growing?
              then
                -- Grow
                do
                  let newCapacity = occupiedSpace + space
                  newFPtr <- mallocForeignPtrBytes newCapacity
                  withForeignPtr newFPtr $ \newPtr -> do
                    withForeignPtr fptr $ \ptr -> do
                      memcpy newPtr (plusPtr ptr start) (fromIntegral occupiedSpace)
                    ptrIO (plusPtr newPtr occupiedSpace) value
                  let newOccupiedSpace = occupiedSpace + space
                  writeIORef stateIORef (State newFPtr 0 newOccupiedSpace newCapacity)
              else 
                if occupiedSpace > 0 -- Needs aligning?
                  then
                    -- Align
                    do
                      withForeignPtr fptr $ \ptr -> do
                        memmove ptr (plusPtr ptr start) (fromIntegral occupiedSpace)
                        ptrIO (plusPtr ptr occupiedSpace) value
                      writeIORef stateIORef (State fptr 0 (occupiedSpace + space) capacity)
                  else
                    do
                      withForeignPtr fptr (\ptr -> ptrIO ptr value)
                      writeIORef stateIORef (State fptr 0 space capacity)

pull :: Buffer -> A.Pull pulled -> (Int -> IO interpreted) -> (pulled -> IO interpreted) -> IO interpreted
pull (Buffer stateIORef) (A.Pull pulledAmount ptrIO) refill succeed =
  do
    State fptr start end capacity <- readIORef stateIORef
    let newStart = start + pulledAmount
    if newStart > end
      then refill (newStart - end)
      else withForeignPtr fptr $ \ptr -> do
        pulled <- ptrIO (plusPtr ptr start)
        writeIORef stateIORef (State fptr newStart end capacity)
        succeed pulled

getSpace :: Buffer -> IO Int
getSpace (Buffer stateIORef) =
  do
    State fptr start end capacity <- readIORef stateIORef
    return (end - start)

{-|
Create a bytestring representation without modifying the buffer.
-}
getBytes :: Buffer -> IO ByteString
getBytes (Buffer stateIORef) =
  do
    State fptr start end capacity <- readIORef stateIORef
    let size = end - start
    withForeignPtr fptr $ \ptr -> C.create size $ \destPtr -> C.memcpy destPtr ptr size
