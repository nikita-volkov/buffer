module Buffer
(
  Buffer,
  new,
  -- * Pushing
  push,
  pushBytes,
  pushStorable,
  -- * Pulling
  pull,
  pullBytes,
  pullStorable,
  -- * Analysing
  getBytes,
  getSpace,
)
where

import Buffer.Prelude hiding (State, Buffer)
import Foreign.C
import qualified Data.ByteString.Internal as C



foreign import ccall unsafe "memmove"
  memmove :: Ptr a {-^ Destination -} -> Ptr a {-^ Source -} -> CSize {-^ Count -} -> IO (Ptr a) {-^ Destination -}

foreign import ccall unsafe "memcpy"
  memcpy :: Ptr a {-^ Destination -} -> Ptr a {-^ Source -} -> CSize {-^ Count -} -> IO (Ptr a) {-^ Destination -}


{-|
Mutable buffer.
-}
newtype Buffer =
  Buffer (IORef State)

data State =
  {-|
  * Buffer pointer
  * Start offset
  * End offset
  * Max amount
  -}
  State {-# UNPACK #-} !(ForeignPtr Word8) {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int

{-|
Create a new buffer of the specified initial capacity.
-}
{-# INLINE new #-}
new :: Int -> IO Buffer
new capacity =
  do
    fptr <- mallocForeignPtrBytes capacity
    stateIORef <- newIORef (State fptr 0 0 capacity)
    return (Buffer stateIORef)

{-|
Prepares the buffer to be filled with at maximum the specified amount of bytes,
then uses the pointer-action to populate it.
It is your responsibility to ensure that the action does not exceed the space limit.

The pointer-action returns the amount of bytes it actually writes to the buffer.
That amount then is used to move the buffer's cursor accordingly.
It can also produce some @result@, which will then be emitted by @push@.

It also aligns or grows the buffer if required.
-}
{-# INLINABLE push #-}
push :: Buffer -> Int -> (Ptr Word8 -> IO (Int, result)) -> IO result
push (Buffer stateIORef) space ptrIO =
  {-# SCC "push" #-} 
  do
    State fptr start end capacity <- readIORef stateIORef
    let
      !remainingSpace = capacity - end
      !capacityDelta = space - remainingSpace
      !occupiedSpace = end - start
      in 
        if capacityDelta <= 0 -- Doesn't need more space?
          then 
            do
              (!actualSpace, !output) <- withForeignPtr fptr $ \ptr -> ptrIO (plusPtr ptr end)
              writeIORef stateIORef $! State fptr start (end + actualSpace) capacity
              return output
          else 
            if capacityDelta > start -- Needs growing?
              then
                -- Grow
                do
                  let !newCapacity = occupiedSpace + space
                  newFPtr <- mallocForeignPtrBytes newCapacity
                  (!actualSpace, !output) <- withForeignPtr newFPtr $ \newPtr -> do
                    withForeignPtr fptr $ \ptr -> do
                      memcpy newPtr (plusPtr ptr start) (fromIntegral occupiedSpace)
                    ptrIO (plusPtr newPtr occupiedSpace)
                  let !newOccupiedSpace = occupiedSpace + actualSpace
                  writeIORef stateIORef $! State newFPtr 0 newOccupiedSpace newCapacity
                  return output
              else 
                -- Align
                do
                  (!actualSpace, !output) <- withForeignPtr fptr $ \ptr -> do
                    memmove ptr (plusPtr ptr start) (fromIntegral occupiedSpace)
                    ptrIO (plusPtr ptr occupiedSpace)
                  writeIORef stateIORef $! State fptr 0 (occupiedSpace + actualSpace) capacity
                  return output

{-|
Pulls the specified amount of bytes from the buffer using the provided pointer-action,
freeing the buffer from the pulled bytes afterwards.

In case the buffer does not contain enough bytes yet,
the second action is called instead, given the amount of required bytes missing.
You should use that action to refill the buffer accordingly and pull again.
-}
{-# INLINE pull #-}
pull :: Buffer -> Int -> (Ptr Word8 -> IO result) -> (Int -> IO result) -> IO result
pull (Buffer stateIORef) pulledAmount ptrIO refill =
  {-# SCC "pull" #-} 
  do
    State fptr start end capacity <- readIORef stateIORef
    let !newStart = start + pulledAmount
    if newStart > end
      then refill $! newStart - end
      else do
        !pulled <- withForeignPtr fptr $ \ptr -> ptrIO (plusPtr ptr start)
        writeIORef stateIORef $! State fptr newStart end capacity
        return pulled

{-|
Push a byte array into the buffer.
-}
{-# INLINE pushBytes #-}
pushBytes :: Buffer -> ByteString -> IO ()
pushBytes buffer (C.PS bytesFPtr offset length) =
  push buffer length $ \ptr ->
  withForeignPtr bytesFPtr $ \bytesPtr ->
  C.memcpy ptr (plusPtr bytesPtr offset) length $> (length, ())

{-|
Pulls the specified amount of bytes, converting them into @result@,
if the buffer contains that amount.

In case the buffer does not contain enough bytes yet,
the second action is called instead, given the amount of required bytes missing.
You should use that action to refill the buffer accordingly and pull again.
-}
{-# INLINE pullBytes #-}
pullBytes :: Buffer -> Int -> (ByteString -> result) -> (Int -> IO result) -> IO result
pullBytes buffer amount bytesResult =
  pull buffer amount (\ptr -> fmap bytesResult (C.create amount (\destPtr -> C.memcpy destPtr ptr amount)))

{-|
Push a storable value into the buffer.
-}
{-# INLINE pushStorable #-}
pushStorable :: (Storable storable) => Buffer -> storable -> IO ()
pushStorable buffer storable =
  push buffer amount (\ptr -> poke (castPtr ptr) storable $> (amount, ()))
  where
    amount = sizeOf storable

{-|
Pulls a storable value, converting it into @result@,
if the buffer contains enough bytes.

In case the buffer does not contain enough bytes yet,
the second action is called instead, given the amount of required bytes missing.
You should use that action to refill the buffer accordingly and pull again.
-}
{-# INLINE pullStorable #-}
pullStorable :: (Storable storable) => Buffer -> (storable -> result) -> (Int -> IO result) -> IO result
pullStorable buffer storableResult =
  pull buffer amount (\ptr -> fmap storableResult (peek (castPtr ptr)))
  where
    amount =
      sizeOf ((undefined :: (a -> b) -> a) storableResult)

{-|
Get how much space is occupied by the buffer's data.
-}
{-# INLINE getSpace #-}
getSpace :: Buffer -> IO Int
getSpace (Buffer stateIORef) =
  {-# SCC "getSpace" #-} 
  do
    State fptr start end capacity <- readIORef stateIORef
    return $! end - start

{-|
Create a bytestring representation without modifying the buffer.
-}
{-# INLINE getBytes #-}
getBytes :: Buffer -> IO ByteString
getBytes (Buffer stateIORef) =
  do
    State fptr start end capacity <- readIORef stateIORef
    let size = end - start
    withForeignPtr fptr $ \ptr -> C.create size $ \destPtr -> C.memcpy destPtr (plusPtr ptr start) size
