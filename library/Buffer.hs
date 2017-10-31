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
data Buffer =
  {-|
  * Buffer pointer
  * Start offset
  * End offset
  * Capacity
  -}
  Buffer !(IORef (ForeignPtr Word8)) !(IORef Int) !(IORef Int) !(IORef Int)

{-|
Create a new buffer of the specified initial capacity.
-}
{-# INLINE new #-}
new :: Int -> IO Buffer
new capacity =
  do
    fptr <- mallocForeignPtrBytes capacity
    Buffer <$> newIORef fptr <*> newIORef 0 <*> newIORef 0 <*> newIORef capacity

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
push (Buffer fptrVar startVar endVar capacityVar) space ptrIO =
  {-# SCC "push" #-} 
  do
    fptr <- readIORef fptrVar
    end <- readIORef endVar
    capacity <- readIORef capacityVar
    let
      capacityDelta = end + space - capacity
      in 
        if capacityDelta <= 0 -- Doesn't need more space?
          then 
            do
              (!actualSpace, !output) <- withForeignPtr fptr $ \ptr -> ptrIO (plusPtr ptr end)
              writeIORef endVar $! end + actualSpace
              return output
          else 
            do
              start <- readIORef startVar
              let !occupiedSpace = end - start
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
                    writeIORef fptrVar newFPtr
                    writeIORef startVar 0
                    writeIORef endVar $! occupiedSpace + actualSpace
                    writeIORef capacityVar newCapacity
                    return output
                else 
                  -- Align
                  do
                    (!actualSpace, !output) <- withForeignPtr fptr $ \ptr -> do
                      memmove ptr (plusPtr ptr start) (fromIntegral occupiedSpace)
                      ptrIO (plusPtr ptr occupiedSpace)
                    writeIORef startVar 0
                    writeIORef endVar $! occupiedSpace + actualSpace
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
pull (Buffer fptrVar startVar endVar capacityVar) pulledAmount ptrIO refill =
  {-# SCC "pull" #-} 
  do
    start <- readIORef startVar
    end <- readIORef endVar
    let !newStart = start + pulledAmount
    if newStart > end
      then refill $! newStart - end
      else do
        fptr <- readIORef fptrVar
        !pulled <- withForeignPtr fptr $ \ptr -> ptrIO (plusPtr ptr start)
        writeIORef startVar newStart
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
getSpace (Buffer fptrVar startVar endVar capacityVar) =
  {-# SCC "getSpace" #-} 
  do
    end <- readIORef endVar
    start <- readIORef startVar
    return (end - start)

{-|
Create a bytestring representation without modifying the buffer.
-}
{-# INLINE getBytes #-}
getBytes :: Buffer -> IO ByteString
getBytes (Buffer fptrVar startVar endVar capacityVar) =
  do
    start <- readIORef startVar
    end <- readIORef endVar
    let size = end - start
    fptr <- readIORef fptrVar
    withForeignPtr fptr $ \ptr -> C.create size $ \destPtr -> C.memcpy destPtr ptr size
