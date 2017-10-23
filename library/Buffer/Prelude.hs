module Buffer.Prelude
(
  module Exports,
  forMToZero_,
  forMFromZero_,
  strictCons,
  traceEventIO,
  traceEvent,
  traceMarkerIO,
  traceMarker,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, left, right, isLeft, isRight, (<>), First(..), Last(..), ProtocolError, traceEvent, traceEventIO, traceMarker, traceMarkerIO)

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- bug
-------------------------
import Bug as Exports

-- 
-------------------------

import qualified GHC.RTS.Flags as A
import qualified BasePrelude as B


-- * Workarounds for unremoved event logging
-------------------------

{-# NOINLINE matchTraceUserEvents #-}
matchTraceUserEvents :: a -> a -> a
matchTraceUserEvents =
  case A.user (unsafeDupablePerformIO A.getTraceFlags) of
    True -> \_ x -> x
    False -> \x _ -> x

{-# NOINLINE traceEventIO #-}
!traceEventIO =
  matchTraceUserEvents (const (return ())) B.traceEventIO

{-# NOINLINE traceEvent #-}
!traceEvent =
  matchTraceUserEvents (const id) B.traceEvent

{-# NOINLINE traceMarkerIO #-}
!traceMarkerIO =
  matchTraceUserEvents (const (return ())) B.traceMarkerIO

{-# NOINLINE traceMarker #-}
!traceMarker =
  matchTraceUserEvents (const id) B.traceMarker

{-# INLINE forMToZero_ #-}
forMToZero_ :: Applicative m => Int -> (Int -> m a) -> m ()
forMToZero_ !startN f =
  ($ pred startN) $ fix $ \loop !n -> if n >= 0 then f n *> loop (pred n) else pure ()

{-# INLINE forMFromZero_ #-}
forMFromZero_ :: Applicative m => Int -> (Int -> m a) -> m ()
forMFromZero_ !endN f =
  ($ 0) $ fix $ \loop !n -> if n < endN then f n *> loop (succ n) else pure ()

{-# INLINE strictCons #-}
strictCons :: a -> [a] -> [a]
strictCons !a b =
  let !c = a : b in c
