{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Beamable.Integer
    ( beamInteger
    , unbeamInteger
    ) where

import Control.Arrow (first)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Blaze.ByteString.Builder
import qualified Data.ByteString as B
import GHC.Integer.GMP.Internals
import GHC.Base
import GHC.Word
import GHC.ST

import Data.Beamable.Int

beamInteger :: Integer -> Builder
beamInteger (S# x#)     = beamInt 0 <> beamInt (fromIntegral $ I# x#)
beamInteger i =
    beamInt (fromIntegral baSize) <> beamInt (fromIntegral $ I# x#) <>
        fromWord8s [W8# (indexWord8Array# ba# i#) | I# i# <- [0 .. baSize - 1]]
  where
    ba# = getBA# i
    x# = getTag# i
    baSize# = sizeofByteArray# ba#
    baSize  = I# baSize#
{-# INLINE beamInteger #-}

unbeamInteger :: ByteString -> (Integer, ByteString)
unbeamInteger bs
    | primTrue (baSize# ==# 0#) = (S# x#, bs'')
    | otherwise  = runSTRep $ \s# ->
        let (# s'#, mba# #)  = newByteArray# baSize# s#
            s''#             = go mba# 0# s'#
            (# s'''#, ba# #) = unsafeFreezeByteArray# mba# s''#

        in (# s'''#, (mkInteger# x# ba#, B.drop (I# baSize#) bs'') #)
  where
    !(I# baSize#, bs') = first fromIntegral $ unbeamInt bs
    !(I# x#, bs'')     = first fromIntegral $ unbeamInt bs'

    go mba# i# s#
        | primTrue (i# >=# baSize#) = s#
        | otherwise      =
            let !(W8# b#) = B.index bs'' (I# i#)
                s'#       = writeWord8Array# mba# i# b# s#
            in go mba# (i# +# 1#) s'#
{-# INLINE unbeamInteger #-}


#if MIN_VERSION_base(4,7,0)
-- could use isTrue#, but that will introduce extraneous error
-- checking that we don't need.
primTrue :: Int# -> Bool
primTrue x = tagToEnum# x
#else
primTrue :: Bool -> Bool
primTrue = id
#endif

#if MIN_VERSION_base(4,8,0)
getBA# :: Integer -> ByteArray#
getBA# (Jp# (BN# ba#)) = ba#
getBA# (Jn# (BN# ba#)) = ba#
getBA# _ = error "getBA#? Beamable Internal error"

getTag# :: Integer -> Int#
getTag# (Jp# _) = 0#
getTag# (Jn# _) = 1#
getTag# _ = error "getTag#? Beamable Internal error"

mkInteger# :: Int# -> ByteArray# -> Integer
mkInteger# 0# ba# = Jp# (BN# ba#)
mkInteger# 1# ba# = Jn# (BN# ba#)
mkInteger# _ _ = error "mkInteger#? Beamable Integer error"
#else
getBA# :: Integer -> ByteArray#
getBA# (J# _ ba#) = ba#
getBA# _ = error "getBA#? Beamable Internal error"

getTag# :: Integer -> Int#
getTag# (J# x# _) = x#
getTag# _ = error "getTag#? Beamable Internal error"

mkInteger# :: Int# -> ByteArray# -> Integer
mkInteger# x# ba# = J# x# ba#
#endif
