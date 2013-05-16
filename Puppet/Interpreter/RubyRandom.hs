module Puppet.Interpreter.RubyRandom (rbGenrandInt32, randInit, limitedRand) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Bits
import Data.List (unfoldr,foldl')

data RandState = RandState { _array :: V.Vector Int
                           , _left  :: Int
                           , _initf :: Int
                           , _next  :: Int
                           } deriving (Show)

mixbits :: Int -> Int -> Int
mixbits u v = (u .&. 0x80000000) .|. (v .&. 0x7fffffff)

twist :: Int -> Int -> Int
twist u v = (mixbits u v `shiftR` 1) `xor` ma
    where
        ma = if (v .&. 1) == 1
                 then 0x9908b0df
                 else 0

valN :: Int
valN = 624
valM :: Int
valM = 397

initGenrand :: Integer -> RandState
initGenrand rseed = RandState (V.fromList (scanl genfunc seed [1..(valN - 1)])) 1 1 0
    where
        seed = fromIntegral rseed .&. 0xffffffff
        genfunc :: Int -> Int -> Int
        genfunc curval x = (1812433253 * (curval `xor` (curval `shiftR` 30)) + x) .&. 0xffffffff

nextState :: RandState -> RandState
nextState (RandState array _ initf _) = RandState narray valN 1 0
    where
        rarray = if initf == 0
                     then _array (initGenrand 5489)
                     else array
        narray = V.modify (\v -> twist1 v >> twist2 v >> final v) rarray
        twist1 v = mapM_ (twist' valM v) [0..(valN - valM - 1)]
        twist2 v = mapM_ (twist' (valM - valN) v) [(valN - valM) .. (valN - 2)]
        final v = do
            a <- VM.read v (valN - 1)
            b <- VM.read v 0
            pm <- VM.read v (valM - 1)
            let res = pm `xor` twist a b
            VM.write v (valN - 1) res
        twist' idx v n = do
            a <- VM.read v n
            b <- VM.read v (n+1)
            pm <- VM.read v (idx + n)
            let res = pm `xor` twist a b
            VM.write v n res

-- needs refactoring, too tedious for me
initGenrandBigint :: Integer -> RandState
initGenrandBigint seed =
    let intarray = unfoldr reduce seed
        reduce :: Integer -> Maybe (Integer, Integer)
        reduce 0 = Nothing
        reduce x = Just (x .&. 0xffffffff, x `shiftR` 32)
        initstate = _array (initGenrand 19650218)
        keylist = concat (repeat intarray)
        jlist = concat (repeat [0..(length intarray - 1)])
        kmax = max (length intarray) valN
        state1 = foldl' apply1 initstate (zip3 keylist jlist [1..kmax])
        apply1 :: V.Vector Int -> (Integer, Int, Int) -> V.Vector Int
        apply1 ra (initKey, j, ri) =
            let (a, i, sti, stim) = rollover ra ri
                nsti = ((sti `xor` ((stim `xor` (stim `shiftR` 30)) * 1664525)) + fromIntegral initKey + j) .&. 0xffffffff
            in  a V.// [(i,nsti)]
        state2 = foldl' apply2 state1 [2..valN]
        rollover :: V.Vector Int -> Int -> (V.Vector Int, Int, Int, Int)
        rollover ra ri =
            let (a,i) = if ri >= valN
                            then (ra V.// [(0, ra V.! (valN-1))],1)
                            else (ra,ri)
            in  (a,i,a V.! i, a V.! (i-1))
        apply2 :: V.Vector Int -> Int -> V.Vector Int
        apply2 ra ri =
            let (a, i, sti, stim) = rollover ra ri
                nsti = ((sti `xor` ((stim `xor` (stim `shiftR` 30)) * 1566083941)) - i) .&. 0xffffffff
            in  a V.// [(i,nsti)]
    in  RandState (state2 V.// [(0,0x80000000)]) 1 1 0


randInit :: Integer -> RandState
randInit x = if x <= 0xffffffff
                 then initGenrand x
                 else initGenrandBigint x

rbGenrandInt32 :: RandState -> (Int, RandState)
rbGenrandInt32 st =
    let rst = if _left st == 1
                  then nextState st
                  else st { _left = _left st - 1 }
        next = _next rst
        cv = _array rst V.! next
        nst = rst { _next = next + 1 }
        y1 = cv `xor` (cv `shiftR` 11)
        y2 = y1 `xor` ((y1 `shiftL` 7) .&. 0x9d2c5680)
        y3 = y2 `xor` ((y2 `shiftL` 15) .&. 0xefc60000)
        y4 = y3 `xor` (y3 `shiftR` 18)
    in (y4,nst)

limitedRand :: RandState -> Int -> (Int, RandState)
limitedRand s n = limitedRand' s
    where
        mask = foldl' (\x pow -> x .|. (x `shiftR` pow)) (n - 1) [1,2,4,8,16,32]
        limitedRand' s' =
            let (rval, ns) = rbGenrandInt32 s'
                val = rval .&. mask
            in  if n < val
                    then limitedRand' ns
                    else (val, ns)
