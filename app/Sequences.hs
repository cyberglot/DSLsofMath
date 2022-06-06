module Sequences where

import Numeric.Natural
import Data.Ratio

type Nat    =  Natural     -- imported from |Numeric.Natural|
type QQP    =  Ratio Nat   -- imported from |Data.Ratio|

type Seq a  = Nat -> a

idSeq :: Seq Nat
idSeq i = i                -- |{0, 1, 2, 3, ...}|

invSeq :: Seq QQP
invSeq i = 1 % (1 + i)       -- |{frac 1 1, frac 1 2, frac 1 3, frac 1 4, ...}|

pow2 :: Num r =>  Seq r
pow2 = (2^)        -- |{1, 2, 4, 8, ...}|

conSeq :: a -> Seq a
conSeq c i = c             -- |{c, c, c, c, ...}|

addSeq :: Num a => Seq a -> Seq a -> Seq a
addSeq f g i = f i + g i

liftSeq2 :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
liftSeq2 op f g i = op (f i) (g i)    -- |{op (f 0) (g 0), op (f 1) (g 1), ...}|

liftSeq1 :: (a -> b) -> Seq a -> Seq b
liftSeq1 h f i = h (f i)              -- |{h (f 0), h (f 1), h (f 2), ...}|

liftSeq0 :: a -> Seq a
liftSeq0 c i = c

sums :: Num a => Seq a -> Seq a
sums a 0 = 0
sums a i = sums a (i - 1) + a i

type Lim x = (Natural -> x) -> Maybe x
