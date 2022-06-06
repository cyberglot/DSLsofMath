module ComplexNumSemantics where

import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..),sum)
import Algebra
  (  Additive (zero, (+)),             AddGroup (negate), (-),  Ring
  ,  Multiplicative (one, (*)), (^+),  MulGroup ((/)),          Field
  )

newtype Complex r = C (r , r)    deriving Eq

type CS = Complex -- for shorter type expressions below
liftCS ::  (    r  ->     r  ->     r  ) ->
           (CS  r  -> CS  r  -> CS  r  )
liftCS (+) (C (x, y)) (C (x', y')) = C (x+x', y+y')

addC :: Additive r =>  Complex r -> Complex r -> Complex r
addC = liftCS (+)
toC :: Additive r => r -> Complex r
toC x = C (x, zero)

mulC :: Ring r =>  Complex r -> Complex r -> Complex r
mulC (C (ar, ai)) (C (br, bi))  =  C (ar*br - ai*bi, ar*bi + ai*br)

modulusSquaredC :: Ring r => Complex r -> r
modulusSquaredC (C (x, y)) = x^+2 + y^+2

scaleC :: Multiplicative r => r -> Complex r -> Complex r
scaleC a (C (x, y)) = C (a * x, a * y)

conj :: AddGroup r => Complex r -> Complex r
conj (C (x, y))   = C (x, negate y)

instance Additive r => Additive (Complex r) where
  (+)   = addC
  zero  = toC zero

instance AddGroup r => AddGroup (Complex r) where
  negate (C (a , b)) = C (negate a, negate b)

instance Ring r => Multiplicative (Complex r) where
  (*)   = mulC
  one   = toC one

instance Field r => MulGroup (Complex r) where
  (/)   = divC

divC :: Field a => Complex a -> Complex a -> Complex a
divC x y = scaleC (one/modSq) (x * conj y)
  where  modSq  =  modulusSquaredC y

re :: Complex r      ->  r
re z@(C (x , y))   =   x

im :: Complex r      ->  r
im z@(C (x , y))   =   y

instance Show r => Show (Complex r) where
  show = showCS

showCS :: Show r => Complex r -> String
showCS (C (x, y)) = show x ++ " + " ++ show y ++ "i"

data ComplexSy v r  =  Var v
                    |  FromCart r r
                    |  ComplexSy v r  :++  ComplexSy v r
                    |  ComplexSy v r  :**  ComplexSy v r
