module ComplexNum where

import Prelude hiding (Real)
import ComplexNumSemantics (Complex(C), addC, mulC)
import Algebra (Ring)

data ImagUnits = I

i :: ImagUnits
i = I

instance Show ImagUnits where
  show = showIU

showIU ::  ImagUnits       ->  String
showIU     I               =   "i"

-- apparent non-commutativity
data ComplexA  =  CPlus1 Real Real ImagUnits  -- the form |a + bi|
               |  CPlus2 Real ImagUnits Real  -- the form |a + ib|

instance Show ComplexA where
  show = showCA

showCA ::  ComplexA       ->  String
showCA     (CPlus1 x y i)  =  show x ++ " + " ++ show y ++ showIU i
showCA     (CPlus2 x i y)  =  show x ++ " + " ++ showIU i ++ show y

type Real = Double

testC1 :: [ComplexA]
testC1 =  [  CPlus1 3 2 I  ,    CPlus1 (7/2) (-2/3) I
          ,  CPlus2 0 I pi ,    CPlus1 (-3) 0 I
          ]
testS1 = map showCA testC1

toComplex :: Real -> ComplexA
toComplex x = CPlus1 x 0 I

data ComplexB = CPlusB Real Real ImagUnits

data ComplexC = CPlusC Real Real

instance Eq ComplexC where
    CPlusC a b == CPlusC x y = a == x && b == y

newtype ComplexD = CD (Real, Real)
  deriving Eq

re :: ComplexD        ->  Real
re z@(CD (x , y))   =   x

im :: ComplexD        ->  Real
im z@(CD (x , y))   =   y

addD :: ComplexD -> ComplexD -> ComplexD
addD (CD (a , b)) (CD (x , y))  =  CD ((a + x) , (b + y))

evalE :: ComplexE -> ComplexD

data ComplexE  =  I2
               |  ToComplex Real
               |  Add   ComplexE  ComplexE
               |  Mul  ComplexE  ComplexE
 deriving (Eq, Show)

testE1 = Mul I2 I2
testE2 = Add (ToComplex 3) (Mul (ToComplex 2) I2)

evalE I2              = iD
evalE (ToComplex r)   = toComplexD r
evalE (Add  c1 c2)    = addD  (evalE c1)  (evalE c2)
evalE (Mul  c1 c2)    = mulD  (evalE c1)  (evalE c2)

iD            = CD (0 ,  1)
toComplexD r  = CD (r ,  0)

fromCD :: ComplexD -> ComplexE
fromCD (CD (x , y)) = Add (ToComplex x) (Mul (ToComplex y) I2)

propI2 :: Bool
propI2 =  Mul I2 I2 === ToComplex (-1)

(.=.) :: ComplexE -> ComplexE -> Bool
z .=. w  = evalE z == evalE w

infix 0 ===
class SemEq a where
  (===) :: a -> a -> Bool
instance SemEq Int where
  (===) = (==)
instance SemEq Double where
  (===) = (==)
instance SemEq ComplexE where
  (===) = (.=.)

propFromCD :: ComplexD -> Bool
propFromCD s =  evalE (fromCD s) == s

propAssocAdd    :: (Num a, SemEq a) => a -> a -> a -> Bool
propDistMulAdd  :: (Num a, SemEq a) => a -> a -> a -> Bool

propCommAdd     x y                = {-"\quad"-}  x + y          ===  y + x
propCommMul     x y                = {-"\quad"-}  x * y          ===  y * x
propAssocAdd    x y z              = {-"\quad"-}  (x + y) + z    ===  x + (y + z)
propAssocMul    x y z              =              (x * y) * z    ===  x * (y * z)
propDistMulAdd  x y z {-"\quad"-}  =              x * (y + z)    ===  (x * y) + (x * z)

propAssocInt     = propAssocAdd ::  Int     -> Int     -> Int     -> Bool
propAssocDouble  = propAssocAdd ::  Double  -> Double  -> Double  -> Bool

notAssocEvidence :: (Double , Double , Double , Bool)
notAssocEvidence = (lhs , rhs , lhs-rhs , lhs==rhs)
  where  lhs = (1+1)+1/3
         rhs =  1+(1+1/3)

data ComplexSyn r  =  ToComplexCart r r
                   |  ComplexSyn r  :+:  ComplexSyn r
                   |  ComplexSyn r  :*:  ComplexSyn r

toComplexSyn :: Num a => a -> ComplexSyn a
toComplexSyn x = ToComplexCart x 0

evalCSyn :: Ring r => ComplexSyn r -> Complex r
evalCSyn (ToComplexCart x y)  = C (x , y)
evalCSyn (l  :+:  r)          = addC  (evalCSyn l)  (evalCSyn r)
evalCSyn (l  :*:  r)          = mulC  (evalCSyn l)  (evalCSyn r)

negateCS :: Num a => ComplexSyn a -> ComplexSyn a
negateCS = ((-1) :*:)
absCS = error "absCS: missing constructor"
signumCS = error "signumCS: missing constructor"
instance Num a => Num (ComplexSyn a) where
   (+)  = (:+:)
   (*)  = (:*:)
   fromInteger = fromIntegerCS
   negate = negateCS
   abs = absCS
   signum = signumCS
fromIntegerCS :: Num r =>  Integer -> ComplexSyn r
fromIntegerCS = toComplexSyn . fromInteger

propAssoc :: SemEq a => (a -> a -> a) -> a -> a -> a -> Bool
propAssoc (⊛) x y z =  (x ⊛ y) ⊛ z === x ⊛ (y ⊛ z)

-- mulD :: ComplexD -> ComplexD -> ComplexD
mulD (CD (ar, ai)) (CD (br, bi)) = CD (ar*br - ai*bi, ar*bi + ai*br)

instance Show ComplexD where
  show = showCD

showCD :: ComplexD -> String
showCD (CD (x, y)) = show x ++ " + " ++ show y ++ "i"

propAssocAdd2 :: (SemEq a, Num a) => a -> a -> a -> Bool
propAssocAdd2 = propAssoc (+)
