module Solutions.Chap1 where

import ComplexNum (ComplexSyn(..))
import ComplexNumSemantics (Complex(..))

-- Exercise 1
data Exp  =  Con Integer
          |  Plus   Exp  Exp
          |  Minus  Exp  Exp
          |  Times  Exp  Exp
  deriving (Eq, Show)

a :: Exp
a = Plus (Con 2)  (Con 2)

b :: Exp
b = Plus a (Times (Con 7) (Con 9))

c :: Exp
c = Minus lhs rhs
  where
    lhs = Times (Con 8) (Plus (Con 2) (Con 11))
    rhs = Times (Plus (Con 3) (Con 7)) (Plus a b)

eval :: Exp -> Integer
eval (Con i)     = i
eval (Plus i j)  = (eval i) + (eval j)
eval (Minus i j) = (eval i) - (eval j)
eval (Times i j) = (eval i) * (eval j)

data Exp'  =  Con'' Integer
           |  Var String
           |  Plus''   Exp'  Exp'
           |  Minus''  Exp'  Exp'
           |  Times'' Exp'  Exp'
  deriving (Eq, Show)

var :: String -> Integer
var "x" = 5
var "y" = 8
var "z" = 13
var _  = 0

eval' :: Exp' -> Integer
eval' (Var s) = var s
-- same as `eval`
eval' (Con'' i)     = i
eval' (Plus'' i j)  = (eval' i) + (eval' j)
eval' (Minus'' i j) = (eval' i) - (eval' j)
eval' (Times'' i j) = (eval' i) * (eval' j)

c1 :: Exp'
c1 = Times'' (Times'' lhs rhs) (Var "z")
  where
    lhs = Minus'' (Var "x") (Con'' 15)
    rhs = Plus''  (Var "y") (Con'' 12)

-- Exercise 2
data E2 a  =  Con' a
           |  Var' String
           |  Plus'   (E2 a)  (E2 a)
           |  Minus'  (E2 a)  (E2 a)
           |  Times'  (E2 a)  (E2 a)
  deriving(Eq, Show)

-- Exercise 11
embed :: Complex r -> ComplexSyn r
embed (C (x, y)) = ToComplexCart x y
