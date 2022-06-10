module Exercises.Chap1 where

import ComplexNum
import ComplexNumSemantics

-- Exercise 1
data Exp  =  Con Integer
          |  Plus   Exp  Exp
          |  Minus  Exp  Exp
          |  Times  Exp  Exp
  deriving (Eq, Show)


-- Exercise 2
data E2 a  =  Con' a
           |  Var String
           |  Plus'   (E2 a)  (E2 a)
           |  Minus'  (E2 a)  (E2 a)
           |  Times'  (E2 a)  (E2 a)
  deriving(Eq, Show)

-- Exercise 11
embed :: Complex r -> ComplexSyn r
embed (C (x, y)) = ToComplexCart x y
