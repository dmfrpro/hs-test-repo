module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет

infixl 6 |+|, |-|
infixl 7 |*|

(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (Variable name)
  | name == varName = replacement
  | otherwise = Variable name
replaceVar varName replacement (BinaryTerm op l r) = 
  BinaryTerm op (replaceVar varName replacement l) (replaceVar varName replacement r)
replaceVar _ _ term = term

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (IntConstant n) = IntConstant n
evaluate (Variable name) = Variable name
evaluate (BinaryTerm op l r) = 
  let l' = evaluate l
      r' = evaluate r
  in case (l', r') of
    (IntConstant a, IntConstant b) -> 
      IntConstant $ case op of
        Plus -> a + b
        Minus -> a - b
        Times -> a * b
    _ -> BinaryTerm op l' r'
