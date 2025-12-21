{-# LANGUAGE InstanceSigs #-}
module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl (:<) REmpty

toListForShow :: ReverseList a -> [a]
toListForShow REmpty = []
toListForShow (xs :< x) = toListForShow xs ++ [x]

eqReverseList :: Eq a => ReverseList a -> ReverseList a -> Bool
eqReverseList REmpty REmpty = True
eqReverseList REmpty _ = False
eqReverseList _ REmpty = False
eqReverseList (xs :< x) (ys :< y) = x == y && eqReverseList xs ys

-- Реализуйте все представленные ниже классы (см. тесты)
-- Show instance
instance Show a => Show (ReverseList a) where
  showsPrec _ lst = showString "[" . showItems lst . showString "]"
    where
      showItems REmpty = id
      showItems (REmpty :< x) = shows x
      showItems (xs :< x) = showItems xs . showString "," . shows x
  
  show = show . toListForShow

instance Eq a => Eq (ReverseList a) where
  (==) = eqReverseList
  (/=) x y = not (x == y)

instance Semigroup (ReverseList a) where
  (<>) :: ReverseList a -> ReverseList a -> ReverseList a
  xs <> REmpty = xs
  xs <> (ys :< y) = (xs <> ys) :< y

instance Monoid (ReverseList a) where
  mempty = REmpty
  mappend = (<>)

instance Functor ReverseList where
  fmap :: (a -> b) -> ReverseList a -> ReverseList b
  fmap _ REmpty = REmpty
  fmap f (xs :< x) = fmap f xs :< f x

instance Applicative ReverseList where
  pure :: a -> ReverseList a
  pure x = REmpty :< x
  
  (<*>) :: ReverseList (a -> b) -> ReverseList a -> ReverseList b
  REmpty <*> _ = REmpty
  _ <*> REmpty = REmpty
  (fs :< f) <*> xs = (fs <*> xs) <> fmap f xs

instance Monad ReverseList where
  return = pure
  
  (>>=) :: ReverseList a -> (a -> ReverseList b) -> ReverseList b
  REmpty >>= _ = REmpty
  (xs :< x) >>= f = (xs >>= f) <> f x
