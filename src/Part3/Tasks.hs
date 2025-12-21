module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f = iterate f

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq nums =
  let
    digitsOf n
      | n == 0 = [0]
      | otherwise = digitsOf' (abs n)
      where
        digitsOf' 0 = []
        digitsOf' x = digitsOf' (x `div` 10) ++ [x `mod` 10]

    allDigits = [digit | num <- nums, digit <- digitsOf num]

    frequencyMap = [(d, count d allDigits) | d <- [0..9]]
      where
        count digit list = length (filter (== digit) list)

    maximumBy cmp (x:xs) = foldl maxBy x xs
      where
        maxBy x y = case cmp x y of
            GT -> x
            _ -> y

    compareCount (_, c1) (_, c2) = compare c1 c2

    (maxDigit, _) = maximumBy compareCount frequencyMap

  in maxDigit

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f = foldr insertByKey []
  where
    insertByKey x [] = [(f x, [x])]
    insertByKey x ((k, xs):rest)
      | f x == k = (k, x:xs) : rest
      | otherwise = (k, xs) : insertByKey x rest
