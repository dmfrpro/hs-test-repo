module Part1.Tasks where

import Util(notImplementedYet)
import Data.List

-- обрезание ответа до промежутка [-pi, pi]
reduceToPiPi :: Double -> Double
reduceToPiPi angle = angle - 2 * pi * fromIntegral (round (angle / (2 * pi)))

-- формула Тейлора с точностью до 10 членов
taylorSeries :: (Integer -> Double) -> Double
taylorSeries termFunc = foldr (+) 0 (take 10 [termFunc n | n <- [0..]])

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = taylorSeries (term (reduceToPiPi x))
  where
    term x n = fromIntegral ((-1) ^ n) * xPower / fromIntegral (factorial (2 * n + 1))
      where
        xPower = x ^ (fromIntegral (2 * n + 1))
        factorial m = product [1..m]

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = taylorSeries (term (reduceToPiPi x))
  where
    term x n = fromIntegral ((-1) ^ n) * xPower / fromIntegral (factorial (2 * n))
      where
        xPower = x ^ (fromIntegral (2 * n))
        factorial m = product [1..m]

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | month < 1 || month > 12 = False
  | day < 1 = False
  | month `elem` [1, 3, 5, 7, 8, 10, 12] = day <= 31
  | month `elem` [4, 6, 9, 11] = day <= 30
  | month == 2 = if isLeapYear year then day <= 29 else day <= 28
  | otherwise = False
  where
    isLeapYear y = (y `mod` 400 == 0) || (y `mod` 4 == 0 && y `mod` 100 /= 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow base 0 = 1
myPow base exp
  | even exp = let half = myPow base (exp `div` 2) in half * half
  | otherwise = base * myPow base (exp - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | n <= 3 = True
  | even n = False
  | otherwise = isPrime' 3
  where
    isPrime' d
      | d * d > n = True
      | n `mod` d == 0 = False
      | otherwise = isPrime' (d + 2)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea [] = 0
shapeArea points = abs sumArea / 2
  where
    pairs = zip points (tail points ++ [head points])
    reducer ((x1, y1), (x2, y2)) acc = (x1 * y2 - x2 * y1) + acc
    sumArea = foldr reducer 0 pairs

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | not isValid = -1
  | isRight = 2
  | isObtuse = 0
  | otherwise = 1
  where
    [x, y, z] = sort [a, b, c]

    isValid = x > 0 && y > 0 && z > 0 && x + y > z
    isRight = abs (z * z - (x * x + y * y)) < 1e-10
    isObtuse = z * z > x * x + y * y
