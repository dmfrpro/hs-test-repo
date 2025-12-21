{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map (Map, empty, insert, lookup, keys, elems, fromList, toList)
import Data.List (transpose, foldl')

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
  sparseMatrixWidth :: Int,
  sparseMatrixHeight :: Int,
  sparseMatrixElements :: Map (Int, Int) a
} deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
  getElem :: mx -> Int -> Int -> Int
  getRows :: mx -> Int
  getCols :: mx -> Int
  fromList2D :: [[Int]] -> mx
  transposeM :: mx -> mx
  getRow :: mx -> Int -> [Int]
  getCol :: mx -> Int -> [Int]

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
instance Matrix Int where
  getElem x 0 0 = x
  getElem _ _ _ = 0
  
  getRows _ = 1
  getCols _ = 1
  
  fromList2D [[x]] = x
  fromList2D _ = error "Int matrix must be 1x1"
  
  transposeM x = x
  
  getRow x 0 = [x]
  getRow _ _ = []
  
  getCol x 0 = [x]
  getCol _ _ = []

--  * списка списков чисел
instance Matrix [[Int]] where
  getElem m i j = (m !! i) !! j
  
  getRows = length
  
  getCols m 
    | null m = 0
    | otherwise = length (head m)
  
  fromList2D = id
  
  transposeM = transpose
  
  getRow = (!!)
  
  getCol m j = map (!! j) m

--  * типа SparseMatrix, представленного выше
instance Matrix (SparseMatrix Int) where
  getElem matrix i j
    | i < 0 || i >= rows = 0
    | j < 0 || j >= cols = 0
    | otherwise = case Data.Map.lookup (i, j) elements of
      Just x -> x
      Nothing -> 0
    where
      rows = getRows matrix
      cols = getCols matrix
      elements = sparseMatrixElements matrix
  
  getRows (SparseMatrix _ height _) = height
  
  getCols (SparseMatrix width _ _) = width
  
  fromList2D rows = SparseMatrix width height sparseMap
    where
      height = length rows
      width = if height == 0 then 0 else length (head rows)
      indexedRows = zip [0..] rows
      sparseMap = fromList nonZeroElements
      nonZeroElements = concatMap processRow indexedRows
      processRow (i, row) = 
        [((i, j), value) | (j, value) <- zip [0..] row, value /= 0]
  
  transposeM (SparseMatrix width height elements) = 
    SparseMatrix height width transposedElements
    where
      transposedElements = fromList $
        [((j, i), value) | ((i, j), value) <- toList elements]
  
  getRow matrix rowIndex = 
    [getElem matrix rowIndex j | j <- [0..cols - 1]]
    where cols = getCols matrix
  
  getCol matrix colIndex = 
    [getElem matrix i colIndex | i <- [0..rows - 1]]
    where rows = getRows matrix

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye size = fromList2D $
  [[if i == j then 1 else 0 | j <- [0..size-1]] | i <- [0..size-1]]

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero width height = fromList2D $
  replicate height (replicate width 0)

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix matrixA matrixB
  | colsA /= rowsB = error "Matrix dimensions don't match for multiplication"
  | otherwise = fromList2D result
  where
    rowsA = getRows matrixA
    colsA = getCols matrixA
    rowsB = getRows matrixB
    colsB = getCols matrixB

    dotProduct xs ys = sum $ zipWith (*) xs ys
    
    result = 
      [ [ dotProduct (getRow matrixA i) (getCol matrixB j)
        | j <- [0..colsB-1] ]
      | i <- [0..rowsA-1] ]

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant matrix
  | rows /= cols = error "Determinant only defined for square matrices"
  | otherwise = determinant' listMatrix
  where
    rows = getRows matrix
    cols = getCols matrix
    listMatrix = [[getElem matrix i j | j <- [0..cols-1]] | i <- [0..rows-1]]
    
    determinant' [[x]] = x
    determinant' m = sumOfFirstRow
      where
        firstRow = head m
        sumOfFirstRow = foldl' sumAccumulator 0 (zip [0..] firstRow)

        getMinor matrix rowToRemove colToRemove =
          [removeAt colToRemove row | (i, row) <- zip [0..] matrix, i /= rowToRemove]
          where 
            removeAt index list = take index list ++ drop (index + 1) list

        sumAccumulator acc (j, elemVal) =
          acc + (fromIntegral ((-1) ^ j)) * elemVal * determinant' (getMinor m 0 j)
