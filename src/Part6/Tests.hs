module Part6.Tests where

import qualified Data.Map

import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Part6.Tasks

unit_eye = do
    eye 1 @?= one
    eye 1 @?= [[one]]
    eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
    eye 2 @?= [[one, 0], [0, one]]
    eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])

    where one :: Int; one = 1

unit_zero = do
    zero 1 1 @?= zz
    zero 2 1 @?= [[zz, zz]]
    zero 2 2 @?= [[zz, zz], [zz, zz]]
    zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([]::[((Int, Int), Int)]))
    where zz :: Int; zz = 0

unit_transposeM = do
    transposeM (5 :: Int) @?= (5 :: Int)
    
    let squareMatrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]]
    transposeM squareMatrix @?= [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
    
    let rectMatrix = [[1, 2, 3], [4, 5, 6]] :: [[Int]]
    transposeM rectMatrix @?= [[1, 4], [2, 5], [3, 6]]
    
    let rowMatrix = [[1, 2, 3, 4]] :: [[Int]]
    transposeM rowMatrix @?= [[1], [2], [3], [4]]
    
    let colMatrix = [[1], [2], [3], [4]] :: [[Int]]
    transposeM colMatrix @?= [[1, 2, 3, 4]]
    
    transposeM ([] :: [[Int]]) @?= ([] :: [[Int]])
    
    let sparseSquare = fromList2D [[1, 0, 3], [0, 5, 0], [7, 0, 9]] :: SparseMatrix Int
    let transposedSparseSquare = transposeM sparseSquare
    getRows transposedSparseSquare @?= 3
    getCols transposedSparseSquare @?= 3
    getElem transposedSparseSquare 0 0 @?= 1
    getElem transposedSparseSquare 0 2 @?= 7
    getElem transposedSparseSquare 2 0 @?= 3
    getElem transposedSparseSquare 2 2 @?= 9
    getElem transposedSparseSquare 1 1 @?= 5
    
    let sparseRect = fromList2D [[1, 0, 3], [0, 5, 0]] :: SparseMatrix Int
    let transposedSparseRect = transposeM sparseRect
    getRows transposedSparseRect @?= 3
    getCols transposedSparseRect @?= 2
    getElem transposedSparseRect 0 0 @?= 1
    getElem transposedSparseRect 0 1 @?= 0
    getElem transposedSparseRect 2 0 @?= 3
    getElem transposedSparseRect 2 1 @?= 0
    getElem transposedSparseRect 1 0 @?= 0
    getElem transposedSparseRect 1 1 @?= 5
    
    let sparseMat = fromList2D [[0, 1, 0], [2, 0, 3], [0, 4, 0]] :: SparseMatrix Int
    transposeM (transposeM sparseMat) @?= sparseMat
    
    let idMatrix = eye 3 :: SparseMatrix Int
    transposeM idMatrix @?= idMatrix

unit_multiplyMatrix = do
    multiplyMatrix (2 :: Int) (3 :: Int) @?= 6
    
    let a = [[1, 2], [3, 4]] :: [[Int]]
    let b = [[5, 6], [7, 8]] :: [[Int]]
    multiplyMatrix a b @?= [[19, 22], [43, 50]]
    
    let c = [[1, 2, 3], [4, 5, 6]] :: [[Int]]
    let d = [[7, 8], [9, 10], [11, 12]] :: [[Int]]
    multiplyMatrix c d @?= [[58, 64], [139, 154]]
    
    multiplyMatrix a (eye 2 :: [[Int]]) @?= a
    multiplyMatrix (eye 2 :: [[Int]]) a @?= a
    
    multiplyMatrix a (zero 2 2 :: [[Int]]) @?= zero 2 2
    multiplyMatrix (zero 2 2 :: [[Int]]) a @?= zero 2 2
    
    let sparseA = fromList2D a :: SparseMatrix Int
    let sparseB = fromList2D b :: SparseMatrix Int
    let result = multiplyMatrix sparseA sparseB
    getElem result 0 0 @?= 19
    getElem result 0 1 @?= 22
    getElem result 1 0 @?= 43
    getElem result 1 1 @?= 50
    
    multiplyMatrix sparseA (eye 2 :: SparseMatrix Int) @?= sparseA
    
    let sparseMat = fromList2D [[1, 0, 2], [0, 3, 0], [4, 0, 5]] :: SparseMatrix Int
    let sparseMat2 = fromList2D [[1, 0, 0], [0, 2, 0], [0, 0, 3]] :: SparseMatrix Int
    let sparseResult = multiplyMatrix sparseMat sparseMat2
    getElem sparseResult 0 0 @?= 1
    getElem sparseResult 1 1 @?= 6
    getElem sparseResult 2 2 @?= 15

unit_determinant = do
    determinant (5 :: Int) @?= 5
    determinant (0 :: Int) @?= 0
    determinant ((-3) :: Int) @?= -3
    
    determinant ([[7]] :: [[Int]]) @?= 7
    
    determinant ([[1, 2], [3, 4]] :: [[Int]]) @?= -2
    determinant ([[5, 6], [7, 8]] :: [[Int]]) @?= -2
    
    determinant ([[2, 5, 3], [1, -2, -1], [1, 3, 4]] :: [[Int]]) @?= -20
    
    determinant (eye 1 :: [[Int]]) @?= 1
    determinant (eye 2 :: [[Int]]) @?= 1
    determinant (eye 3 :: [[Int]]) @?= 1
    
    determinant (zero 2 2 :: [[Int]]) @?= 0
    determinant (zero 3 3 :: [[Int]]) @?= 0
    
    let sparse2x2 = fromList2D [[1, 2], [3, 4]] :: SparseMatrix Int
    determinant sparse2x2 @?= -2
    
    let sparse3x3 = fromList2D [[2, 5, 3], [1, -2, -1], [1, 3, 4]] :: SparseMatrix Int
    determinant sparse3x3 @?= -20
    
    determinant (eye 2 :: SparseMatrix Int) @?= 1
    
    determinant (zero 3 3 :: SparseMatrix Int) @?= 0
    
    let diagSparse = fromList2D [[2, 0, 0], [0, 3, 0], [0, 0, 4]] :: SparseMatrix Int
    determinant diagSparse @?= 24
