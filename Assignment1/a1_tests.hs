import Test.HUnit
-- notice that we specify the file here as a module to import its bindings, thus
-- Haskell looks for a file called a1c.hs in its current working folder
import A1c
          
-- this defines the set of tests, notice it's a big list!     
tests = test [
  -- 1. sDotProduct
  "sDotProduct 5 (0, 0) (0, 0)" ~: 0 ~=? (sDotProduct 5 (0, 0) (0, 0)),
  "sDotProduct 5 (3, 4) (4, 5)" ~: 160 ~=? (sDotProduct 5 (3, 4) (4, 5)),

  -- 2. distance
  "distance (0, 1) (0, 0)" ~: 1.0 ~=? (distance (0, 1) (0, 0)),
  "distance (0, 0) (3, 4)" ~: 5.0 ~=? (distance (0, 0) (3, 4)),

  -- 3. tripleDistance
  "tripleDistance (0, 0, 1) (0, 0, 0)" ~: 1.0 ~=? (tripleDistance (0, 0, 1) (0, 0, 0)),
  "tripleDistance (0, 0, 0) (1, 2, 2)" ~: 3.0 ~=? (tripleDistance (0, 0, 0) (1, 2, 2)),

  -- 4. findMin
  "findMin [2, 3, 1]" ~: 1 ~=? (findMin [2, 3, 1]),
  "findMin [4, 5, 6]" ~: 4 ~=? (findMin [4, 5, 6]),
  "findMin [-1, 0, 1]" ~: -1 ~=? (findMin [-1, 0, 1]),
  "findMin [-20, -10, 0]" ~: -20 ~=? (findMin [-20, -10, 0]),

  -- 5. tupleDotProduct
  "tupleDotProduct [1, 1, 1] [1, 2, 3]" ~: 6 ~=? (tupleDotProduct [1, 1, 1] [1, 2, 3]),
  "tupleDotProduct [1, 2, 3] [4, 5, 6]" ~: 32 ~=? (tupleDotProduct [1, 2, 3] [4, 5, 6]),
  "tupleDotProduct [73, 22, 79, 30, 29, 84, 88, 44, 1, 83, 33, 38, 52, 32, 98, 8, 25, 14, 42, 82, 89, 43, 18, 36, 65, 95, 99, 2, 74, 7, 54, 9, 55, 91, 19, 69, 57, 37, 31, 47, 39, 70, 62, 48, 80, 20, 16, 86, 28, 75] [59, 74, 10, 78, 90, 5, 44, 87, 71, 61, 58, 95, 25, 68, 26, 19, 88, 16, 48, 20, 1, 91, 37, 47, 6, 65, 31, 57, 28, 84, 49, 42, 30, 99, 60, 96, 77, 22, 92, 93, 53, 3, 56, 46, 40, 97, 76, 55, 69, 50]" ~: 120075 ~=? (tupleDotProduct [73, 22, 79, 30, 29, 84, 88, 44, 1, 83, 33, 38, 52, 32, 98, 8, 25, 14, 42, 82, 89, 43, 18, 36, 65, 95, 99, 2, 74, 7, 54, 9, 55, 91, 19, 69, 57, 37, 31, 47, 39, 70, 62, 48, 80, 20, 16, 86, 28, 75] [59, 74, 10, 78, 90, 5, 44, 87, 71, 61, 58, 95, 25, 68, 26, 19, 88, 16, 48, 20, 1, 91, 37, 47, 6, 65, 31, 57, 28, 84, 49, 42, 30, 99, 60, 96, 77, 22, 92, 93, 53, 3, 56, 46, 40, 97, 76, 55, 69, 50]),
  "tupleDotProduct [86, 93, 8, 82, 90, 65, 39, 87, 66, 41] [53, 26, 23, 14, 34, 90, 43, 92, 40, 62]" ~: 32081 ~=? (tupleDotProduct [86, 93, 8, 82, 90, 65, 39, 87, 66, 41] [53, 26, 23, 14, 34, 90, 43, 92, 40, 62]),


  -- 6. revZip2Lists
  "revZip2Lists [1, 2, 3] ['a', 'b', 'c']" ~: [('c', 3), ('b', 2), ('a', 1)] ~=? (revZip2Lists [1, 2, 3] ['a', 'b', 'c']),

  -- 7. everyThird
  "everyThird [1..30]" ~: [3,6,9,12,15,18,21,24,27,30] ~=? (everyThird [1..30]),
  "everyThird [1, 2, 3]" ~: [3] ~=? (everyThird [1, 2, 3]),
  "everyThird [1, 2, 3, 4]" ~: [3] ~=? (everyThird [1, 2, 3, 4]),
  "everyThird [1..10]" ~: [3, 6, 9] ~=? (everyThird [1..10])]
