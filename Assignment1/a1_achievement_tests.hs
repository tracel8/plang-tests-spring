import Test.HUnit
--- This test suite includes tests for the "achivement" problems for Assignment 1
--- These tests are separate so that you can test the required problems before the achivement ones
import A1c

tests = test [
    -- 8. minMax (graduate required)
    "minMax [3, 1, 2]" ~: (1, 3) ~=? (minMax [3, 1, 2]),

    -- 9. everyK (undergraduate, graduate optional)
    "everyK 1 [1..10]" ~: [1..10] ~=? (everyK 1 [1..10])
]