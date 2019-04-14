import Test.HUnit

import A2b

tests = test [
  -- 1. removeAllExcept
  "removeAllExcept 'a' ['a', 'b', 'c', 'a']" ~: ['a', 'a'] ~=? (removeAllExcept 'a' ['a', 'b', 'c', 'a']),
  "removeAllExcept 1 [1, 2, 3, 4, 5]" ~: [1] ~=? (removeAllExcept 1 [1, 2, 3, 4, 5]),
  "removeAllExcept 1.5 [1, 1.5, 2, 1.5, 1]" ~: [1.5, 1.5] ~=? (removeAllExcept 1.5 [1, 1.5, 2, 1.5, 1]),

  -- 2. removeAll
  "removeAll 'a' ['a', 'b', 'c', 'a']" ~: ['b', 'c'] ~=? (removeAll 'a' ['a', 'b', 'c', 'a']),
  "removeAll 1 [1, 2, 3, 4, 5]" ~: [2, 3, 4, 5] ~=? (removeAll 1 [1, 2, 3, 4, 5]),
  "removeAll 1.5 [1, 1.5, 2, 1.5, 1]" ~: [1, 2, 1] ~=? (removeAll 1.5 [1, 1.5, 2, 1.5, 1]),

  -- 3. substitute
  "substitute 'a' 'b' ['a', 'b', 'c', 'a']" ~: ['b', 'b', 'c', 'b'] ~=? (substitute 'a' 'b' ['a', 'b', 'c', 'a']),
  "substitute 1 2 [1, 2, 3, 4, 5]" ~: [2, 2, 3, 4, 5] ~=? (substitute 1 2 [1, 2, 3, 4, 5]),
  "substitute 1.5 1 [1, 1.5, 2, 1.5, 1]" ~: [1, 1, 2, 1, 1] ~=? (substitute 1.5 1 [1, 1.5, 2, 1.5, 1]),

  -- 4. mergeSorted3
  "mergeSorted3 [2, 4, 6] [3, 7, 9] [1, 5, 8]" ~: [1..9] ~=? (mergeSorted3 [2, 4, 6] [3, 7, 9] [1, 5, 8]),
  "mergeSorted3 ['a', 'c', 'e'] ['d', 'i', 'p'] ['b', 'o', 'r']" ~: "abcdeiopr" ~=? (mergeSorted3 ['a', 'c', 'e'] ['d', 'i', 'p'] ['b', 'o', 'r'])]