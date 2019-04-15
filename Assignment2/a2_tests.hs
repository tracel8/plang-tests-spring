import Test.HUnit
import A2b

alpha:: TriTree Char
alpha =
    TriNode 'a'
        (TriNode 'b'
            (TriNode 'l'
                EmptyNode EmptyNode EmptyNode) 
            (TriNode 'u'
                EmptyNode EmptyNode EmptyNode) 
            (TriNode 'x'
                EmptyNode EmptyNode EmptyNode))
        (TriNode 'c'
            (TriNode 'r'
                EmptyNode EmptyNode EmptyNode) 
            (TriNode 'q'
                EmptyNode EmptyNode EmptyNode) 
            (TriNode 'p'
                EmptyNode EmptyNode EmptyNode))
        (TriNode 'k'
            (TriNode 'v'
                EmptyNode EmptyNode EmptyNode) 
            (TriNode 's'
                EmptyNode EmptyNode EmptyNode) 
            (TriNode 'y'
                EmptyNode EmptyNode EmptyNode))

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
  "mergeSorted3 [1.5, 2.5, 3.5] [2, 7.5, 8.5], [3, 9.5, 11.5]" ~: [1.5, 2, 2.5, 3, 3.5, 7.5, 8.5, 9.5, 11.5] ~=? (mergeSorted3 [1.5, 2.5, 3.5] [2, 7.5, 8.5] [3, 9.5, 11.5]),
  "mergeSorted3 ['a', 'c', 'e'] ['d', 'i', 'k'] ['b', 'o', 'r']" ~: "abcdeikor" ~=? (mergeSorted3 ['a', 'c', 'e'] ['d', 'i', 'k'] ['b', 'o', 'r']),

  -- nodeValue
  "nodeValue alpha" ~: 'a' ~=? (nodeValue alpha),

  "inTree 'a' alpha" ~: True ~=? (inTree 'a' alpha),

  -- leaflist
  "leafList alpha" ~: "luxrqpvsy" ~=? (leafList alpha)]