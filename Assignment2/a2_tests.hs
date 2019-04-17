import Test.HUnit
import A2b
import Data.Char
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

beta :: TriTree Int
beta = TriNode 1
          (TriNode 2
              EmptyNode EmptyNode EmptyNode)
          (TriNode 4
              EmptyNode EmptyNode EmptyNode)
          (TriNode 3
              EmptyNode EmptyNode EmptyNode)

betaSquared :: TriTree Int
betaSquared = TriNode 1
                (TriNode 4
                  EmptyNode EmptyNode EmptyNode)
                (TriNode 16
                  EmptyNode EmptyNode EmptyNode)
                (TriNode 9
                  EmptyNode EmptyNode EmptyNode)

modTree :: TriTree Int
modTree = TriNode 152
            (TriNode 24
                EmptyNode EmptyNode EmptyNode)
            (TriNode 5
                EmptyNode EmptyNode EmptyNode)
            (TriNode 3
                EmptyNode EmptyNode EmptyNode)

biggerModTree :: TriTree Int
biggerModTree = TriNode 467
            (TriNode 123
                EmptyNode EmptyNode EmptyNode)
            (TriNode 29
                EmptyNode EmptyNode EmptyNode)
            (TriNode 13
                EmptyNode EmptyNode EmptyNode)

alphaOrd :: TriTree Int
alphaOrd = (TriNode 97
                (TriNode 98
                    (TriNode 108
                        EmptyNode EmptyNode EmptyNode)
                    (TriNode 117
                        EmptyNode EmptyNode EmptyNode)
                    (TriNode 120
                        EmptyNode EmptyNode EmptyNode))
                (TriNode 99
                    (TriNode 114
                        EmptyNode EmptyNode EmptyNode)
                    (TriNode 113
                        EmptyNode EmptyNode EmptyNode)
                    (TriNode 112
                        EmptyNode EmptyNode EmptyNode))
                (TriNode 107
                    (TriNode 118
                        EmptyNode EmptyNode EmptyNode)
                    (TriNode 115
                        EmptyNode EmptyNode EmptyNode)
                    (TriNode 121
                        EmptyNode EmptyNode EmptyNode)))

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
  "nodeValue middleChild( leftChild alpha)" ~: 'u' ~=? (nodeValue (middleChild (leftChild alpha))),

  -- leftChild
  "leftChild beta" ~: (TriNode 2 EmptyNode EmptyNode EmptyNode) ~=? (leftChild beta),
  "leftChild (leftChild beta)" ~: EmptyNode ~=? (leftChild (leftChild beta)),

  -- middleChild
  "middleChild beta" ~: (TriNode 4 EmptyNode EmptyNode EmptyNode) ~=? (middleChild beta),

  -- rightChild
  "rightChild beta" ~: (TriNode 3 EmptyNode EmptyNode EmptyNode) ~=? (rightChild beta),

  -- inTree 
  "inTree 'a' alpha" ~: True ~=? (inTree 'a' alpha),
  "inTree 'z' alpha" ~: False ~=? (inTree 'z' alpha),
  "inTree 1 beta" ~: True ~=? (inTree 1 beta),
  "inTree 10 beta" ~: False ~=? (inTree 10 beta),

  -- leaflist
  "leafList alpha" ~: "luxrqpvsy" ~=? (leafList alpha),
  "leafList beta" ~: [2, 4, 3] ~=? (leafList beta),

  -- inOrderMap
  "inOrderMap (x -> x * x) beta" ~: betaSquared ~=? (inOrderMap (\x -> x * x) beta),
  "inOrderMap ord alpha" ~: alphaOrd ~=? (inOrderMap ord alpha),

  -- preOrderFold
  "preOrderFold (+) 0 beta" ~: 10 ~=? (preOrderFold (+) 0 beta),
  "preOrderFold (+) 1 beta" ~: 11 ~=? (preOrderFold (+) 1 beta),
  "preOrderFold mod 641 biggerModTree" ~: 9 ~=? (preOrderFold mod 641 biggerModTree),
  -- This should make sure that the tree is visited in the correct order
  "preOrderFold mod 641 modTree" ~: 1 ~=? (preOrderFold mod 641 modTree)] 