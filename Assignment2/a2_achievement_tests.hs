import Test.HUnit

import A2b

tree = (KNode [
          (KNode [KLeaf 9, KLeaf 6, KLeaf 5]),
          KLeaf 1,  
          KLeaf 10])

treeSquared = (KNode [
                (KNode [KLeaf 81, KLeaf 36, KLeaf 25]),
                KLeaf 1,  
                KLeaf 100])

tests = test [
  -- kLeafList
  "kLeafList tree" ~: [9, 6, 5, 1, 10] ~=? (kLeafList tree),

  -- kMap
  "kMap (x -> x * x) tree" ~: treeSquared ~=? (kMap (\x -> x * x) tree),

  -- kFold
  "kFold (+) 0 tree" ~: 31 ~=? (kFold (+) 0 tree),
  "kFold (+) 1 tree" ~: 32 ~=? (kFold (+) 1 tree)]
