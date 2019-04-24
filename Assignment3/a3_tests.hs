import Test.HUnit

import A3d

tests = test [
  -- 1. onlyLowercase
  "onlyLowercase [\"an\", \"the\", \"Hi\"]" ~: ["an", "the"] ~=? (onlyLowercase ["an", "the", "Hi"]),

  -- 2. longestString
  "longestString [\"one\", \"two\", \"thr\", \"fou\"]" ~: "one" ~=? (longestString ["one", "two", "thr", "fou"]),

  -- 3. longestString`
  "longestString` [\"one\", \"two\", \"thr\", \"fou\"]" ~: "fou" ~=? (longestString` ["one", "two", "thr", "fou"]),

  -- 4.1 longestStringHelper
  "longestStringHelper (>) [\"a\", \"bc\", \"def\"]" ~: "def" ~=? (longestStringHelper (>) ["a", "bc", "def"]),

  -- 4.2 longestString3
  "longestString3 [\"one\", \"two\", \"thr\", \"fou\"]" ~: "one" ~=? (longestString3 ["one", "two", "thr", "fou"]),

  -- 4.3 longestString4
  "longestString4 [\"one\", \"two\", \"thr\", \"fou\"]" ~: "fou" ~=? (longestString4 ["one", "two", "thr", "fou"]),

  -- 5. longestLowercase
  "longestLowercase [\"hello\", \"Hiya\", \"world\", \"Hi\"]" ~: "hello" ~=? (longestLowercase ["hello", "Hiya", "world", "Hi"]),

  -- 6. revStringRev
  "revStringRev \"Hi Ho\"" ~: "oh ih" ~=? (revStringRev "Hi Ho"),

  -- 7. firstAnswer
  "firstAnswer (v -> if v == 0 then Nothing else Just v) [0, 1]" ~: Just 1 ~=? (firstAnswer (\v -> if v == 0 then Nothing else Just v) [0, 1]),

  -- 8. allAnswers
  "allAnswers (x -> if x == 0 then Nothing else Just [x]) [1,0]" ~: Nothing ~=? (allAnswers (\x -> if x == 0 then Nothing else Just [x]) [1,0]),
  "allAnswers (x -> if x == 1 then Just [] else Nothing) [1]" ~: Just [] ~=? (allAnswers (\x -> if x == 1 then Just [] else Nothing) [1]),
  "allAnswers (x -> if x == 0 then Nothing else Just [x]) [1,2]" ~: Just [1,2] ~=? (allAnswers (\x -> if x == 0 then Nothing else Just [x]) [1,2]),

  -- Pattern Matching Questions
  -- 1. checkPat
  "checkPat (ConstantPat 5)" ~: True ~=? (checkPat (ConstantPat 5)),

  -- 2. match
  "match (Unit, VariablePat \"x\")" ~: Just [("x", Unit)] ~=? (match (Unit, VariablePat "x")),
  "match (Constructor (\"foo\", Unit), ConstructorPat (\"foo\", UnitPat))" ~: Just [] ~=? (match (Constructor ("foo", Unit), ConstructorPat ("foo", UnitPat))),
  "match (Tuple[Unit], TuplePat [UnitPat])" ~: Just [] ~=? (match (Tuple[Unit], TuplePat [UnitPat])),

  -- 3. firstMatch
  "firstMatch (Constant 5) [UnitPat,ConstantPat 5]" ~: Just [] ~=? (firstMatch (Constant 5) [UnitPat,ConstantPat 5])]