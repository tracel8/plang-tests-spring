import Test.HUnit

import A3d

--Test convenience definitions
c1 = ConstantPat(5)
c2 = ConstantPat(100)
c3 = ConstantPat(17)

u = UnitPat
w = WildcardPat

v1 = VariablePat ("s")
v2 = VariablePat ("var")
v3 = VariablePat ("third")
v4 = VariablePat ("unit")

constr = ConstructorPat("construct", v1)
varvar = ConstructorPat("var", v2)
goodvar = ConstructorPat("var", c1)
nested = ConstructorPat("nester", goodvar)
nested2 = ConstructorPat("nest", constr)

tup1 = TuplePat([c1, c2, u, w])
tup2 = TuplePat([v1, u, c3, goodvar])
tupBreak = TuplePat([v2, goodvar, u])

constr2 = ConstructorPat("tuple", tup2)

tup3 = TuplePat([v1,v2, v3, v4])

cv1 = Constant (5)
cv2 = Constant (100)
cv3 = Constant (17)

uv = Unit

conv1 = Constructor("var", cv1)
conv2 = Constructor("construct", cv1)
nest1 = Constructor("nester", conv1)
nest2 = Constructor("nest", conv2)

t1 = Tuple[cv1, cv2, uv, cv3]
t2 = Tuple[cv1, uv, cv3, conv1]
t3 = Tuple[uv, cv1]

conv3 = Constructor("tuple", t2)

t4 = Tuple [cv1, cv2, cv3, uv]


tests = test [
  -- 1. onlyLowercase
  "onlyLowercase [\"an\", \"the\", \"Hi\"]" ~: ["an", "the"] ~=? (onlyLowercase ["an", "the", "Hi"]),

  -- 2. longestString
  "longestString [\"one\", \"two\", \"thr\", \"fou\"]" ~: "one" ~=? (longestString ["one", "two", "thr", "fou"]),

  -- 3. longestString`
  "longestString' [\"one\", \"two\", \"thr\", \"fou\"]" ~: "fou" ~=? (longestString' ["one", "two", "thr", "fou"]),

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
 -- Using the testing framework, the following test produces an error. However, if you execute the test on the command line, you can check the result that way.
 -- "allAnswers (x -> if x == 1 then Just [] else Nothing) [1]" ~: Just [] ~=? (allAnswers (\x -> if x == 1 then Just [] else Nothing) [1]),
  "allAnswers (x -> if x == 0 then Nothing else Just [x]) [1,2]" ~: Just [1,2] ~=? (allAnswers (\x -> if x == 0 then Nothing else Just [x]) [1,2]),

  -- Pattern Matching Questions
  -- 1. checkPat
  "checkPat (ConstantPat 5)" ~: True ~=? (checkPat (ConstantPat 5)),
  "checkPat c1" ~: True ~=? (checkPat c1),
  "checkPat varvar" ~: False ~=? (checkPat varvar),
  "checkPat goodvar" ~: True ~=? (checkPat goodvar),
  "checkPat u" ~: True ~=? (checkPat u),
  "checkPat w" ~: True ~=? (checkPat w),
  "checkPat c2" ~: True ~=? (checkPat c2),
  "checkPat c3" ~: True ~=? (checkPat c3),
  "checkPat constr" ~: True ~=? (checkPat constr),
  "checkPat nested" ~: True ~=? (checkPat nested),
  "checkPat tup1" ~: True ~=? (checkPat tup1),
  "checkPat tup2" ~: True ~=? (checkPat tup2),
  "checkPat tupBreak" ~: False ~=? (checkPat tupBreak),

  -- 2. match
  "match (Unit, VariablePat \"x\")" ~: Just [("x", Unit)] ~=? (match (Unit, VariablePat "x")),
  "match (Constructor (\"foo\", Unit), ConstructorPat (\"foo\", UnitPat))" ~: Just [] ~=? (match (Constructor ("foo", Unit), ConstructorPat ("foo", UnitPat))),
  "match (Tuple[Unit], TuplePat [UnitPat])" ~: Just [] ~=? (match (Tuple[Unit], TuplePat [UnitPat])),
  "match (cv1, w)" ~: Just [] ~=? (match (cv1, w)),
  "match (uv, w)" ~: Just [] ~=? (match (uv, w)),
  "match (conv1,w)" ~: Just [] ~=? (match (conv1,w)),
  "match (t1, w)" ~: Just [] ~=? (match (t1, w)),
  "match (cv1, v1)" ~: Just [("s",Constant 5)] ~=? (match (cv1, v1)),
  "match (cv3,v1)" ~: Just [("s",Constant 17)] ~=? (match (cv3,v1)),
  "match (uv, v1)" ~: Just [("s",Unit)] ~=? (match (uv, v1)),
  "match (uv, v2)" ~: Just [("var",Unit)] ~=? (match (uv, v2)),
  "match (t1, v2)" ~: Just [("var",Tuple [Constant 5,Constant 100,Unit,Constant 17])] ~=? (match (t1, v2)),
  "match (uv, u)" ~: Just [] ~=? (match (uv, u)),
  "match (conv1, u)" ~: Nothing ~=? (match (conv1, u)),
  "match (conv1, u)" ~: Nothing ~=? (match (conv1, u)),
  "match (cv2, c2)" ~: Just [] ~=? (match (cv2, c2)),
  "match (cv3, c2)" ~: Nothing ~=? (match (cv3, c2)),
  "match (conv1, goodvar)" ~: Just [] ~=? (match (conv1, goodvar)),
  "match (conv2, constr)" ~: Just [("s",Constant 5)] ~=? (match (conv2, constr)),
  "match (nest1, nested)" ~: Just [] ~=? (match (nest1, nested)),
  "match (nest2, nested)" ~: Nothing ~=? (match (nest2, nested)),
  "match (nest2, nested2)" ~: Just [("s",Constant 5)] ~=? (match (nest2, nested2)),
  "match (t1, tup1)" ~: Just [] ~=? (match (t1, tup1)),
  "match (t2, tup2)" ~: Just [("s",Constant 5)] ~=? (match (t2, tup2)),
  "match (t3, tup1)" ~: Nothing ~=? (match (t3, tup1)),
  "match (t3, constr)" ~: Nothing ~=? (match (t3, constr)),
  "match (t3, w)" ~: Just [] ~=? (match (t3, w)),
  "match (conv3, constr2)" ~: Just [("s",Constant 5)] ~=? (match (conv3, constr2)),

  -- 3. firstMatch
  "firstMatch uv [constr, varvar, tup1, u]" ~: Just [] ~=? (firstMatch uv [constr, varvar, tup1, u]),
  "firstMatch uv [constr, varvar, v1, w]" ~: Just [("s",Unit)] ~=? (firstMatch uv [constr, varvar, v1, w]),
  "firstMatch uv [constr, varvar, w, v1]" ~: Just [] ~=? (firstMatch uv [constr, varvar, w, v1]),
  "firstMatch uv [constr, tup1, tup2]" ~: Nothing ~=? (firstMatch uv [constr, tup1, tup2]),
  "firstMatch t4 [constr, tup1, tup2, tup3]" ~: Just [("s",Constant 5),("var",Constant 100),("third",Constant 17),("unit",Unit)] ~=? (firstMatch t4 [constr, tup1, tup2, tup3]),
  "firstMatch t4 [constr, tup1, tup2, c3]" ~: Nothing ~=? (firstMatch t4 [constr, tup1, tup2, c3]),
  "firstMatch t4 [constr, tup3, tup2, c3]" ~: Just [("s",Constant 5),("var",Constant 100),("third",Constant 17),("unit",Unit)] ~=? (firstMatch t4 [constr, tup3, tup2, c3]),
  "firstMatch t4 [constr, tup3, tup2, tup3]" ~: Just [("s",Constant 5),("var",Constant 100),("third",Constant 17),("unit",Unit)] ~=? (firstMatch t4 [constr, tup3, tup2, tup3]),
  "firstMatch (Constant 5) [UnitPat,ConstantPat 5]" ~: Just [] ~=? (firstMatch (Constant 5) [UnitPat,ConstantPat 5])]