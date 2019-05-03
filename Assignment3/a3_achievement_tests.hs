import Test.HUnit

import A3b

tests = test [
  -- 1. typeCheckPatterns
  "typecheckPatterns [] [ConstantPat 5, WildcardPat, ConstantPat 3, VariablePat \"x\"]" ~: Just IntType ~=? (typecheckPatterns [] [ConstantPat 5, WildcardPat, ConstantPat 3, VariablePat "x"]),
  "typecheckPatterns [] [TuplePat [WildcardPat, TuplePat [WildcardPat,WildcardPat]],TuplePat[WildcardPat,WildcardPat]]" ~: Just (TupleType [AnythingType,TupleType[AnythingType,AnythingType]]) ~=? (typecheckPatterns [] [TuplePat [WildcardPat, TuplePat [WildcardPat,WildcardPat]],TuplePat[WildcardPat,WildcardPat]]),
  "typecheckPatterns [(\"c1\", \"t\", TupleType[IntType, DataType \"t\"]), (\"c2\", \"t\", UnitType)] [ConstructorPat(\"c1\", TuplePat [ConstantPat 5, ConstructorPat(\"c2\", UnitPat)]), ConstructorPat(\"c2\", UnitPat)]" ~: (Just DataType "t") ~=? (typecheckPatterns [("c1", "t", TupleType[IntType, DataType "t"]), ("c2", "t", UnitType)] [ConstructorPat("c1", TuplePat [ConstantPat 5, ConstructorPat("c2", UnitPat)]), ConstructorPat("c2", UnitPat)])]