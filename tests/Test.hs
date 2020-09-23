-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) notTests

ogTests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]


parseValueString :: String -> Either ParseError Value
parseValueString str = parse parseValue "There was a Parse Fail" str

tests = testGroup "Value tests" [
  testCase "simple True" $ parseValueString "True" @?=
    Right TrueVal,
  testCase "simple False" $ parseValueString "False" @?=
    Right FalseVal,
  testCase "simple None" $ parseValueString "None" @?=
    Right NoneVal,
  testCase "simple Num" $ parseValueString "37" @?=
    Right (IntVal 37),
  testCase "simple String" $ parseValueString "'ilike37'" @?=
    Right (StringVal "ilike37")
    ]




notString :: String -> Either ParseError Exp
notString str = parse notParse "There was a Parse Fail" str
notTests = testGroup "Not tests" [
  testCase "simple Not" $ notString "not 37" @?=
    Right (Not (Const (IntVal 37)))]

