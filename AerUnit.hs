
module Main where

import System.Exit (exitFailure)
import Grammar
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Monad

data TestCase tarrange tresult = TestCase
  { arrangement :: tarrange
  , expectedResult :: tresult }

data TestResult tarrange tresult = TestResult
  { originalTest :: TestCase tarrange tresult
  , resultValue ::  tresult }

parseExpressionTestCases :: [TestCase String (Either ParseError Expression)]
parseExpressionTestCases =
  [ "test" `succeedsWith` NamedEntry "test"
  , "'14'" `succeedsWith` NamedEntry "14"
  , "14" `succeedsWith` IndexedEntry 14
  , "*" `succeedsWith` Children
  , "..." `succeedsWith` Descendants
  , "." `succeedsWith` CurrentEntry
  , "@" `succeedsWith` TerminalNode
  , "a/b" `succeedsWith` InSequence (NamedEntry "a") (NamedEntry "b")
  , ".?a" `succeedsWith` IfMatch CurrentEntry (NamedEntry "a")
  , ".?(a/b)" `succeedsWith` IfMatch CurrentEntry (InSequence (NamedEntry "a") (NamedEntry "b"))
  , "a,b" `succeedsWith` AnyMatch (NamedEntry "a") (NamedEntry "b")
  , "a,b/c" `succeedsWith` AnyMatch (NamedEntry "a") (InSequence (NamedEntry "b") (NamedEntry "c"))
  ]
  where succeedsWith arrange result = TestCase arrange $ Right result
        failsWith arrange result = TestCase arrange $ Left result

runTest :: (tarrange -> tresult) -> TestCase tarrange tresult -> TestResult tarrange tresult
runTest f input = TestResult
  { originalTest = input
  , resultValue = f (arrangement input) }

printResult :: (Eq tresult, Show tresult, Show tarrange) => TestResult tarrange tresult -> IO ()
printResult result = do
  putStrLn $ "TEST:   " ++ show (arrangement testCase)
  putStrLn $ "EXPECT: " ++ show expectedResultForTest
  when (expectedResultForTest /= resultFromTest) $ putStrLn ("RESULT: " ++ show resultFromTest)
  putStrLn ""
  where resultFromTest = resultValue result
        testCase = originalTest result
        expectedResultForTest = expectedResult testCase

matchesExpectation :: (Eq tresult) => TestResult tarrange tresult -> Bool
matchesExpectation v = expectedResult (originalTest v) == resultValue v

main = do
  let testResults = runTest (parse parserExpression "expr") <$> parseExpressionTestCases
  forM_ testResults printResult
  let (passedTests, total) = foldl countResult (0, 0) (matchesExpectation <$> testResults)
  putStrLn $ show passedTests ++ "/" ++ show total
  when (passedTests /= total) exitFailure
  where countResult (passedTests, total) result = (passedTests + (if result then 1 else 0), total + 1)
