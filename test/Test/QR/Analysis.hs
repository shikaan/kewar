module Test.QR.Analysis where

import QR.Analysis (analyze)
import QR.Types (Exception (InvalidCharacterSet), Mode (AlphaNumeric, Byte, Numeric))
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual)

tests :: [Test]
tests = do
  [ TestLabel "when checking an input string" $
      TestList
        [ TestCase (assertEqual "returns Numeric" (Right Numeric) (analyze "01234")),
          TestCase (assertEqual "returns AlphaNumeric" (Right AlphaNumeric) (analyze "HELLO WORLD")),
          TestCase (assertEqual "returns Byte" (Right Byte) (analyze "https://www.example.com")),
          TestCase (assertEqual "yields an error" (Left InvalidCharacterSet) (analyze "لْأَبْجَدِيَّ لْعَرَبِيَّة"))
        ]
    ]

suite :: Test
suite = TestList [TestLabel "analysis" $ TestList tests]