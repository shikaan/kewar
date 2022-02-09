module Test.Kewar.Encoding.Analysis where

import Kewar.Encoding.Analysis (mode)
import Kewar.Types (Exception (InvalidCharacterSet), Mode (AlphaNumeric, Byte, Numeric))
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual)

tests :: [Test]
tests = do
  [ TestLabel "when checking an input string" $
      TestList
        [ TestCase (assertEqual "returns Numeric" (Right Numeric) (mode "01234")),
          TestCase (assertEqual "returns AlphaNumeric" (Right AlphaNumeric) (mode "HELLO WORLD")),
          TestCase (assertEqual "returns Byte" (Right Byte) (mode "https://www.example.com")),
          TestCase (assertEqual "yields an error" (Left InvalidCharacterSet) (mode "لْأَبْجَدِيَّ لْعَرَبِيَّة"))
        ]
    ]

suite :: Test
suite = TestList [TestLabel "analysis" $ TestList tests]