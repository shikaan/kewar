module Test.Kewar.Encoding.Data where

import Kewar.Encoding.Data (toBitString)
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual)
import Kewar.Types (Mode(Numeric))

numeric :: [Test]
numeric = do
  [ TestLabel "when encoding a mono-chunk string" $
      TestList
        [ TestCase (assertEqual "handles one char" "0010" (toBitString Numeric "2")),
          TestCase (assertEqual "handles two chars" "0010000" (toBitString Numeric "16")),
          TestCase (assertEqual "handles three chars" "0010000000" (toBitString Numeric "128"))
        ],
    TestLabel "when encoding a longer string" $
      TestList
        [ TestCase (assertEqual "handles test string" "110110001110000100101001" (toBitString Numeric "8675309")),
          TestCase (assertEqual "handles 1-char remainder" "00100000000010" (toBitString Numeric "1282")),
          TestCase (assertEqual "handles 2-chars remainder" "00100000000010000" (toBitString Numeric "12816")),
          TestCase (assertEqual "handles 3-chars remainder" "00100000000010000000" (toBitString Numeric "128128"))
        ]
    ]

suite :: Test
suite = TestList [TestLabel "numeric" $ TestList numeric]