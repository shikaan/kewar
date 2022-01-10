module Test.QR.Masking (maskingSuite) where

import QR.Masking (penalty1, penalty2, penalty3, penalty4)
import QR.ModulePlacement (Grid, Module (Black, White), insert, mkGrid, singleton, translateTo, Position)
import Test.HUnit (Test (..), assertEqual)

grid :: Int -> Int -> Grid
grid r c = mkGrid ((0, 0), (c - 1, r - 1))

-- TODO: copy-pasted
finder :: [(Position , Module)]
finder = do
  let whitePositions = [(1, 1), (2, 1), (3, 1), (4, 1), (5, 1), (1, 2), (1, 3), (1, 4), (1, 5), (2, 5), (3, 5), (4, 5), (5, 5), (5, 2), (5, 3), (5, 4)]
  let whites = zip whitePositions (repeat White)
  let blacks = zip [(i, j) | i <- [0 .. 6], j <- [0 .. 6], (i, j) `notElem` whitePositions] (repeat Black)
  blacks ++ whites

penalty1Test :: [Test]
penalty1Test = do
  let allWhite = grid 5 5
  let allBlack = insert allWhite [((i, j), Black) | i <- [0 .. 4], j <- [0 .. 4]]
  let homogeneous = insert (grid 11 11) [((i, j), if odd (i + j) then Black else White) | i <- [0 .. 10], j <- [0 .. 10]]
  let oneRow = insert homogeneous [((i, 0), Black) | i <- [0 .. 7]]
  let oneCol = insert homogeneous [((0, i), Black) | i <- [0 .. 7]]

  [ TestLabel "when checking a singleton" $
      TestList
        [ TestCase (assertEqual "return 0" 0 (penalty1 singleton))
        ],
    TestLabel "when checking a full grid" $
      TestList
        [ TestCase (assertEqual "handles all white" 30 (penalty1 allWhite)),
          TestCase (assertEqual "handles all black" 30 (penalty1 allBlack))
        ],
    TestLabel "when checking a generic grid" $
      TestList
        [ TestCase (assertEqual "returns 3 with only one row" 6 (penalty1 oneRow)),
          TestCase (assertEqual "returns 3 with only one col" 6 (penalty1 oneCol)),
          TestCase (assertEqual "returns 0 with homogeneous grid" 0 (penalty1 homogeneous))
        ]
    ]

penalty2Test :: [Test]
penalty2Test = do
  let allWhite = grid 3 3
  let allBlack = insert allWhite [((i, j), Black) | i <- [0 .. 2], j <- [0 .. 2]]
  let homogeneous = insert (grid 5 5) [((i, j), if odd (i + j) then Black else White) | i <- [0 .. 4], j <- [0 .. 4]]
  let oneSquare = insert homogeneous [((i, j), Black) | i <- [0, 1], j <- [0, 1]]
  let twoSquares = insert homogeneous $ [((i, j), Black) | i <- [0, 1], j <- [0, 1]] ++ [((i, j), White) | i <- [3, 4], j <- [3, 4]]

  [ TestLabel "when checking a singleton" $
      TestList
        [ TestCase (assertEqual "return 0" 0 (penalty2 singleton))
        ],
    TestLabel "when checking a full grid" $
      TestList
        [ TestCase (assertEqual "handles all white" 12 (penalty2 allWhite)),
          TestCase (assertEqual "handles all black" 12 (penalty2 allBlack))
        ],
    TestLabel "when checking a generic grid" $
      TestList
        [ TestCase (assertEqual "returns 0 with homogeneous grid" 0 (penalty2 homogeneous)),
          TestCase (assertEqual "returns 3 with one square" 3 (penalty2 oneSquare)),
          TestCase (assertEqual "returns 3 with two squares" 6 (penalty2 twoSquares))
        ]
    ]

penalty3Test :: [Test]
penalty3Test = do
  let empty = grid 20 20 -- v1
  let fs = map (insert empty) [finder, translateTo (13, 0) finder, translateTo (0, 13) finder]

  [ TestLabel "when checking a singleton" $
      TestList
        [ TestCase (assertEqual "return 0" 0 (penalty3 singleton))
        ],
    TestLabel "when checking a finders pattern" $
      TestList
        [ TestCase (assertEqual "handles white on right" 240 (penalty3 $ fs !! 0)),
          TestCase (assertEqual "handles white on left" 240 (penalty3 $ fs !! 1)),
          TestCase (assertEqual "handles white on top" 240 (penalty3 $ fs !! 2))
        ]
    ]

penalty4Test :: [Test]
penalty4Test = do
  let homogeneous = insert (grid 2 2) [((i, j), if odd (i + j) then Black else White) | i <- [0 .. 1], j <- [0 .. 1]]
  let mainlyBlack = insert (grid 2 2) [((i, j), if (i + j) `mod` 3 /= 0 then Black else White) | i <- [0 .. 1], j <- [0 .. 1]]

  [ TestLabel "when checking a grid" $
      TestList
        [ TestCase (assertEqual "returns no penalty if homogeneous" 0 (penalty4 homogeneous)),
          TestCase (assertEqual "returns penalty" 50 (penalty4 mainlyBlack))
        ]
    ]

maskingSuite :: Test
maskingSuite =
  TestList
    [ TestLabel "penalty1" $ TestList penalty1Test,
      TestLabel "penalty2" $ TestList penalty2Test,
      TestLabel "penalty3" $ TestList penalty3Test,
      TestLabel "penalty4" $ TestList penalty4Test
    ]