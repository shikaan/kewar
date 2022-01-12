module Main (main) where

import QR (generate, CorrectionLevel(Q), showG)

exampleInput:: String
exampleInput = "CIAO PICCOLA"

main :: IO ()
main = do
  putStrLn $ showG $ generate exampleInput Q

