module Kewar.Layout.Constants (size) where
import Kewar.Types (Version)

size :: Version -> Int
size v = ((v -1) * 4) + 21