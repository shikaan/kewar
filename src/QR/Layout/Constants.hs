module QR.Layout.Constants (size) where
import QR.Types (Version)

size :: Version -> Int
size v = ((v -1) * 4) + 21