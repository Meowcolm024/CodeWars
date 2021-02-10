module Fusc where

import           Test.Hspec

fusc :: Integer -> Integer
fusc 0 = 0
fusc n = snd $ fh n

fh :: Integer -> (Integer, Integer)
fh 1 = (0, 1)
fh 2 = (1, 1)
fh n = if even n
    then let (hprv, hcur) = fh (n `quot` 2) in (hprv + hcur, hcur)
    else let (hprv, hcur) = fh (n `quot` 2 + 1) in (hprv, hprv + hcur)
