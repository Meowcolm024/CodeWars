module Fusc where

fusc :: Integer -> Integer
fusc = snd . fh

fh :: Integer -> (Integer, Integer)
fh 0 = (0, 0)
fh 1 = (0, 1)
fh n = if even n
    then let (hprv, hcur) = fh (n `quot` 2) in (hprv + hcur, hcur)
    else let (hprv, hcur) = fh (n `quot` 2 + 1) in (hprv, hprv + hcur)
