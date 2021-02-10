module LastDigit (lastDigit) where

import Test.Hspec
import Test.QuickCheck

examples = do
  it "should work for some examples" $ do
    lastDigit []         `shouldBe` 1
    lastDigit [0,0]      `shouldBe` 1 -- 0 ^ 0
    lastDigit [0,0,0]    `shouldBe` 0 -- 0^(0 ^ 0) = 0^1 = 0
    lastDigit [1,2]      `shouldBe` 1
    lastDigit [12,18]    `shouldBe` 4
    lastDigit [8,21]     `shouldBe` 8
    lastDigit [3,4,5]    `shouldBe` 1
    lastDigit [4,3,6]    `shouldBe` 4
    lastDigit [7,6,21]   `shouldBe` 1
    lastDigit [7,11,2]   `shouldBe` 7
    lastDigit [12,30,21] `shouldBe` 6
    lastDigit [2,2,2,0]  `shouldBe` 4
    lastDigit [937640,767456,981242] `shouldBe` 0
    lastDigit [123232,694022,140249] `shouldBe` 6
    lastDigit [499942,898102,846073] `shouldBe` 6

simpleProperties = do
  it "lastDigit [x] == x `mod` 10" $
    property (\(NonNegative x) -> lastDigit [x] == x `mod` 10)
  it "lastDigit [x, y] == x ^ y `mod` 10" $
    property (\(NonNegative x) (NonNegative y) -> lastDigit [x, y] == x ^ y `mod` 10)

spec :: Spec
spec = do
  describe "Examples" examples
  describe "Simple properties" simpleProperties

main = hspec spec

lastDigit :: [Integer] -> Integer
lastDigit xs = (foldr (\x n -> x ^ (if n < 4 then n else n `mod` 4 + 4)) 1 xs) `mod` 10 
