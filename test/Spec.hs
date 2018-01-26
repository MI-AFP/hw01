import Test.Hspec
import Data.Hashable

import Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- Compare floating numbers
approxEq a b = 0.000001 > abs (a - b)

spec :: Spec
spec = do
  describe "#01 trapezoidArea" $
    it "computes area of trapezoid" $ do
      trapezoidArea 5 5 2 `shouldBe` 10
      trapezoidArea 5 2 7 `shouldBe` 24.5
      trapezoidArea 10 2 15 `shouldBe` 90
      approxEq (trapezoidArea 7.5 6.3 2.1) 14.49 `shouldBe` True
      trapezoidArea 2 2 0 `shouldBe` 0
      trapezoidArea 0 0 5 `shouldBe` 0

  describe "#02 leapYear" $ do
    it "recognizes leap years" $ do
      leapYear 1916 `shouldBe` True
      leapYear 1992 `shouldBe` True
      leapYear 2000 `shouldBe` True
      leapYear 2016 `shouldBe` True
      leapYear 2248 `shouldBe` True
      leapYear 2396 `shouldBe` True
    it "recognizes non-leap years" $ do
      leapYear 1900 `shouldBe` False
      leapYear 1995 `shouldBe` False
      leapYear 2014 `shouldBe` False
      leapYear 2100 `shouldBe` False
      leapYear 2222 `shouldBe` False

  describe "#03 infoReversed" $
    it "informs about reversion" $ do
      infoReversed "cba" `shouldBe` "abc is reversed cba"
      infoReversed "Marek" `shouldBe` "keraM is reversed Marek"
      infoReversed "LOL" `shouldBe` "LOL is reversed LOL"
      infoReversed "" `shouldBe` " is reversed "

  describe "#04 countDigits" $ do
    it "counts digit of positive integers" $ do
      countDigits 7 `shouldBe` 1
      countDigits 12457 `shouldBe` 5
      countDigits 145887742123 `shouldBe` 12
      countDigits 42 `shouldBe` 2
    it "counts digit of negative integers" $ do
      countDigits (-5) `shouldBe` 1
      countDigits (-75687) `shouldBe` 5
      countDigits (-545081742783) `shouldBe` 12
      countDigits (-42) `shouldBe` 2
    it "counts digit of zero" $
      countDigits 0 `shouldBe` 1

  describe "#05 distance2D" $ do
    it "computes 2D euclidean distance" $ do
      distance2D (5, 5) (0, 0) `shouldBe` (5 * sqrt 2)
      distance2D (1, 5) (7, 3) `shouldBe` (2 * sqrt 10)
      distance2D (12, 3) (-1, 3) `shouldBe` 13
      distance2D (-10, 5) (10, 15) `shouldBe` (10 * sqrt 5)
    it "computes zero distance" $ do
      distance2D (0, 0) (0, 0) `shouldBe` 0
      distance2D (7, -2) (7, -2) `shouldBe` 0
    it "is commutative" $ do
      distance2D (0, 5) (0, 3) `shouldBe` 2
      distance2D (0, 3) (0, 5) `shouldBe` 2

  describe "#06 natToBinstring" $ do
    it "returns empty string for 0" $
      natToBinstring 0 `shouldBe` ""
    it "returns valid binary string for powers of two" $ do
      natToBinstring 1 `shouldBe` "1"
      natToBinstring 2 `shouldBe` "10"
      natToBinstring 16 `shouldBe` "10000"
      natToBinstring 1024 `shouldBe` "10000000000"
      natToBinstring 65536 `shouldBe` "10000000000000000"
    it "returns valid binary string for random" $ do
      natToBinstring 15 `shouldBe` "1111"
      natToBinstring 173 `shouldBe` "10101101"
      natToBinstring 215 `shouldBe` "11010111"
      natToBinstring 255 `shouldBe` "11111111"
      natToBinstring 358 `shouldBe` "101100110"
      natToBinstring 1279 `shouldBe` "10011111111"

  describe "#07 stringSplitToWords" $ do
    it "splits by single space" $ do
      stringSplitToWords "a b c d" `shouldBe` ["a", "b", "c", "d"]
      stringSplitToWords "single" `shouldBe` ["single"]
      stringSplitToWords "" `shouldBe` []
    it "splits different whitespace chars" $ do
      stringSplitToWords "a\nb\tc\rd" `shouldBe` ["a", "b", "c", "d"]
      stringSplitToWords "a\r\n  b \t c \rd" `shouldBe` ["a", "b", "c", "d"]
      stringSplitToWords "a.b\t\t\tc,d" `shouldBe` ["a.b", "c,d"]
      stringSplitToWords "   " `shouldBe` []

  describe "#08 allType" $ do
    it "has length equal to 40" $
      length allType `shouldBe` 40
    it "should match hash" $
      hash allType `shouldBe` 630186005456252040

  describe "#09 aesonAuthor" $ do
    it "has length equal to 16" $
      length aesonAuthor `shouldBe` 16
    it "should match hash" $
      hash aesonAuthor `shouldBe` (-4676277736506712894)

  describe "#10 pluralizeFunc" $ do
    it "pluralizes with s" $ do
      pluralizeFunc "pizza" `shouldBe` "pizzas"
      pluralizeFunc "car" `shouldBe` "cars"
      pluralizeFunc "science" `shouldBe` "sciences"
      pluralizeFunc "computer" `shouldBe` "computers"
    it "pluralizes with es" $ do
      pluralizeFunc "penny" `shouldBe` "pennies"
      pluralizeFunc "bus" `shouldBe` "buses"
      pluralizeFunc "tax" `shouldBe` "taxes"
      pluralizeFunc "tomato" `shouldBe` "tomatoes"
    it "pluralizes irregular and tricky" $ do
      pluralizeFunc "tooth" `shouldBe` "teeth"
      pluralizeFunc "wolf" `shouldBe` "wolves"
      pluralizeFunc "analysis" `shouldBe` "analyses"
      pluralizeFunc "deer" `shouldBe` "deer"
      pluralizeFunc "goose" `shouldBe` "geese"
      pluralizeFunc "phenomenon" `shouldBe` "phenomena"
    it "doesn't pluralize plural" $ do
      pluralizeFunc "ways" `shouldBe` "ways"
      pluralizeFunc "computers" `shouldBe` "computers"
      pluralizeFunc "people" `shouldBe` "people"
      pluralizeFunc "analyses" `shouldBe` "analyses"
