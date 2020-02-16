import Test.Hspec
import Data.Hashable

import Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- Compare floating numbers
approxEq a b = 0.0001 > abs (a - b)

spec :: Spec
spec = do
  describe "#01 trapezoidArea" $ do
    it "computes area of triangle" $ do
      approxEq (convexRegularPolygonArea 3 9) 35.0740 `shouldBe` True
      approxEq (convexRegularPolygonArea 3 5) 10.8253 `shouldBe` True
    it "computes area of square" $ do
      approxEq (convexRegularPolygonArea 4 10) 100 `shouldBe` True
      approxEq (convexRegularPolygonArea 4 2.5) 6.25 `shouldBe` True
    it "computes area of hexagons" $ do
      approxEq (convexRegularPolygonArea 6 66) 11317.21997 `shouldBe` True
      approxEq (convexRegularPolygonArea 6 7) 127.30573 `shouldBe` True
    it "computes area of other polygons" $ do
      approxEq (convexRegularPolygonArea 10 6) 276.99151 `shouldBe` True
      approxEq (convexRegularPolygonArea 125 1) 1243.13618 `shouldBe` True

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

  describe "#03 infoBackwards" $
    it "informs about word in backwards" $ do
      infoBackwards "cba" `shouldBe` "'abc' is 'cba' backwards"
      infoBackwards "Marek" `shouldBe` "'keraM' is 'Marek' backwards"
      infoBackwards "LOL" `shouldBe` "'LOL' is 'LOL' backwards"

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

  describe "#05 manhattanDistance" $ do
    it "computes 2D Manhattan distance" $ do
      manhattanDistance (5, 5) (0, 0) `shouldBe` 10
      manhattanDistance (1, 5) (7, 3) `shouldBe` 8
      manhattanDistance (12, 3) (-1, 3) `shouldBe` 13
      manhattanDistance (-10, 5) (10, 15) `shouldBe` 30
    it "computes zero distance" $ do
      manhattanDistance (0, 0) (0, 0) `shouldBe` 0
      manhattanDistance (7, -2) (7, -2) `shouldBe` 0
    it "is commutative" $ do
      manhattanDistance (0, 5) (0, 3) `shouldBe` 2
      manhattanDistance (0, 3) (0, 5) `shouldBe` 2

  describe "#06 binstrToNat" $ do
    it "returns for trivials" $ do
      binstrToNat "" `shouldBe` 0
      binstrToNat "0" `shouldBe` 0
      binstrToNat "1" `shouldBe` 1
    it "returns valid binary string for powers of two" $ do
      binstrToNat "10" `shouldBe` 2
      binstrToNat "1000" `shouldBe` 8
      binstrToNat ("1" ++ replicate 10 '0') `shouldBe` 1024
      binstrToNat ("1" ++ replicate 16 '0') `shouldBe` 65536
    it "returns valid binary string for random" $ do
      binstrToNat "1011" `shouldBe` 11
      binstrToNat "01111" `shouldBe` 15
      binstrToNat "10101101" `shouldBe` 173
      binstrToNat (replicate 8 '1') `shouldBe` 255
      binstrToNat ("100" ++ replicate 8 '1') `shouldBe` 1279

  describe "#07 splitWsString" $ do
    it "works with single WS symbols" $ do
      splitWsString "a\nb\nc\nd" `shouldBe` ["a", "b", "c", "d"]
      splitWsString "a\rb\rc\rd" `shouldBe` ["a", "b", "c", "d"]
      splitWsString "a\tb\tc\td" `shouldBe` ["a", "b", "c", "d"]
      splitWsString "a b c d" `shouldBe` ["a", "b", "c", "d"]
    it "ignores extra WS symbols" $ do
      splitWsString " single_one  \n" `shouldBe` ["single_one"]
      splitWsString "  a b   c  d   " `shouldBe` ["a", "b", "c", "d"]
      splitWsString "a\t b \t\n c\r\nd\n" `shouldBe` ["a", "b", "c", "d"]
    it "returns empty string for empty string" $ do
      splitWsString "" `shouldBe` []
      splitWsString " " `shouldBe` []
      splitWsString " \n\t\r" `shouldBe` []

  describe "#08 flipType" $ do
    it "has length equal to 40" $
      length flipType `shouldBe` 28
    it "should match hash" $
      hash flipType `shouldBe` (-1303639695465303688)

  describe "#09 unorderedContainersAuthor" $ do
    it "has length equal to 15" $
      length unorderedContainersAuthor `shouldBe` 12
    it "should match hash" $
      hash unorderedContainersAuthor `shouldBe` (-6700700548817314600)

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
