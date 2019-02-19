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

  describe "#06 binboolsToNat" $ do
    it "returns for trivials" $ do
      binboolsToNat [] `shouldBe` 0
      binboolsToNat [False] `shouldBe` 0
      binboolsToNat [True] `shouldBe` 1
    it "returns valid binary string for powers of two" $ do
      binboolsToNat [True, False] `shouldBe` 2
      binboolsToNat [True, False, False, False] `shouldBe` 8
      binboolsToNat ([True] ++ replicate 10 False) `shouldBe` 1024
      binboolsToNat ([True] ++ replicate 16 False) `shouldBe` 65536
    it "returns valid binary string for random" $ do
      binboolsToNat [True, False, True, True] `shouldBe` 11
      binboolsToNat [True, True, True, True] `shouldBe` 15
      binboolsToNat [True, False, True, False, True, True, False, True] `shouldBe` 173
      binboolsToNat (replicate 8 True) `shouldBe` 255
      binboolsToNat ([True, False, False] ++ replicate 8 True) `shouldBe` 1279

  describe "#07 makeMultilineString" $ do
    it "joins with new line" $ do
      makeMultilineString ["a", "b", "c", "d"] `shouldBe` "a\nb\nc\nd\n"
      makeMultilineString ["single"] `shouldBe` "single\n"
    it "returns empty string for no-lines" $ do
      makeMultilineString [] `shouldBe` ""

  describe "#08 iterateType" $ do
    it "has length equal to 40" $
      length iterateType `shouldBe` 20
    it "should match hash" $
      hash iterateType `shouldBe` 3584283388811533240

  describe "#09 pandocAuthor" $ do
    it "has length equal to 15" $
      length pandocAuthor `shouldBe` 15
    it "should match hash" $
      hash pandocAuthor `shouldBe` (-447579347104121658)

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
