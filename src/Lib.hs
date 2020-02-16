module Lib
    ( convexRegularPolygonArea
    , leapYear
    , infoBackwards
    , countDigits
    , manhattanDistance
    , binstrToNat
    , splitWsString
    , flipType
    , unorderedContainersAuthor
    , pluralizeFunc
    ) where

-- #01 = Area of convex regular polygon (check tests if not sure; use Pi from Prelude)
convexRegularPolygonArea n s = undefined

-- #02 = Write boolean expression (avoid if-then-else) to check if year is leap
leapYear :: Word -> Bool
leapYear year = undefined

-- #03 = Message about reversed string ("'abc' is 'cba' backwards")
infoBackwards :: String -> String
infoBackwards str = undefined

-- #04 = Count number of digits of given integer
-- (It can be done just by looking up suitable functions.
--  Try it WITHOUT conditions and recursion!)
countDigits x = undefined

-- #05 = Return Manhattan distance of 2 2D points
-- (use variables x1, y1, x2, y2 in expression instead of undefined)
manhattanDistance (x1, y1) (x2, y2) = undefined

-- #06 = Complete the function to translate a binary string to natural number
--       binary string ("1001" -> 17, "" -> 0, "10" -> 2)
--       (Do not forget that String = [Char])
binstrToNat :: String -> Word
binstrToNat [] = 0 -- no need to change this, end of recursion
binstrToNat ('0':rest) = undefined -- 0 is on head, rest is the tail
binstrToNat ('1':rest) = undefined -- 1 is on head, rest is the tail

-- #07 = Lookup a function that splits String by whitespaces
splitWsString :: String -> [String]
splitWsString = undefined

-- #08 = What is the type of function "flip"?
-- (For example for "odd" it would be "Integral a => a -> Bool")
flipType :: String
flipType = "<complete the type here>"

-- #09 = Who is author of "unordered-containers" package?
-- (For example for "QuickCheck" it would be "Koen Claessen")
unorderedContainersAuthor :: String
unorderedContainersAuthor = "<complete the author here>"

-- #10 = You need a function that returns pluralized form of
--       English string ("letter" -> "letters", "tooth -> teeth", ...)
-- (Hint: do not reinvent the wheel, look it up and use as dependency!)
pluralizeFunc :: String -> String
pluralizeFunc = undefined
