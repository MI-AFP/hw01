module Lib
    ( trapezoidArea
    , leapYear
    , infoReversed
    , countDigits
    , distance2D
    , natToBinstring
    , stringSplitToWords
    , allType
    , aesonAuthor
    , pluralizeFunc
    ) where

-- #01 = Area of trapezoid (check tests if not sure)
trapezoidArea a b h = undefined

-- #02 = Write boolean expression to check if year is leap
leapYear :: Word -> Bool
leapYear year = undefined

-- #03 = Message about reversed string ("abc is reversed cba")
infoReversed :: String -> String
infoReversed str = undefined

-- #04 = Count number of digits of given integer
-- (It can be done just by looking up suitable functions.
--  Try it WITHOUT conditions and recursion!)
countDigits x = undefined

-- #05 = Return euclidean distance of 2 2D points
-- (use variables x1, y1, x2, y2 in expression instead of undefined,
--  try not to use multiplication)
distance2D (x1, y1) (x2, y2) = undefined

-- #06 = Complete the function to translate natural number to
--       binary string (17 -> "1001", 0 -> "", 2 -> "10")
-- (You can introduce new helper function or use prepared if-then-else
--  by replacing True with condition, empty strings and add recursion.
--  The if-then-else is expression as any other in Haskell, try to be DRY)
natToBinstring :: Word -> String
natToBinstring 0 = "" -- no need to change this, end of recursion
natToBinstring x = if True then "" else ""

-- #07 = Lookup function that splits string to list of words (again strings)
stringSplitToWords :: String -> [String]
stringSplitToWords = undefined

-- #08 = What is the type of function "all"?
-- (For example for "odd" it would be "Integral a => a -> Bool")
allType :: String
allType = "<complete here>"

-- #09 = Who is author of "aeson" package?
-- (For example for "QuickCheck" it would be "Koen Claessen")
aesonAuthor :: String
aesonAuthor = "<complete here>"

-- #10 = You need a function that returns pluralized form of
--       English string ("letter" -> "letters", "tooth -> teeth", ...)
-- (Hint: do not reinvent the wheel, look it up and use as dependency!)
pluralizeFunc :: String -> String
pluralizeFunc = undefined
