module Lib
    ( convexRegularPolygonArea
    , leapYear
    , infoBackwards
    , countDigits
    , manhattanDistance
    , binboolsToNat
    , makeMultilineString
    , iterateType
    , pandocAuthor
    , pluralizeFunc
    ) where

-- #01 = Area of convex regular polygon (check tests if not sure, use Pi from Prelude)
convexRegularPolygonArea n s = undefined

-- #02 = Write boolean expression to check if year is leap
leapYear :: Word -> Bool
leapYear year = undefined

-- #03 = Message about reversed string ("abc is reversed cba")
infoBackwards :: String -> String
infoBackwards str = undefined

-- #04 = Count number of digits of given integer
-- (It can be done just by looking up suitable functions.
--  Try it WITHOUT conditions and recursion!)
countDigits x = undefined

-- #05 = Return Manhattan distance of 2 2D points
-- (use variables x1, y1, x2, y2 in expression instead of undefined)
manhattanDistance (x1, y1) (x2, y2) = undefined

-- #06 = Complete the function to translate natural number to
--       binary string (17 -> "1001", 0 -> "", 2 -> "10")
-- (You can introduce new helper function or use prepared if-then-else
--  by replacing True with condition, empty strings and add recursion.
--  The if-then-else is expression as any other in Haskell, try to be DRY)
binboolsToNat :: [Bool] -> Word
binboolsToNat [] = 0 -- no need to change this, end of recursion
binboolsToNat (False:bs) = undefined -- False is head of list, bs is the tail
binboolsToNat (True:bs) = undefined -- True is head of list, bs is the tail

-- #07 = Lookup function that makes a multiline string from list of lines
makeMultilineString :: [String] -> String
makeMultilineString = undefined

-- #08 = What is the type of function "iterate"?
-- (For example for "odd" it would be "Integral a => a -> Bool")
iterateType :: String
iterateType = "<complete here>"

-- #09 = Who is author of "pandoc" package?
-- (For example for "QuickCheck" it would be "Koen Claessen")
pandocAuthor :: String
pandocAuthor = "<complete here>"

-- #10 = You need a function that returns pluralized form of
--       English string ("letter" -> "letters", "tooth -> teeth", ...)
-- (Hint: do not reinvent the wheel, look it up and use as dependency!)
pluralizeFunc :: String -> String
pluralizeFunc = undefined
