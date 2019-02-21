# MI-AFP homework #01

First homework focused on Haskell basics with using GHCi, Hackage, Stackage, and Hoogle

## Task

Open `src/Lib.hs` and implement 10 tasks. You can also check the specification in `test/Spec.hs` and run the tests with `stack test`.

1. Math expression to compute area of convex regular polygon of *n* sides with size *s*.
2. Boolean expression to check whether given year is leap (has 366 days). Check wikipedia for the rules. Solution with hardcoded years or branching won't be accepted.
3. String expression to create info string about reversing given string. For example `infoBackwards "cba"` should give `"'abc' is 'cba' backwards"`. Lookup needed function from `Prelude` or guess it.
4. Function to count number of digits of an integer. It is not needed to use any conditions or recursion, just a combination of functions.
5. Math expression to compute Manhattan distance of 2 2D points with coords `x1`, `y1` and `x2`, `y2`.
6. Function to convert array of boolean values to a number. You can use prepared pattern to avoid using `head` and `tail` functions and conditions.
7. Lookup a function that joins strings with newlines. Write the function name instead of `undefined` (assign it, not as string).
8. Lookup and write the type of function `iterate` (`Prelude`) as a string. For example, type of `odd` would be `"Integral a => a -> Bool"`.
9. Lookup and write the name of `pandoc` package author. For example, for `QuickCheck` it would be `"Koen Claessen"`.
10. You need a function that pluralizes given english world. Of course such function is already implemented. Look it up and assign it as in task 7. You may need to add new dependency and import the module as was described in the first lesson. Do not implement the function on your own! (You might find out that used package is not perfect, you can contribute and improve it later on ;-)

## Notes

 * In case of uncertainty, check the [dummy homework](https://github.com/MI-AFP/hw00) to recall what is the homework workflow for this course.
 * If you encounter some trouble, create an issue in your repository.
 * In case you find abug or have an idea how to improve assignment project, create an issue or PR in this repository.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.
