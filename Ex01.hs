-- Name: Senan d'Art,  Username: darts
module Ex01 where
import Data.Char (toUpper) -- needed for Part 1

{- Part 1

Write a function 'raise' that converts a string to uppercase

Function 'toUpper :: Char -> Char' converts a character to uppercase
if it is lowercase. All other characters are unchanged

-}
raise :: [Char] -> [Char]
raise [] = []
raise (x:xs) = toUpper x : raise xs

{- Part 2

Write a function 'nth' that returns the nth element of a list

-}
nth :: Int -> [a] -> a
nth 1 (a:as) = a
nth y (a:as) = nth (y-1) as

{- Part 3

write a function commonLen that compares two sequences
and reports the length of the prefix they have in common.

-}
commonLen :: Eq a => [a] -> [a] -> Int
commonLen [] [] = 0
commonLen [] _ = 0
commonLen _ [] = 0
commonLen (a:as) (b:bs) = 
    if a==b
        then ((commonLen as bs) + 1)
        else 0