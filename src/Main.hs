module Main where

reverseString :: String -> String
reverseString [] = []
reverseString (char : restString) = reverseString restString ++ [char]

getCounterNumber :: Integer -> Integer
getCounterNumber baseNumber = read reversedString :: Integer
  where
    baseString = show baseNumber
    reversedString = reverseString baseString

isPalindrome :: Integer -> Bool
isPalindrome number = numberString == reverseNumberString
  where
    numberString = show number
    reverseNumberString = reverseString numberString

getLychrelsNumber :: Integer -> Integer -> String
getLychrelsNumber number transformations
  | transformations <= 0 = "Couldn't find palindrome of number."
  | isPalindrome counterSum = "Found Palindrome, " ++ show counterSum
  | otherwise = getLychrelsNumber counterSum (transformations - 1)
  where
    counterSum = number + getCounterNumber number

main :: IO ()
main = do
  print (getLychrelsNumber 59 4)