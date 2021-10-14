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
  | transformations <= 0 = "Couldn't find palindrome of number. Number is Lychrel's number."
  | isPalindrome counterSum = "Found Palindrome, " ++ show counterSum
  | otherwise = getLychrelsNumber counterSum (transformations - 1)
  where
    counterSum = number + getCounterNumber number

main :: IO ()
main = do
  putStrLn "Find Lychrel's number"
  putStrLn "#####################"
  putStrLn "Enter number you want to check (natural):"
  input1 <- getLine
  let number = read input1 :: Integer
  putStrLn "Enter number of attemptst you want to make:"
  input2 <- getLine
  let attempts = read input2 :: Integer
  print (getLychrelsNumber number attempts)