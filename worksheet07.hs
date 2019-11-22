import Data.List

greet :: IO ()
greet = 
  do 
    inpStr <- getLine
    putStrLn $ "Hello " ++ inpStr ++"!"

greet' :: IO()
greet' =
  do
    putStrLn "What is your name?"
    greet


addLineStart :: [String] -> [String]
addLineStart hs = map ("> " ++) hs

addLinesStartToFile :: String -> String
addLinesStartToFile = unlines.addLineStart.lines 

hsToLhs :: String -> String -> IO ()
hsToLhs hs lhs =
  do 
    x <- readFile hs
    writeFile lhs (addLinesStartToFile x)
 
    

playRound :: Int -> IO()
playRound x = 
  do
    putStrLn "Guess an int"
    guessInt <- getLine
    if read guessInt == x
      then putStrLn "You guessed right"
      else do
        if read guessInt > x
          then do 
            putStrLn "Too high"
            playRound x
          else do
            putStrLn "Too Low"
            playRound x
  

--shows all the numbers that are multiplies of 3 the list [0-30]
number :: [Integer]
number = filter p [0..30]
            where p x = x `mod` 3 == 0


sumofOddnum :: Integer
sumofOddnum = sum(takeWhile (<10000) (filter odd (map p [1..])))
            where p x = x^2

chain :: Integer -> [Integer]
chain 1 = [1]
chain n    
    | even n = n:chain(n`div`2)
    | odd  n = n:chain(n*3+1)

numLongChain :: Int
numLongChain = length(filter isLong(map chain [1..100]))
            where isLong xs = (length xs) > 15