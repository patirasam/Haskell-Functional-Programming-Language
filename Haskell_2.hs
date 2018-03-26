fizzbuzz :: Int -> String
fizzbuzz x | mod x 15 == 0 = "fizzbuzz"
           | mod x 3  == 0 = "buzz"
           | mod x 5  == 0 = "fizz"
           | otherwise       = show x
main :: IO ()
main = do putStrLn "BuzzFizz"
          ninput<-getLine
          let ninputt= (read ninput::Int)
          a1 $ map fizzbuzz [1..ninputt]
          where
          a1 (x:xs) = putStrLn x >> a1 xs
