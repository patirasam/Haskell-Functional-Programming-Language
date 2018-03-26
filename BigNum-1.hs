{-
  Name: Samkit Patira
  Class: CS 252
  Assigment: HW1
  Date: <09/05/2017>
  Description: <To perform calculations of big numbers without using Integers>
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigPowerOf,
  bigEq,
  bigDec,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = bigAdd' x y 0
bigAdd' :: BigNum -> BigNum -> Block -> BigNum
bigAdd' [] (x:xs) y= x+y : bigAdd' [] xs ((x+y) `quot` 1000)
bigAdd' (x:xs) [] y= x+y : bigAdd' xs [] ((x+y) `quot` 1000)
bigAdd' [] [] x | x<1 = []
                | otherwise = [x]
bigAdd' (x:xs) (y:ys) z= ((x+y+z) `mod` 1000) : bigAdd' xs ys ((x+y) `quot` 1000)
bigAdd' _ _ _ = error " Error in addition "


bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported "
    else if negnum (reverse x) (reverse y) 
      then reverse $ stripLeadingZeroes $ reverse result 
      else error "Negative numbers not supported"  
      where result = bigSubtract' x y 0
stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

negnum :: BigNum -> BigNum -> Bool
negnum [x] [y] =  if x > -1 && y > -1 && x>=y
                  then True
                  else False
negnum [x] [] = True
negnum (x:xs) (y:ys) =  if x > -1 && y > -1 && x>y
                          then True
                        else if x==y 
                          then negnum xs ys
                        else False


-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' (x:xs) [] y = x-y : bigSubtract' xs [] ((x+y) `quot` 1000)
bigSubtract' [] [] x=[] 
bigSubtract' (x:xs) (y:ys) z | z==0 && x>=y =x-y  : bigSubtract' xs ys 0
                             | z==1 && x>y =x-1-y  : bigSubtract' xs ys 0
                             | z==1 && x==y =x-1-y+1000  : bigSubtract' xs ys 1
                             | z==0 && x<y =x+1000-y  : bigSubtract' xs ys 1
                             | z==1 && x<y =999-x-y  : bigSubtract' xs ys 1



bigEq :: BigNum -> BigNum -> Bool
bigEq x [] = False
bigEq [] x = False
bigEq [x] [y] = if x == y 
                then True
                else False
bigEq (x:xs) (y:ys) = if x == y
                      then bigEq xs ys
                      else False



bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply [] []=[]
bigMultiply [] [0] = [0]
bigMultiply [0] []=[0]
bigMultiply [] _ = []
bigMultiply a [] = [] 
bigMultiply a (b:bs) = bigAdd (bigMultiply' a [b] 0) ( 0 :(bigMultiply a bs))
bigMultiply' :: BigNum -> BigNum -> Block -> BigNum
bigMultiply' [x] [y] c =  if z==0 
                            then [q] 
                          else q:[z]
                          where 
                            z = (((x*y)+c) `quot` 1000)
                            q = (((x*y)+c)  `mod` 1000 )
bigMultiply' (x:xs) [y] c = (((x*y)+c) `mod` 1000 ) : (bigMultiply' xs [y] (((x*y)+c) `quot` 1000))

-- bigMultiply _ _ = error "Not implemented"


bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf _ [0] = [1]
bigPowerOf [0] [] = [0]
bigPowerOf a b = bigMultiply a (bigPowerOf a (bigSubtract b [1]))


prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]

