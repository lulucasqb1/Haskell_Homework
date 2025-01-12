{-
    Have you ever wondered how websites validate your credit card
number when you shop online? They don’t check a massive database
of numbers, and they don’t use magic. In fact, most credit providers
rely on a checksum formula for distinguishing valid numbers from
random collections of digits (or typing mistakes).
In this section, you will implement the validation algorithm for
credit cards. It follows these steps:

• Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on. For example,
[1,3,8,6] becomes [2,3,16,6].
• Add the digits of the doubled values and the undoubled digits from the original number. For example, [2,3,16,6] becomes
2+3+1+6+6 = 18•
• Calculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8.
If the result equals 0, then the number is valid.  
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use foldr" #-}
import Data.Char (digitToInt)

invList :: [Int] -> [Int]
invList [] = []
invList [x] = [x]
invList (x:y:zx) = invList zx ++ [y] ++ [x]

twoTimes :: [Int] -> [Int]
twoTimes [] = []
twoTimes [x] = [x]
twoTimes (x:y:zx) = x : 2*y : twoTimes zx 

getDigits :: Int -> [Int]
getDigits n = map (digitToInt) (show n)

sumAll :: [Int] -> Int
sumAll [] = 0 
sumAll (x:xs) = x + sumAll xs

checkMod8 :: Int -> Bool
checkMod8 n
 | mod n 8 == 0 = True
 | otherwise = False
    
validate :: Int -> Bool
validate n = checkMod8 (sumAll (twoTimes (invList (getDigits n))))

main :: IO ()
main = do
    print (validate 4012888888881881)