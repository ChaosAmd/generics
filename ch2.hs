-- From Mathematics to Generic Programming :: Chapter2
-- Author: Amadeus Dabela Lanoa
-- Date: 27/11/2019

import Data.Bits

-- Ahmes algorithm O(n)
multiply0 :: Integer -> Integer -> Integer
multiply0 1 a = a
multiply0 n a = multiply0 (n - 1) a + a

-- Egyptian Multiplication using exponential decomposition O(log n)
multiply1 :: Integer -> Integer -> Integer
multiply1 n a 
  | n == 1    = a 
  | odd' n    = result n + a
  | otherwise = result n
  where
    odd' x = x .&. 0x1 == 1
    result x = multiply1 ((\y -> shiftR y 1)x) a + a

-- The 15 multiplication with only 5 additions, addition chain
multiplyBy15 :: Integer -> Integer
multiplyBy15 a = let b = a + a + a
                     c = b + b
                 in
                     c + c + b 

-- Addition chain for 31
multiplyBy31 :: Integer -> Integer
multiplyBy31 a = let b = a + a; c = b + a
                     d = c + c; e = d + d
                     f = e + e; g = f + d
                 in
                     g + a
                     
-- Some additional chains --
                     
-- Addition chain for 17
multiplyBy19 :: Integer -> Integer
multiplyBy19 a = let b = a + a; c = a + b
                     d = b + c; e = d + b
                     f = e + e
                 in 
                     f + d
                     
-- Addition chain for 27
multiplyBy25 :: Integer -> Integer
multiplyBy25 a = let b = a + a; c = a + b
                     d = c + b; e = d + d 
                     f = e + e; g = f + d
                 in 
                     g + b
                    

-- Defines a halving function apart from the the inverse definition
half :: Integer -> Integer
half x = shiftR x 1

-- Multiplication simple recursion by the computation:
-- accumulator + partial product
multAcc0 :: Integer -> Integer -> Integer -> Integer
multAcc0 r 1 a = r + a
multAcc0 r n a
  | odd n     = multAcc0 ra (half n) a + a
  | otherwise = multAcc0 r (half n) a
  where ra = r + a

-- Multiplication tail recursion
multAcc1 :: Integer -> Integer -> Integer -> Integer
multAcc1 r 1 a = r + a 
multAcc1 r n a = multAcc1 ra (half n) a + a
                 where ra | odd n     = r + a 
                          | otherwise = r 

-- The second and fourth accumulators functions were omitted
-- because of the impure and not convenient approach for functional paradigm
              
-- Strictly version, which in haskell is just syntax sugar
multAcc3 :: Integer -> Integer -> Integer -> Integer
multAcc3 r 1 a = r + a
multAcc3 r 0 a = r
multAcc3 r n a = multAcc3 ra na da
                 where ra | odd n     = r + a
                          | otherwise = r
                       na = (half n)
                       da = a + a
                       

-- Multiplication with iterative process behind
multiply2 :: Integer -> Integer -> Integer
multiply2 n a
  | n == 1    = a
  | otherwise = multAcc3 a (n - 1) a

-- Until n becomes odd, the value is doubled and n halved
-- Some modifications was necessary to make the algo work
multiply3 :: Integer -> Integer -> Integer
multiply3 1 a = a
multiply3 n a = multAcc3 a1 n1 (a1 + a1)
                where t  = comp (a, n)
                      a1 = fst t
                      n1 = snd t

comp :: (Integer, Integer) -> (Integer, Integer)
comp (a, n )
  | not $ odd n = comp(a + a, half n)
  | otherwise   = (a, half $ n - 1)
