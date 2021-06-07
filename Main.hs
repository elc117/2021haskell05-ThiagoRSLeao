--module Main where

bmi :: Float -> Float -> String
bmi peso altura = let x = peso / altura^2
    in case () of 
  _ | x >= 30 -> "ACIMA" 
    |x <= 18.5 -> "ABAIXO" 
    |otherwise -> "NORMAL" 
    

bmi' :: Float -> Float -> String
bmi' peso altura 
    |x >= 30 = "ACIMA" 
    |x <= 18.5 = "ABAIXO" 
    |otherwise = "NORMAL" 
    where x = peso / altura^2

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = let expr = (sum $ zipWith (*) digits mults) `mod` 11
    in if expr < 2 then 0 else 11-expr

cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
    where dv1 = cpfDV digits [10,9..] 
          dv2 = cpfDV (digits ++ [dv1]) [11,10..] 
          digits = take 9 cpf
        
andTable :: [(Bool, Bool, Bool)]
andTable = let tf = [True, False] in [(p,q,p && q) | (p,q) <- [ (p, q) | p <- tf, q <- tf]]

