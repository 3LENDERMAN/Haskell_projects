-- fullAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
fullAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
fullAdder False a b = (a /= b, a && b)
fullAdder True a b = (a == b, a || b)

binaryAdd :: [Bool] -> [Bool] -> ([Bool], Bool)
binaryAdd [] [] = ([], False)
binaryAdd (x:xs) (y:ys) = (sumBits : restSum, carryOut)
  where
    (carryOut, carryIn) = fullAdder carryIn x y
    (restSum, sumBits) = binaryAdd xs ys

showBinary :: ([Bool], Bool) -> String
showBinary (bits, carry) = (if carry then "1" else "") ++ (concatMap (\b -> if b then "1" else "0") bits)

main = do
    print $ binaryAdd [False] [False] -- ~>* ([False], False)                  -- 0 + 0 = 0 
    print $ binaryAdd [True] [False] -- ~>* ([True], False)                   -- 1 + 0 = 0 
    print $ binaryAdd [True] [True] -- ~>* ([False], True)                   -- 1 + 1 = 0 
    print $ binaryAdd [True, False] [False, True] -- ~>* ([True, True], False)             
    print $ binaryAdd [False, True] [True, True] -- ~>* ([False, False], True)            
    print $ binaryAdd [True, True] [True, True] -- ~>* ([True, False], True)             
    print $ binaryAdd [True, False, True] [False, False, True] -- ~>* ([True, True, False], False)      
    print $ binaryAdd [True, False, True] [False, True, True] -- ~>* ([False, False, False], True)    