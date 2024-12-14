-- binary addition operation
binaryAdd :: [Bool] -> [Bool] -> ([Bool], Bool)
binaryAdd [] [] = ([False], False)
binaryAdd (x:xs) (y:ys) =
  let (carryIn, sum) = binaryAdd xs ys
      (carryOut, result) = fullAdder x y carryOut
  in (result : carryIn, sum)
  where
    fullAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
    fullAdder a b c =
      let sum = (a /= b) /= c
          carryOut = (a && b) || ((a /= b) && c)
      in (carryOut, sum)

main :: IO ()
main = do
  -- tests
  print $ binaryAdd [False] [False] -- ([False], False)
  print $ binaryAdd [True] [False]  -- ([True], False)
  print $ binaryAdd [True] [True]   -- ([False], True)
  print $ binaryAdd [True, False] [False, True] -- ([True, True], False)
  print $ binaryAdd [False, True] [True, True] -- ([False, False], True)
  print $ binaryAdd [True, True] [True, True] -- ([True, False], True)
  print $ binaryAdd [True, False, True] [False, False, True] -- ([True, True, False], False)
  print $ binaryAdd [True, False, True] [False, True, True] -- ([False, False, False, True], True)
