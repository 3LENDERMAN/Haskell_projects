-- Sum function made by recursion
addup :: Num a => [a] -> a
addup [] = 0 
addup (y:ys) = y + addup ys
-- Map function made by clear recursion
listFun :: (a -> b) -> [a] -> [b]
listFun _ [] = []  
listFun a (x:xs) = a x : listFun a xs
-- Main function of this task.
haveMajority :: [String] -> [(String, Integer)] -> Bool
haveMajority _ [] = False
haveMajority parties results = 
    let totalSeats = addup $ listFun snd results
        coalitionSeats = addup [seats | (party, seats) <- results, party `elem` parties]
    in coalitionSeats > totalSeats `div` 2

realResults = [("Smer", 42), ("Hlas", 27), ("PS", 32), ("OLaNO", 16), ("SaS", 11), ("KDH", 12), ("SNS", 10)]

-- use examples
main = do
    print $ haveMajority ["Smer", "Hlas"] realResults
    print $ haveMajority ["Smer", "Hlas", "SNS"] realResults
    print $ haveMajority ["PS", "Hlas"] realResults
    print $ haveMajority ["PS", "Hlas", "SaS", "KDH"] realResults
    print $ haveMajority ["Party1"] [("Party1", 1), ("Party2", 2)]
    print $ haveMajority ["Party2"] [("Party1", 1), ("Party2", 2)]
    print $ haveMajority ["Party2", "Party3"] [("Party1", 1), ("Party2", 2)]
    print $ haveMajority ["Party2"] [("Party1", 10), ("Party2", 10)]
