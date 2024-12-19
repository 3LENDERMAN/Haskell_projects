rows :: [Integer] -> [Integer]
rows x = 1 : zipWith (+) x (tail x) ++ [1]

pascal :: [[Integer]]
pascal = iterate rows [1]


main :: IO ()
main = do
    print $ take 6 pascal -- ~>* [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
    print $ pascal !! 20 -- ~>* [1,20,190,1140,4845,15504,38760,77520,125970,167960,184756,167960,125970,77520,38760,15504,4845,1140,190,20,1]
    print $ length (pascal !! 100) -- ~>* 101
    print $ pascal !! 100 !! 50 -- ~>* 100891344545564193334812497256
    print $ pascal !! 1000 !! 500 -- ~>* 27028824094543
