data Deed = GoodDeed Integer | BadDeed Integer deriving (Show, Eq)

data Present = Present String Integer deriving (Show, Eq)
type Wishlist = [Present]

assignPresents :: [([Deed], Wishlist)] -> [[String]]
assignPresents = map childPresent

childPresent :: ([Deed], Wishlist) -> [String]
childPresent (deeds, wishlist) =
  let points = pointsSum deeds
   in presentHelper points wishlist []

presentHelper :: Integer -> Wishlist -> [String] -> [String]
presentHelper _ [] result = result
presentHelper points ((Present name cost):rest) result =
  if points >= cost
    then presentHelper (points - cost) rest (result ++ [name])
    else presentHelper points rest result

pointsSum :: [Deed] -> Integer
pointsSum deeds = sum (map pointsTotal deeds)

pointsTotal :: Deed -> Integer
pointsTotal (GoodDeed points) = points
pointsTotal (BadDeed points) = (-points)

-- Test data
babyJesusData :: [([Deed], Wishlist)]
babyJesusData =
  [ ([GoodDeed 10, BadDeed 3, GoodDeed 2], [Present "bike" 5, Present "dog" 5, Present "socks" 2]),
    ([GoodDeed 10, BadDeed 3], [Present "blanket" 100, Present "A z IB015" 5, Present "iPhone" 5]),
    ([GoodDeed 10, BadDeed 3, BadDeed 100], [Present "socks" 2]),
    ([BadDeed 10, GoodDeed 20], [Present "you" 10])
  ]

-- Test
main :: IO ()
main = print $ assignPresents babyJesusData
