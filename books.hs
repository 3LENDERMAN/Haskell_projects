import Data.List (nub)

selectSuitableSeries :: [(String, String, Int)] -> [String]
selectSuitableSeries books =
  let
    filteredSeries = filter (\(series, _, _) -> seriesMeetsCriteria series) books
    seriesMeetsCriteria series =
      let
        seriesBooks = filter (\(s, _, _) -> s == series) books
        numBooks = length seriesBooks
        newestYear = maximum (map (\(_, _, year) -> year) seriesBooks)
      in
        numBooks <= 4 && newestYear <= 2010
  in
    nub (map (\(series, _, _) -> series) filteredSeries)

-- example of use
main :: IO ()
main = do
  let books = [
        ("The Lord of the Rings", "The Fellowship of the Ring", 1954),
        ("The Lord of the Rings", "The Two Towers", 1954),
        ("The Lord of the Rings", "The Return of the King", 1955),
        ("Harry Potter", "Philosopher's Stone", 1997),
        ("Harry Potter", "Chamber of Secrets", 1998),
        ("Harry Potter", "Prisoner of Azkaban", 1999),
        ("Harry Potter", "Goblet of Fire", 2000),
        ("Harry Potter", "Order of the Phoenix", 2003),
        ("Harry Potter", "Half-Blood Prince", 2005),
        ("Harry Potter", "Deathly Hallows", 2007),
        ("Mistborn: Era One", "The Final Empire", 2006),
        ("Mistborn: Era One", "The Well of Ascension", 2007),
        ("Mistborn: Era One", "The Hero of Ages", 2008),
        ("Hyperion Cantos", "Hyperion", 1989),
        ("Hyperion Cantos", "The Fall of Hyperion", 1990),
        ("Hyperion Cantos", "Endymion", 1996),
        ("Hyperion Cantos", "The Rise of Endymion", 1997),
        ("The Kingkiller Chronicle", "The Name of the Wind", 2007),
        ("The Kingkiller Chronicle", "The Wise Man's Fear", 2011)]
  print $ selectSuitableSeries books
