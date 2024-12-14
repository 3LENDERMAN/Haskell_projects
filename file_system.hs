data Filesystem = Folder String [Filesystem]
                | File String
                  deriving (Show)

-------------------------------------

removeStartingByA :: Filesystem -> Filesystem
removeStartingByA (File name) = if head name == 'a' then Folder "/" [] else File name
removeStartingByA (Folder name files) =
    let removeFile = filter (\xs -> case xs of (File x) -> head x /= 'a' 
                                               (Folder y _) -> head y /= 'a') files
        newFile = map removeStartingByA removeFile
    in Folder name newFile

main :: IO ()
main = do
  let fs1 = Folder "/" [File "ahoj", File "svete"]
  print $ removeStartingByA fs1
  -- Output: Folder "/" [File "svete"]

  let fs2 = Folder "/" [Folder "movies" [File "frozen", File "frozen 2"],
                        Folder "anime" [File "one punch man", File "naruto"]]
  print $ removeStartingByA fs2
  -- Output: Folder "/" [Folder "movies" [File "frozen", File "frozen 2"]]

  let fs3 = Folder "/" [Folder "music" [File "architects",
                                         File "animals as leaders",
                                         File "bring me the horizon",
                                         File "meshuggah"]]
  print $ removeStartingByA fs3
  -- Output: Folder "/" [Folder "music" [File "bring me the horizon", File "meshuggah"]]

  let fs4 = Folder "/" [Folder "alcohol" [File "gin", File "rum"]]
  print $ removeStartingByA fs4
  -- Output: Folder "/" []

-- outputs
-- 1 ~>* Folder "/" [File "svete"] OK
-- 2 ~>* Folder "/" [Folder "movies" [File "frozen", File "frozen 2"]] Z ČÁSTI
-- 3 ~>* Folder "/" [Folder "music" [File "bring me the horizon", File "meshuggah"]] OK
-- 4 ~>* Folder "/" [] NE

-- 1(right)  ~>* Folder "/" [File "svete"]
-- 2(wrong)  ~>* Folder "/" [Folder "movies" [File "frozen",File "frozen 2"],Folder "anime" [File "one punch man",File "naruto"]]

-- 3(right)  ~>* Folder "/" [Folder "music" [File "bring me the horizon",File "meshuggah"]]

-- 4(wrong)  ~>* Folder "/" [Folder "alcohol" [File "gin",File "rum"]]

