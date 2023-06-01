module Main where

import Data.List.Split
import Data.List(sort,sortBy,nub)
import Data.Function (on)

main :: IO ()
main = do
    {-
    -- Titles (movies)
    ms <- readFile "data/title.basics.tsv/data.tsv"
    writeFile "filtered/title-basics.tsv" ""
    appendAll (filterOnConst (filterOnTconstLength (getRecords ms) 9) (movieTconsts (getRecords ms))) "filtered/title-basics.tsv"
    appendAll (filterOnConst (filterOnTconstLength (getRecords ms) 10) (movieTconsts (getRecords ms))) "filtered/title-basics.tsv"
    putStrLn "Checkpoint: Title filter on movies done"

    -- Crew (movies)
    filterOnMovies "data/title.crew.tsv/data.tsv" "filtered/title-crew.tsv"
    putStrLn "Checkpoint: Filter Crew on movies done"
    -- Principals (movies)
    filterOnMovies "data/title.principals.tsv/data.tsv" "filtered/title-principals.tsv"
    putStrLn "Checkpoint: Filter Principals on movies done"
    -- Ratings (movies)
    filterOnMovies "data/title.ratings.tsv/data.tsv" "filtered/title-ratings.tsv"
    putStrLn "Checkpoint: Filter Ratings on movies done"

    -}

    -- Read file as (tconst, votes) pair, and sort from highest to lowest amount of votes 
    v <- readFile "filtered/title-ratings.tsv"
    let votes = sortVotes $ tail $ getRecords v
    

    -- nums is the list of different splits we want
    let nums = [20000]
    
    putStrLn "Checkpoint: Begin loop on title absolute split"
    titleLoop votes nums
    
    putStrLn "Checkpoint: Begin iterations on other files"
    generalLoop nums

    putStrLn "Checkpoint: Begin sorting name file"
    
    {-
    ms <- readFile "data/name.basics.tsv/data.tsv"
    appendAll (filterOnTconstLength (getRecords ms) 9) "temp9.tsv"
    putStrLn "Checkpoint: temp9 done"
    ms <- readFile "data/name.basics.tsv/data.tsv"
    appendAll (filterOnTconstLength (getRecords ms) 10) "temp10.tsv"
    putStrLn "Checkpoint: temp10 done"

        
    writeFile "filtered/names-basics.tsv" ""
    

    ms <- readFile "filtered/temp9.tsv"
    appendFile "filtered/names-basics.tsv" ms
    putStrLn "Checkpoint: temp9 merged"
    ms <- readFile "filtered/temp10.tsv"
    appendFile "filtered/names-basics.tsv" ms
    putStrLn "Checkpoint: merging done"
    

    putStrLn "Checkpoint: Begin iterations on name files"
    nameLoop nums
    -} 
    
    -- remove when done

    putStrLn "Done."

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------                     I/O operations                 -----------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

filterOnMovies :: FilePath -> FilePath -> IO ()
filterOnMovies input output = do
    m <- readFile "filtered/title-basics.tsv"
    let movies = tconsts (getRecords m)
    d <- readFile input
    writeFile output ""
    appendAll (filterOnConst (filterOnTconstLength (getRecords d) 9) movies) output
    appendAll (filterOnConst (filterOnTconstLength (getRecords d) 10) movies) output

--------------------------------
--    Iterative filtering     --
--------------------------------
titleLoop :: [(String,Int)] -> [Int] -> IO ()
titleLoop votes [] = return ()
titleLoop votes (x:xs) = do
    titleIteration votes x
    titleLoop votes xs

titleIteration :: [(String,Int)] -> Int -> IO ()
titleIteration votes i = do
    r <- readFile "filtered/title-basics.tsv"
    let takeamt = take i (map fst votes)
    writeVotes (take i (map snd votes)) ("filtered/votes/votes" ++ (show i) ++".csv")
    writeVotes (takeamt) ("filtered/votes/tconsts" ++ (show i) ++".csv")
    writeFile ("filtered/titles/titles"++ show i ++".tsv") ""
    appendAll (filterTitles (getRecords r) takeamt) $ "filtered/titles/titles"++ show i ++".tsv"


generalLoop :: [Int] -> IO ()
generalLoop [] = return ()
generalLoop (x:xs) = do
    generalIteration x
    generalLoop xs

generalIteration :: Int -> IO ()
generalIteration i = do
    t <- readFile $ "filtered/titles/titles"++ show i ++".tsv"
    let ts = tconsts $ getRecords t
    filterAndWrite "filtered/title-principals.tsv" ("filtered/principals/principals"++ show i ++".tsv") ts
    filterAndWrite "filtered/title-crew.tsv" ("filtered/crew/crew"++ show i ++".tsv") ts
    filterAndWrite "filtered/title-ratings.tsv" ("filtered/ratings/ratings"++ show i ++".tsv") ts
    putStrLn $ "Checkpoint: " ++ show i ++ " done."

filterAndWrite :: FilePath -> FilePath -> [String] -> IO ()
filterAndWrite input output f = do
    d <- readFile input
    writeFile output ""
    appendAll (filterOnConst (getRecords d) f) output

nameLoop :: [Int] -> IO ()
nameLoop [] = return ()
nameLoop (x:xs) = do
    nameIteration x
    nameLoop xs

nameIteration :: Int -> IO ()
nameIteration i = do
    p <- readFile $ "filtered/principals/principals"++ show i ++".tsv"
    let uniqueNames = nub $ names (getRecords p)
    let sortedUniqueNames9 = sort $ filter (\x -> length x == 9) uniqueNames
    let sortedUniqueNames10 = sort $ filter (\x -> length x == 10) uniqueNames
    let fullSortedList = sortedUniqueNames9 ++ sortedUniqueNames10
    putStrLn $ "\tunique names in principals" ++ show i ++ ": " ++ show (length fullSortedList)
    filterAndWrite "filtered/names-basics.tsv" ("filtered/names/names"++ show i ++".tsv") fullSortedList
    putStrLn $ "Checkpoint: Names " ++ show i ++ " done."

names :: [String] -> [String]
names [] = []
names (s:xs) = nconst : names xs
    where (tconst:ordering:nconst:_) = splitOn "\t" s

getMissing :: [String] -> [String] -> [String]
getMissing [] b = []
getMissing (x:xs) b
    | x `elem` b = getMissing xs b
    | otherwise = x : getMissing xs b

--------------------------------
--        Write to file       --
--------------------------------
appendAll :: [String] -> FilePath -> IO ()
appendAll [] d = appendFile d ""
appendAll (x:xs) d = do 
  appendFile d (x ++ "\n")
  appendAll xs d

writeVotes :: (Show a) =>[a] -> FilePath -> IO ()
writeVotes input output = do
    writeFile output ""
    appendAll (map show input) output


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------                  Non-I/O operations                -----------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------


--------------------------------
--          Filtering         --
--------------------------------

filterOnConst :: [String] -> [String] -> [String]
filterOnConst _ [] = []
filterOnConst [] _ = []
filterOnConst (s:splitFile) (t:titles)
    | constConverter tconst == constConverter t = s : filterOnConst splitFile (t:titles)
    | constConverter tconst > constConverter t = filterOnConst (s:splitFile) titles
    | constConverter tconst < constConverter t = filterOnConst splitFile (t:titles)
    where
      (tconst:_) = splitOn "\t" s

filterOnTconstLength :: [String] -> Int -> [String]
filterOnTconstLength [] _ = []
filterOnTconstLength (s:splitFile) i
    | length (head (splitOn "\t" s)) == i = s : filterOnTconstLength splitFile i
    | otherwise = filterOnTconstLength splitFile i

filterTitles :: [String] -> [String] -> [String]
filterTitles [] _ = []
filterTitles (s:splitFile) titles 
    | tconst `elem` titles = s : filterTitles splitFile titles
    | otherwise = filterTitles splitFile titles 
    where
      (tconst:_) = splitOn "\t" s

--------------------------------
--        Get top votes       --
--------------------------------

sortVotes :: [String] -> [(String,Int)]
sortVotes xs = sortBy (flip compare `on` snd) $ parseVotes xs

parseVotes :: [String] -> [(String,Int)]
parseVotes [] = []
parseVotes (s:splitFile) = (tconst, converter numvotes) : parseVotes splitFile
    where (tconst:rating:numvotes:_) = splitOn "\t" s

--------------------------------
--          Helpers           --
--------------------------------
constConverter :: String -> Int
constConverter t = converter (tail $ tail t)

converter :: String -> Int
converter x = read x :: Int

getRecords :: String -> [String]
getRecords xs = filter (\x -> length (splitOn "\t" x) >= 2) $ splitOn "\n" xs

tconsts :: [String] -> [String]
tconsts [] = []
tconsts (s:splitFile) = tconst : tconsts splitFile
    where (tconst:titleType:rest) = splitOn "\t" s

movieTconsts :: [String] -> [String]
movieTconsts [] = []
movieTconsts (s:splitFile)
    | titleType == "movie" = tconst : movieTconsts splitFile
    | otherwise = movieTconsts splitFile
    where
      (tconst:titleType:rest) = splitOn "\t" s