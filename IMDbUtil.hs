------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------                    EXPORTS                   -------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

module IMDbUtil (
    -- class
    IMDb (..)
    -- Data types and type synonyms
    , Principal (..), Crew (..), Rating (..), Title (..), Name (..), Tconst, Nconst, Year
    -- File parsing
    , dropHeader, parseFile, parseFileWithParser
    -- Explicit parsers for parseFileWithParser
    , parsePrincipal, parseCrew, parseRating, parseName, parseTitle
    -- for finding names
    , findName, findNames, findTitle, findTitles
    -- printers (I/O operations)
    , findAndPrintNames, findAndPrintTitles, genericPrint, genericPrintEnum
    -- Getters
    , getRowById, getRowListById, getColumn, getRow
    -- Filters
    , wheref, wherefL, searchColumn, searchAnyColumn
    -- Grouping
    , groupWith, groupWithColumn, groupAsList, applyToGroup, frequencies
    -- Other functions 
    , average, median, exists, L.nub, substring, minFrequency, maxFrequency, minimumBy, maximumBy, sortBy
    -- examples (I/O operations)
    , example1, example2
    )

    where

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------                    IMPORTS                   -------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

import qualified Data.Map as M (Map,fromListWith,toList)
import qualified Data.List as L (maximumBy,sortBy,minimumBy,group,sort,genericLength,isInfixOf,nub,tails)
import qualified Data.Char as C (toLower)
import Data.Ord (compare)
import Data.Function (on)
import qualified System.IO.Unsafe as US (unsafePerformIO)

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------                   DATA TYPES                 -------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------


type Tconst = String
type Nconst = String
type Year = Int

class (Show a, Eq a, Ord a, Read a) => IMDb a where
  getId :: a -> String
  search :: String -> a -> Bool

data Principal = Principal {principalID::Tconst,
                    ordering::Int,
                    nconst::Nconst,
                    category::String,
                    job::String,
                    characters::String}
              deriving Eq

data Crew = Crew {crewID::Tconst,
                    directors::[Nconst],
                    writers::[Nconst]}
              deriving Eq

data Name = Name {nameID::Nconst,
                    primaryName::String,
                    birthYear::Year,
                    deathYear::Year,
                    primaryProfession::[String],
                    knownForTitles::[Tconst]}
              deriving Eq

data Title = Title {titleID::Tconst,
                    titleType::String,
                    primaryTitle::String,
                    originalTitle::String,
                    isAdult::Bool,
                    startYear::Year,
                    endYear::Year,
                    runtimeMinutes::Int,
                    genres::[String]}
              deriving Eq

data Rating = Rating {ratingID::Tconst,
                    averageRating::Double,
                    numVotes::Int}
              deriving Eq

instance Ord Principal where
    compare x y = compare (principalID x) (principalID y)
instance Ord Crew where
    compare x y = compare (crewID x) (crewID y)
instance Ord Name where
    compare x y = compare (nameID x) (nameID y)
instance Ord Title where
    compare x y = compare (titleID x) (titleID y)
instance Ord Rating where
    compare x y = compare (ratingID x) (ratingID y)


instance Show Principal where
    show a = toStringNullCheck (principalID a) ++ "\t" ++ toStringInt (ordering a) 
                            ++ "\t" ++ toStringNullCheck (nconst a) ++ "\t" ++ toStringNullCheck (category a)
                            ++ "\t" ++ toStringNullCheck (job a)++ "\t" ++ toStringNullCheck (characters a)
instance Show Crew where
    show a = toStringNullCheck (crewID a)++ "\t" ++ toStringList (directors a) 
                            ++ "\t" ++ toStringList (writers a)
instance Show Name where
    show a = toStringNullCheck (nameID a)++ "\t" ++ toStringNullCheck (primaryName a) 
                            ++ "\t" ++ toStringInt (birthYear a)++ "\t" ++ toStringInt (deathYear a) 
                            ++ "\t" ++ toStringList (primaryProfession a) ++ "\t" ++ toStringList (knownForTitles a)
instance Show Title where
    show a = toStringNullCheck (titleID a)++ "\t" ++ toStringNullCheck (titleType a)++ "\t" ++ toStringNullCheck (primaryTitle a) 
                            ++ "\t" ++ toStringNullCheck (originalTitle a)++ "\t" ++ toStringBool (isAdult a) 
                            ++ "\t" ++ toStringInt (startYear a) ++ "\t" ++ toStringInt (endYear a)++ "\t" ++ toStringInt (runtimeMinutes a) 
                            ++ "\t" ++ toStringList (genres a)
instance Show Rating where
    show a = toStringNullCheck (ratingID a)++ "\t" ++ show (averageRating a)++ "\t" ++ toStringInt (numVotes a)

instance Read Principal where
    readsPrec _ = parsePrincipal
instance Read Crew where
    readsPrec _ = parseCrew
instance Read Name where
    readsPrec _ = parseName
instance Read Title where
    readsPrec _ = parseTitle
instance Read Rating where
    readsPrec _ = parseRating


instance IMDb Principal where
  getId = principalID
  search x a =  substring (stringToLower x) (stringToLower $ principalID a) || substring (stringToLower x) (stringToLower $ show (ordering a)) 
                        || substring (stringToLower x) (stringToLower $ nconst a) || substring (stringToLower x) (stringToLower $ category a) 
                        || substring (stringToLower x) (stringToLower $ job a) || substring (stringToLower x) (stringToLower $ characters a)

instance IMDb Crew where
  getId = crewID
  search x a = substring (stringToLower x) (stringToLower $ crewID a) 
                        || exists [ 1 | g <- directors a, substring (stringToLower x) (stringToLower g)]
                        || exists [ 1 | g <- writers a, substring (stringToLower x) (stringToLower g)]

instance IMDb Name where
  getId = nameID
  search x a = substring (stringToLower x) (stringToLower $ nameID a) || substring (stringToLower x) (stringToLower $ primaryName a) 
                        || substring (stringToLower x) (stringToLower $ show (birthYear a)) 
                        || substring (stringToLower x) (stringToLower $ show (deathYear a)) 
                        || exists [ 1 | g <- primaryProfession a, substring (stringToLower x) (stringToLower g)]
                        || exists [ 1 | g <- knownForTitles a, substring (stringToLower x) (stringToLower g)]

instance IMDb Title where
  getId = titleID
  search x a = substring (stringToLower x) (stringToLower $ titleID a) || substring (stringToLower x) (stringToLower $ titleType a) 
                        || substring (stringToLower x) (stringToLower $ primaryTitle a) || substring (stringToLower x) (stringToLower $ originalTitle a) 
                        || substring (stringToLower x) (stringToLower $ show (isAdult a)) 
                        || substring (stringToLower x) (stringToLower $ show (startYear a)) 
                        || substring (stringToLower x) (stringToLower $ show (endYear a)) 
                        || substring (stringToLower x) (stringToLower $ show (runtimeMinutes a)) 
                        || exists [ 1 | g <- genres a, substring (stringToLower x) (stringToLower g)]

instance IMDb Rating where
  getId = ratingID
  search x a = substring (stringToLower x) (stringToLower $ ratingID a) || substring (stringToLower x) (stringToLower $ show (averageRating a)) 
                        || substring (stringToLower x) (stringToLower $ show (numVotes a))


-- Helpers, not exported
newtype RemoveQuotes = RemoveQuotes String
instance Show RemoveQuotes where 
  show (RemoveQuotes str) = str

toStringList :: [String] -> String
toStringList [] = "\\N"
toStringList [x] = show (RemoveQuotes x)
toStringList (x:xs) = show (RemoveQuotes x) ++ "," ++ toStringList xs

toStringInt :: Int -> String
toStringInt (-1) = "\\N"
toStringInt x = show x

toStringNullCheck :: String -> String
toStringNullCheck "" ="\\N"
toStringNullCheck x = show (RemoveQuotes x)

toStringBool :: Bool -> String
toStringBool False = "0"
toStringBool True = "1"

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------                    PARSING                   -------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

dropHeader :: String -> String
dropHeader input = US.unsafePerformIO $ do 
  fullfile <- readFile input
  let output = (takeWhile (/='.') input) ++ "-noheader.tsv"
  writeFile output $ drop 1 $ dropWhile (/='\n') fullfile
  return output

parseFile :: (IMDb a) => String -> [a]
parseFile path = map read (filter (\x -> length (splitOn '\t' x) >= 2) (splitOn '\n' (US.unsafePerformIO $ readFile path)))

parseFileWithParser :: (IMDb a) => (String -> [(a,String)]) -> String -> [a]
parseFileWithParser parser path = map (\x -> getInstance (parser x) x) (filter (\x -> length (splitOn '\t' x) >= 2) (splitOn '\n' (US.unsafePerformIO $ readFile path)))
  where getInstance [] x = error $ "Error in parsing. Cannot parse: " ++ x
        getInstance r x = fst $ head r 


parsePrincipal :: String -> [(Principal,String)]
parsePrincipal x = let (tconst:ordering:nconst:category:job:characters:rest) = splitOn '\t' x in
        if length (splitOn '\t' x) == 6 && rest == [] then
         [(Principal tconst (np ordering) nconst (sp category) (sp job) (sp characters), "")]
        else []

parseCrew :: String -> [(Crew,String)]
parseCrew x = let (id:d:w:rest) = splitOn '\t' x in
        if length (splitOn '\t' x) == 3 && rest == [] then
         [(Crew id (lp d) (lp w), "")]
        else []

parseRating :: String -> [(Rating,String)]
parseRating x = let (id:r:v:rest) = splitOn '\t' x in
        if length (splitOn '\t' x) == 3 && rest == [] then
         [(Rating id (read r :: Double) (read v :: Int), "")]
        else [] 

parseName :: String -> [(Name,String)]
parseName x =let (nameID:primaryName:birthYear:deathYear:primaryProfession:knownForTitles:rest) = splitOn '\t' x in
        if length (splitOn '\t' x) == 6 && rest == [] then
         [(Name nameID (sp primaryName) (np birthYear) (np deathYear) (lp primaryProfession) (lp knownForTitles), "")]
        else []

parseTitle :: String -> [(Title,String)]
parseTitle x = let (tconst:titleType:primaryTitle:originalTitle:isAdult:startYear:endYear:runtimeMinutes:genres:rest) = splitOn '\t' x in
        if length (splitOn '\t' x) == 9 && rest == [] then
         [(Title tconst (sp titleType) (sp primaryTitle) (sp originalTitle) (bp isAdult) (np startYear) (np endYear) (np runtimeMinutes) (lp genres), "")]
        else [] 


-- Helpers for Null handling, not exported
lp :: String -> [String]
lp l
    | l == "\\N" = []
    | otherwise = splitOn ',' l

np :: String -> Int
np y
    | y == "\\N" = -1
    | otherwise = read y :: Int

bp :: String -> Bool
bp b
    | (read b :: Int) == 1 = True
    | otherwise = False

sp :: String -> String
sp s 
    | s == "\\N" = ""
    | otherwise = s


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------                  NAMES/TITLES                -------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

findName :: String -> Nconst -> String
findName path i = find (parseFileWithParser parseName path) i
  where find [] id = ""
        find (n:names) id
          | nameID n == id = primaryName n
          | otherwise = find names id

findNames :: String -> [Nconst] -> [String]
findNames path i = find (parseFileWithParser parseName path) i
  where find names [] = [] 
        find names (id:ids) = getEntry (filter (\x -> (nameID x == id)) names) : (find names ids)
          where getEntry xs
                        | xs /= [] = primaryName (head xs)
                        | otherwise = ""

findTitle :: String -> Tconst -> String
findTitle path i = find (parseFileWithParser parseTitle path) i
  where find [] id = ""
        find (t:titles) id
          | titleID t == id = primaryTitle t
          | otherwise = find titles id

findTitles :: String -> [Tconst] -> [String]
findTitles path i = find (parseFileWithParser parseTitle path) i
  where find titles [] = [] 
        find titles (id:ids) = getEntry (filter (\x -> (titleID x == id)) titles) : (find titles ids)
        getEntry xs | xs /= [] = primaryTitle (head xs)
                    | otherwise = ""


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------                     I/O                  ---------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

findAndPrintNames :: String -> [Nconst] -> IO ()
findAndPrintNames ns id = do
  genericPrint $ findNames ns id

findAndPrintTitles :: String -> [Tconst] -> IO ()
findAndPrintTitles ts id = do
  genericPrint $ findTitles ts id

genericPrint :: (Show a) => [a] -> IO ()
genericPrint [] = return ()
genericPrint (x:xs) = do
  putStr $ show x ++ "\n"
  genericPrint xs

genericPrintEnum :: (Show a) => [a] -> IO ()
genericPrintEnum xs = putStr $ enumHelper $ zip [0..] xs

-- Helper, not exported
enumHelper :: (Show a) => [(Int,a)] -> String
enumHelper [] = ""
enumHelper ((i,x):xs) = show i ++ " : " ++ show x ++ "\n" ++ enumHelper xs

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------                    GETTERS                   -------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

getRowById :: (IMDb a) => String -> [a] -> Maybe a
getRowById _ [] = Nothing
getRowById s (x:xs)
  | getId x == s = Just x
  | otherwise = getRowById s xs

getRowListById :: (IMDb a) => String -> [a] -> [a]
getRowListById _ [] = []
getRowListById s (x:xs)
  | getId x == s = x : getRowListById s xs
  | otherwise = getRowListById s xs

getColumn :: (IMDb a) => (a -> b) -> [a] -> [b]
getColumn _ [] = []
getColumn col (x:xs) = col x : getColumn col xs

getRow :: (IMDb a) => Int -> [a] -> a
getRow i xs = xs !! i

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------                    FILTERS                   -------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
wheref :: (IMDb a, Eq b) => (a -> b) -> (b -> b -> Bool) -> b -> [a] -> [a]
wheref column function term = filter (\x -> function (column x) term)

wherefL :: (IMDb a, Eq b) => (a -> b) -> (b -> [b] -> Bool) -> [b] -> [a] -> [a]
wherefL column function term = filter (\x -> function (column x) term)

searchColumn :: (IMDb a, Show b) => (a -> b) -> String -> [a] -> [a]
searchColumn column term = filter (\x -> substring (stringToLower term) $ stringToLower $ show (column x))

searchAnyColumn :: (IMDb a) => String -> [a] -> [a]
searchAnyColumn term = filter (\x -> search term x)

substring :: String -> String -> Bool
substring xs ys = xs `L.isInfixOf` ys

-- Helper, not exported
stringToLower :: String -> String
stringToLower "" = ""
stringToLower (c:cs) = C.toLower c : stringToLower cs

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
--------------                  GROUPING & AGGREGATION                --------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

groupWith :: (Ord b) => (a -> a -> a) -> [(b, a)] -> [(b, a)]
groupWith f xs = M.toList $ M.fromListWith f xs

groupWithColumn :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupWithColumn column xs = groupWith (++) $ map (\x -> (column x, [x])) xs

groupAsList :: (Ord b) => [(b, a)] -> [(b, [a])]
groupAsList xs = groupWith (++) (map (\(x,y) -> (x,[y])) xs)

frequencies :: (Ord a) => [a] -> [(Int, a)]
frequencies ns = [ (length ks, head ks) | ks <- L.group (L.sort ns) ]

applyToGroup :: (Ord b) => (b -> c) -> [(a,b)] -> [(a,c)]
applyToGroup f m = map (\(x,y) -> (x, f y)) m


average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / L.genericLength xs

median :: (Real a) => [a] -> a
median xs = L.sort xs !! (length xs `div` 2)

exists :: [a] -> Bool
exists xs = not $ null xs

minFrequency :: (Ord a) => [a] -> (Int, a)
minFrequency ns = minimum $ frequencies ns

maxFrequency :: (Ord a) => [a] -> (Int, a)
maxFrequency ns = maximum $ frequencies ns

minimumBy :: (Ord b) => (a -> b) -> [a] -> a
minimumBy f = L.minimumBy (compare `on` f)

maximumBy :: (Ord b) => (a -> b) -> [a] -> a
maximumBy f = L.maximumBy (compare `on` f)

sortBy :: (Ord b) => (a -> b) -> [a] -> [a]
sortBy f xs = L.sortBy (compare `on` f) xs

-- Helper, not exported
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn s [] = []
splitOn s xs = x : splitOn s (drop 1 y)
  where (x,y) = span (/= s) xs

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------                    EXAMPLES                  -------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

-- Example with explicit parser, expects a title.tsv file path
example1:: String -> IO ()
example1 filePath = do
  genericPrintEnum $ take 5 $ searchColumn genres "horror" $ parseFileWithParser parseTitle filePath

-- Example with auto parser, expects a title.tsv file path
example2:: String -> IO ()
example2 filePath = do
  print $ median $ getColumn runtimeMinutes $ parseFile filePath