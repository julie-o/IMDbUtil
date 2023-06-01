
module Main where

import IMDbUtil as I
import System.Environment (getArgs)





-- Which Director-Actor pair has worked together the most?
directorActorPair :: [Principal] -> (Nconst,(Int,Nconst))
directorActorPair xs = maximumBy (fst . snd) $ applyToGroup maxFrequency $ groupActorsToDirector $ groupPersonsToTitle xs

groupPersonsToTitle :: [Principal] -> [(Tconst,[Principal])]
groupPersonsToTitle xs = groupAsList [ (getId x, x) | x <- wherefL category elem ["actor","actress","director"] xs]

groupActorsToDirector :: [(Tconst,[Principal])] -> [(Nconst,[String])] 
groupActorsToDirector xs = groupWith (++) [ (nconst x, getColumn nconst (wherefL category elem ["actor","actress"] people)) 
                        | (title, people) <- xs, x <- wheref category (==) "director" people,
                        (exists (wheref category (==) "actor" people) || exists (wheref category (==) "actress" people))]




-- Which Director-Writer pair has worked together the most?
directorWriterPair :: [Crew] -> (Nconst,(Int,Nconst))
directorWriterPair xs = maximumBy (fst . snd) $ applyToGroup maxFrequency $ groupWith (++) [ (director, filter (/= director) (writers title)) | title <- xs, director <- directors title, hasWriters director (writers title)]
  where hasWriters :: Nconst -> [Nconst] -> Bool
        hasWriters _ [] = False
        hasWriters d (t:ts)
            | t == d = False || hasWriters d ts
            | otherwise = True








-- Find the degree of separation between actor A and actor B
degreesOfSeparation :: String -> String -> [Principal] -> Int
degreesOfSeparation a b ps = (length (separationList a b $ wherefL category elem ["actor","actress"] ps)) - 1

-- The list of people from actor A to actor B
separationList :: String -> String -> [Principal] -> [String]
separationList a b ps = reverse $ backPropagation b $ reverse $ takeStep b [(a,"")] [] ps


-- While the first actor in the queue is not actor B, call function recursively and:
--    1. add all actors that can be reached directly (i.e. working on the same movie, degree 1) from the 
--       current actor to the queue as (reachedActor,currentActor)-pairs, remove current element from queue
--    2. add the current queue element to the visited list
--    3. remove all rows that mention the current actor from the list of Principal records to prevent infinite looping
-- takes actor B (the last node), the queue of actors to visit, the list of visited actors, and a list of unused Principal records as arguments
takeStep :: String -> [(String,String)] -> [(String,String)] -> [Principal] -> [(String,String)]
takeStep _ [] visited _ = visited
takeStep b ((q,prev):queue) visited ps
  | b == q = (q,prev):visited
  | otherwise = takeStep b (queue ++ expand (nub $ getColumn getId $ wheref nconst (==) q ps) ps q visited)
                           ((q,prev):visited) 
                           (wherefL nconst (notElem) (map fst visited) ps)
    where expand ts ps n visited = [(x, n) | x <- getColumn nconst $ wherefL principalID elem ts ps, x /= n]

-- While there is a previous actor (i.e. the actor is not ""), backpropagate recursively through the visited list
-- takes the current actor and the list of visited actors as arguments
backPropagation :: String -> [(String,String)] -> [String]
backPropagation "" _ = []
backPropagation b xs = (fst $ backit b xs) : backPropagation (snd $ backit b xs) xs
  where backit _ [] = error "No match found"
        backit b (x:xs) | b == fst x = x
                        | otherwise = backit b xs








-- Which director has the highest average rating?
highestAverageRating :: [Principal] -> [Rating] -> (Double,String)
highestAverageRating ps rs = maximumBy fst $ [ (avgRating rs titles, person) | (person,titles) <- groupTitlesToDirector ps ]

groupTitlesToDirector :: [Principal] -> [(Nconst, [Tconst])]
groupTitlesToDirector ps = groupAsList [(nconst x, getId x) | x <- wheref category (==) "director" ps]

avgRating :: [Rating] -> [Tconst] -> Double
avgRating rs titles = average $ getColumn averageRating $ wherefL getId elem titles rs









-- Who has the highest sum of # of votes in their 'known for'-list?
mostPopularPerson :: [Name] -> [Rating] -> (Name, Int)
mostPopularPerson ns rs = maximumBy snd $ [(person, sumForPerson (knownForTitles person) rs) | person <- ns]

sumForPerson :: [Tconst] -> [Rating] -> Int
sumForPerson titles rs = sum $ getColumn numVotes $ wherefL getId elem titles rs







-- Version using list comprehension for comparison
mostPopularPersonAlt :: [Name] -> [Rating] -> (Name, Int)
mostPopularPersonAlt ns rs = maximumBy snd $ [(person, sumForPersonAlt (knownForTitles person) rs) | person <- ns]

sumForPersonAlt :: [Tconst] -> [Rating] -> Int
sumForPersonAlt p rs = sum [getVotes $ getRowById x rs | x <- p, getRowById x rs /= Nothing]
  where getVotes (Just x) = numVotes x
        getVotes _ = error ""







-- Which actor has the most amount of movies?
mostMoviesPerActor :: [Principal] -> (String, Int)
mostMoviesPerActor xs = maximumBy snd $ groupWith (+) [ (nconst x, 1) | x <- wherefL category elem ["actor","actress"] xs]





-- What is the most common birth and death years? (note that death year gotta be non-null)
-- First int in result is the year, the second int is the number of occurences
mostFrequentYear :: [Name] -> (Name -> Int) -> (Int, Int)
mostFrequentYear xs bOrD = maximumBy snd $ groupWith (+) [ (bOrD x, 1) | x <- xs, bOrD x /= -1]






-- What is the most popular genre?
mostPopularGenre :: [Title] -> (String, Int)
mostPopularGenre ts = maximumBy snd $ groupWith (+) [ (g, 1) | t <- ts, g <- genres t ]






-- What is the average runtime
averageRuntime :: [Title] -> Double
averageRuntime ts = average $ getColumn runtimeMinutes ts






-- What is the median rating?
medianRating :: [Rating] -> Double
medianRating rs = median $ getColumn averageRating rs







-- Main function
main :: IO ()
main = do
  (func:args) <- getArgs
  case func of
    "directorActorPair" -> do 
      print $ directorActorPair (parseFile (args !! 0))
    "directorWriterPair" -> do 
      print $ directorWriterPair (parseFile (args !! 0))
    "mostMoviesPerActor" -> do 
      print $ mostMoviesPerActor (parseFile (args !! 0))
    "degreesOfSeparation" -> do
      print $ degreesOfSeparation (args !! 0) (args !! 1) (parseFile (args !! 2))
    "highestAverageRating" -> do
      print $ highestAverageRating (parseFile (args !! 0)) (parseFile (args !! 1))
    "mostPopularPerson" -> do
      let result = mostPopularPerson (parseFile (args !! 0)) (parseFile (args !! 1))
      print $ (primaryName (fst result),snd result)
    "mostPopularPersonAlt" -> do
      let result = mostPopularPersonAlt (parseFile (args !! 0)) (parseFile (args !! 1))
      print $ (primaryName (fst result),snd result)
    "mostFrequentYear" -> do
      let y | (args !! 1) == "birthYear" = birthYear
            | (args !! 1) == "deathYear" = deathYear
            | otherwise
            = error $ "Type of year invalid (must be birthYear or deathYear, was: " ++ (args !! 1) ++ ")"
      print $ mostFrequentYear (parseFile (args !! 0)) y
    "mostPopularGenre" -> do
      print $ mostPopularGenre (parseFile (args !! 0))
    "averageRuntime" -> do 
      print $ averageRuntime (parseFile (args !! 0))
    "medianRating" -> do 
      print $ medianRating (parseFile (args !! 0))
    "findName" -> do
      print $ findName (args !! 0) (args !! 1)
    _ -> print $ "No such function: " ++ func

