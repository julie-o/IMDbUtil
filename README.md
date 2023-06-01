# IMDbUtil - Supporting Introductory Computing Education with Data Manipulation
The purpose of the IMDbUtil library is to enable the use of data in introductory computing courses, specifically with Haskell. IMDbUtil can be used with the IMDb dataset, which is freely available for personal use (https://www.imdb.com/interfaces/). The main library file contains functions for parsing, exploring and printing the data. Additionally, this repository contains the file used for creating subsets of the data and a file with example queries.
## IMDbUtil.hs
This is the main library that can be used to read, manipulate and explore IMDb data using Haskell. More information on each function will be added shortly.
## queries.hs
This file contains a number of example queries using IMDbUtil. The queries can be run in GHCI, but the file also contains a main function which makes the queries runnable directly from command line. This is done by running the compiled file with the name of the query and the arguments to the query as arguments to the executable.
```haskell
./compiledQueries queryName arg1 arg2 ...
```
Query parameters are usually files. 
## filtering.hs
This file contains code used for filtering the original IMDb dataset into smaller, more manageable datasets. The size of the full size IMDb dataset is unrealistic for use in an education setting, as running even simple queries is very time consuming. The filtering itself is time consuming, hence filtered datasets (from a dataset retrieved 16/10/2022) have been provided in the **data** folder.

To create a subset, the *n* most popular titles have been included. Since the *n* most popular titles determine the contents of the dataset, this is referred to as the *size* of the dataset. The "most popular" titles has been determined based on the number votes the titles have in *ratings.tsv*. All files (except names.tsv) are filtered to only contain these titles. Additionally, *names.tsv* only contains names that appear in *principals.tsv*.
## /data 
The data folder contains 15 compressed datasets, and the size is indicated by the name of the folder. The file names correspond as follows to the files found on IMDb:

| **File** | **Original IMDb file**   |
|----------------|----------------------|
| titles.tsv     | title.basics.tsv     |
| crew.tsv       | title.crew.tsv       |
| principals.tsv | title.principals.tsv |
| ratings.tsv    | title.ratings.tsv    |
| names.tsv      | name.basics.tsv      |
The internal structures of the files are the same as the original files.