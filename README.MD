This repo contains my recommendation system for movie data base from [Movielens](http://grouplens.org/datasets/movielens/) in R. The code was created to run on Sharks Domain[hulk.soic.indiana.edu] - Hulk is a quad-socket, 8-core (32 total cores) AMD Opteron system with 512GB of memory running 64-bit Red Hat Enterprise Linux. This code can be run on a normal machine by modifying the number of cores.

The code needs following R packages:
- [doParallel] (https://cran.r-project.org/web/packages/doParallel/index.html) - for parallel implementation.
- [do.table] (https://cran.r-project.org/web/packages/data.table/index.html) - for fast file read.

# Recommendation system
-  For each user i and each movie j they did not see, the system finds the top k most similar users who have seen j and then use them to infer the user i’s rating on movie j. 
- The performance of system is measured using cross-validation. For each data set, the MovieLens database already provides a split of the initial data set into N = 5 folds. This means algorithm runs N times; in each step, use the training partition to make predictions for each user on all terms rated in the test partition (by that user). When all N iterations are complete, a large number of user-movie pairs from the 5 test partitions are used to evaluate the performance of system.
- Measure the performance of your recommendation system using the mean absolute difference (MAD).
- Used “100K Dataset” to evaluate three different distance metrics: the Euclidean distance, the Manhattan distance and the L max distance using the entire vectors of ratings over all movies. 
