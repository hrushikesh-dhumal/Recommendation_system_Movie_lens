myDist <- function(v1, v2)
{
  # Function to calculate Euclidean, Manhattan and maximum distance between two vectors.
  # Input:  v1 : Vector contaning NA values
  #         v2 : Vector contaning NA values
  # Output: Vector containg 3 distance vales. If all the inputs are NA then NA will be returned.
  
  val = v1-v2;
  if (all(is.na(val)))
  {
    return(c(NA, NA, NA));
  }
  else
  {
    c(sqrt(sum(val^2, na.rm = T)),sum(abs(v1-v2), na.rm = T),max(abs(v1-v2), na.rm = T));
  }
}

getPrediction = function (r, i, j, u.best.match){
  # Function to predict rating for movie(j) by a user(i) based on similar users.
  # Input:    r : matrix containing the rating given to movies by user
  #           i : user id
  #           j : movie id
  #u.best.match : vector containing ids of best matching users  
  # Output: Numeric containing prediction
  
  #If any of matching users have rated the movie
  u.best.match.rate = r[u.best.match, j];
  p = 0;
  if (!all(is.na(u.best.match.rate)))
  {
    #Prediction is the average of the ratings by matching users
    p = mean(u.best.match.rate, na.rm = T);
  }
  #If none of the matching users has rated the movie 
  else
  {
    # If the movie is rated by any other users
    if (!all(is.na(r[, j])))
    {
      # Average the rating by all other users to the movie
      p = mean(r[, j], na.rm = T);
    }
    #If no user rated the movie
    else
    {
      # give it the average of ratingsgiven to a movie by a user
      p = mean(r[i, ], na.rm = T);
    }
  }
  return(p);
}

getError = function(p, test){
  error <-  foreach(i = 1:dim(test)[1]) %dopar%
  {
    tryCatch({
      abs(p[test$user_id[i], test$item_id[i]] -  test$rating[i]);
    }, error = function(e){return(0)});
  }
  # error;
  sum(unlist(error)) / dim(test)[1];
}