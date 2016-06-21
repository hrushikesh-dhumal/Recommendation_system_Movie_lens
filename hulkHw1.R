# Read data
temp <- list.files(pattern="u.*[.]test",full.names=T, ignore.case = T, recursive = T);
test.master <- lapply(temp, read.table);

# Initialize the number of maximum users(m) and movies(n)
m = 943; #Change to test for less number of users
n = 1682;

# Initialize the error matrices
mad.euclidean = mad.manhattan = mad.lmax = rep(0, 5);

for (idx in 1:5) #5 fold cross-validation
{
  train = rbindlist(test.master[c(1:5)[-idx]]);
  colnames(train) = c("user_id", "item_id", "rating", "timestamp");
  test = test.master[[idx]];
  colnames(test) = c("user_id", "item_id", "rating", "timestamp");
  
  # User rating for movies
  r <-  foreach(i=1:m, .combine='rbind') %:% 
    foreach(j=1:n, .combine='cbind') %dopar% 
    {
      u.m = train[(train$user_id == i & train$item_id == j),];
      u.m$rating[order(u.m$timestamp)][1];
    }

  # Distance matrices using upper triangular indices
  dist.euclidean = matrix(rep(0, m*m), m);
  dist.manhattan = matrix(rep(0, m*m), m);
  dist.lmax = matrix(rep(0, m*m), m);
  d = c(0,0,0);
  for (i in 1:(m-1))
  {
    for (j in (i+1):(m-1))
    {
      d = myDist(r[i,], r[j,]);
      dist.euclidean[j, i] = dist.euclidean[i, j] = d[1];
      dist.manhattan[j, i] = dist.manhattan[i, j] = d[2];
      dist.lmax[j, i] = dist.lmax[i, j] = d[3];
    }
  }
  
  # Most similar users for each user 
  best.match.count = 4;
  u.best.match.euclidean = matrix(rep(0, m * best.match.count), m);
  u.best.match.manhattan = matrix(rep(0, m * best.match.count), m);
  u.best.match.lmax = matrix(rep(0, m * best.match.count), m);
  for (i in 1:m)
  {
    u.best.match.euclidean[i,] = order(dist.euclidean[i,-i])[1:best.match.count] + 1;
    u.best.match.manhattan[i,] = order(dist.manhattan[i,-i])[1:best.match.count] + 1;
    u.best.match.lmax[i,] = order(dist.lmax[i,-i])[1:best.match.count] + 1;
  }
  
  #Predictions for all users(i) and all movies(j)
  # Predictions based on Euclidean distance
  p.euclidean <- foreach(i = 1:m, .combine='rbind') %:% 
    foreach(j = 1:n, .combine = 'cbind') %dopar%
    {
      if (is.na(r[i, j])) #If the user has not already rated. 
      {
        return(getPrediction(r, i, j, u.best.match.euclidean[i,]));
      }
      else #User has rated the movie
      {
        return(r[i,j]);
      }
    }
  # Predictions based on Manhattan distance
  p.manhattan <- foreach(i = 1:m, .combine='rbind') %:% 
    foreach(j = 1:n, .combine = 'cbind') %dopar%
    {
      if (is.na(r[i, j])) #If the user has not already rated. 
      {
        return(getPrediction(r, i, j, u.best.match.manhattan[i,]));
      }
      else #User has rated the movie
      {
        return(r[i,j]);
      }
    }
  
  # Predictions based on Lmax distance
  p.lmax <- foreach(i = 1:m, .combine='rbind') %:% 
    foreach(j = 1:n, .combine = 'cbind') %dopar%
    {
      if (is.na(r[i, j])) #If the user has not already rated. 
      {
        return(getPrediction(r, i, j, u.best.match.lmax[i,]));
      }
      else #User has rated the movie
      {
        return(r[i,j]);
      }
    }
  
  #Write predictions to disk
  write.table(p.euclidean, paste("predictions/euclidean_",idx), sep = "\t", row.names = F, na = "", col.names = F);
  write.table(p.euclidean, paste("predictions/manhattan_",idx), sep = "\t", row.names = F, na = "", col.names = F);
  write.table(p.euclidean, paste("predictions/lmax_",idx), sep = "\t", row.names = F, na = "", col.names = F);
  
  # Errors on test data
  mad.euclidean[idx] = getError(p.euclidean, test);
  mad.manhattan[idx] = getError(p.manhattan, test);
  mad.lmax[idx] = getError(p.lmax, test);
}   


stopCluster(cl); #Stop cluster
