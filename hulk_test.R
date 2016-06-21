# dataPath = "/l/b565/ml-100k";
# 
# setwd(dataPath);
# temp <- list.files(pattern="u.*[.]test",full.names=T, ignore.case = T, recursive = T);
# 

source("myFunctions.R");

library(data.table)

require(doParallel);
cl <- makeCluster(64);
registerDoParallel(cl);
