library(doParallel)
library(DBI)
library(RSQLite)

# First we need to register our parallel cluster.
# Set the number of cores to use as the machine's maximum number of cores minus 1 for background processes.
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

# Then run the get_payload function as normal.

s = Sys.time()

innings_df <- get_payload(start = "2017-01-01", end = "2018-01-01")

f = s - Sys.time()

# Don't forget to stop the cluster when finished.
stopImplicitCluster()
rm(cl)


gidslist <- make_gids(start = "2017-08-03", end = "2017-09-04") 
