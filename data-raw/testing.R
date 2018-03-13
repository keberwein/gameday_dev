library(doParallel)
library(DBI)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), dbname = "gameday.sqlite3")

# First we need to register our parallel cluster.
# Set the number of cores to use as the machine's maximum number of cores minus 1 for background processes.
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

#linescore <- get_payload(start = "2017-01-01", end = "2018-01-01", dataset = "linescore")

get_payload(start = "2017-01-01", end = "2018-01-01", dataset = "linescore", db_con = con)


stopImplicitCluster()
rm(cl)

