library(mlbgameday)
library(dplyr)
library(DBI)
library(RSQLite)
library(doParallel)



con <- dbConnect(RSQLite::SQLite(), dbname = "gameday.sqlite3")

library(pitchRx)

scrape(start = "2016-08-03", end = "2016-08-03", con = con)


no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

get_payload(start = "2016-08-04", end = "2016-08-04", db_con=con)

stopImplicitCluster()
rm(cl)



z <- dbGetQuery(con,"SELECT * FROM po limit 5")




library(pitchRx)

cdatt <- scrape(start = "2016-08-03", end = "2016-08-03")

innings <- get_payload(start = "2016-08-04", end = "2016-08-04")


atbat = innings$atbat
action = innings$action
pitch = innings$pitch
runner = innings$runner
po = innings$po


catbat = cdatt$atbat
caction = cdatt$action
cpitch = cdatt$pitch
crunner = cdatt$runner
cpo = cdatt$po

