library(mlbgameday)
library(dplyr)
library(DBI)
library(RSQLite)


con <- dbConnect(RSQLite::SQLite(), dbname = "gameday.sqlite3")

library(pitchRx)

scrape(start = "2016-08-03", end = "2016-08-03", con = con)

get_payload(start = "2016-08-04", end = "2016-08-04", db_con=con)

dates <- dbGetQuery(con,"SELECT * FROM runner")


library(pitchRx)

cdatt <- scrape(start = "2016-08-03", end = "2016-08-03")

innings <- get_payload(start = "2016-08-03", end = "2016-08-03")


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

