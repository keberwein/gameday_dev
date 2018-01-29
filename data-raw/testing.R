library(mlbgameday)
library(dplyr)

innings_df <- get_payload(start = "2016-08-03", end = "2016-08-03")

atbat <- innings_df$atbat
action <- innings_df$action
pitch <- innings_df$pitch
po <- innings_df$po
runner <- innings_df$runner


library(pitchRx)

cdat <- scrape(start = "2016-08-03", end = "2016-08-03")

cabtat <- cdat$atbat
cactioin <- cdat$action
cpitch <- cdat$pitch
cpo <- cdat$po 
crunner <- cdat$runner

# Need to add num to po.

