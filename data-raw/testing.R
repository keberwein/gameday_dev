library(gamedaydev)
library(dplyr)

innings_df <- get_payload(start = "2016-08-03", end = "2016-08-04")

action <- innings_df$action
runner <- innings_df$runner

# Every runner at an AB "num", there are some double when there was more than one runner--that's OK.


# Looks like the flow is, action -> atbat, runner.
# 
# Matching event_num for runner and actioni won't work. Look at actioin and runner at the top of this xml.
# http://gd2.mlb.com/components/game/mlb/year_2016/month_08/day_04/gid_2016_08_04_chamlb_detmlb_1/inning/inning_all.xml


z= select(runner, num, event_num, gameday_link)



# Need to join the matches to action.
test <- dplyr::left_join(action, z, by = c("event_num", "gameday_link"))