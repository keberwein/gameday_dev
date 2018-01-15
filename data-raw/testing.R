library(gamedaydev)

innings_df <- get_payload(start = "2016-08-03", end = "2016-08-04")

action <- innings_df$action %>% subset(gameday_link == "/gid_2016_08_04_chamlb_detmlb_1")

# disabled the transform_payload function at the end of innings_all method.
# Seems that the $ add-ons on the dataframes are mis-matching lines. 
# Action returns correct if all $ are commented out, but mis-matches when they are present.




#Matching event numbers are under pitch and action. Pitch is a child of atbat, maybe join like that?
# I'm already grabbing num from atbat and inserting it into the pitch table--could probably join pitch and action on event_num and grab num from there.
# 

# Every runner at an AB "num", there are some double when there was more than one runner--that's OK.

# This creates a one to one match on event_num and AB num.
z= select(runner, num, event_num, gameday_link)


### Looks like event_nums are getting crossed up on the xml piece of the action dataframe. In fact, just about everything in action is wrong.
### Need to to a long test of a single game.


# Need to join the matches to action.
test <- dplyr::left_join(action, z, by = c("event_num", "gameday_link"))