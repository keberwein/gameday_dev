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




urlz <- "http://gd2.mlb.com/components/game/mlb/year_2016/month_08/day_04/gid_2016_08_04_chamlb_detmlb_1/inning/inning_all.xml"

file <- tryCatch(xml2::read_xml(urlz[[1]][[1]], n=256), error=function(e) NULL)

# This is working.
atbat_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat"), 
                 xml2::xml_find_all(file, "./inning/bottom/atbat")) 
# Not getting the correct inning count from this...
action_nodes <- c(xml2::xml_find_all(file, "./inning/top/action"), 
                  xml2::xml_find_all(file, "./inning/bottom/actioin"))
# But when I do this it works...should be a list of 14.
act_top = xml2::xml_find_all(file, "./inning/top/action")
act_bot = xml2::xml_find_all(file, "./inning/bottom/action")
action_nodes <- c(act_top, act_bot)

for (i in seq_along(action_nodes)) {
    xml_add_child(atbat_nodes[[i]], action_nodes[[i]], .where = "after", free = T)
}

action_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/action"), 
                  xml2::xml_find_all(file, "./inning/bottom/atbat/actioin"))



action <- purrr::map_dfr(action_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
    out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
    out$num <- as.numeric(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
    out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
    #out$url <- url
    #out$gameday_link <- gameday_link
    out
})



library(pitchRx)

cdat <- scrape(game.ids = "gid_2016_08_04_chamlb_detmlb_1")

cactioin <- cdat$action
