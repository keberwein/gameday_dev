z = statcast_payload(start = "2016-04-06", end = "2016-04-15", player_id = 592789, player_type='pitcher')

z = statcast_payload(start = "2016-04-06", end = "2016-04-06")



# game_pk column now added to gidz. This should be used in place of the game_ids arg to get this funciton to work with get_payload.
# 
# game_pk is in returned data. Going to have to pull all the data for those dates and take care of it in transform_methods.
gidz <- gamedaydev::game_ids

                          
                          
jz = jsonlite::fromJSON("http://statsapi.mlb.com/api/v1/game/446893/feed/color")

zz = jz$items$data

# Index playbyplay??
groups <- jz$items$group
idx <- which(groups=="descriptioin")

# Probably going to have to spilt this into two dfs, batther and pitcher.
# TODO Take a look at statcast columns from savant and make sure we've got all the right columns.



for (i in idx){
    print(zz$data[[i]])
}

atbat <- purrr::map_dfr(idx, function(x) {
    out <- zz$data[[x]]
    #if(out$group == "playByPlay") print(out$data)
    #out <- zz$data
    out
})