

library(jsonlite)
library(dplyr)
library(purrr)
# url with some information about project in Andalussia
url <- 'http://statsapi.mlb.com/api/v1/game/529572/playByPlay.json'

cont = jsonlite::fromJSON(url, flatten = T)
allplays = cont$allPlays

playevents <- purrr::map_dfr(seq_along(allplays$playEvents), function(x) {
    out <- data.frame(allplays$playEvents[[x]])
    #out$inning <- allplays$about.inning
    out
})



for(p in seq_along(allplays)){
    #print(cont$allPlays$result.event)
    playevents <- purrr::map_dfr(seq_along(allplays$playEvents), function(x) {
        out <- data.frame(allplays$playEvents[[x]])
        out$result.type <- allplays$result.type[[x]]
        out$result.event <- allplays$result.event[[x]]
        out$result.typeType <- allplays$result.eventType[[x]]
        out$result.description <- allplays$result.description[[x]][[1]]
        out$result.rbi <- allplays$result.rbi[[x]]
        out$result.awayScore <- allplays$result.awayScore[[x]]
        out$result.homeScore <- allplays$result.homeScore[[x]]
        out$about.atBatIndex <- allplays$about.atBatIndex[[x]]
        out$about.halfInning <- allplays$about.halfInning[[x]]
        out$about.inning <- allplays$about.inning[[x]]
        out$about.startTime <- allplays$about.startTime[[x]]
        out$about.endTime <- allplays$about.endTime[[x]]
        out$about.isComplete <- allplays$about.isComplete[[x]]
        out$about.isScoringPlay <- allplays$about.isScoringPlay[[x]]
        out$about.hasReview <- allplays$about.hasReview[[x]]
        out$about.hasOut <- allplays$about.hasOut[[x]]
        out$about.captivatingIndex <- allplays$about.captivatingIndex[[x]]
        out$matchup.batterId <- allplays$matchup.batter.id[[x]]
        out$matchup.batterFullName <- allplays$matchup.batter.fullName[[x]]
        out$matchup.batSideCode <- allplays$matchup.batSide.code[[x]]
        out$matchup.batSideDes <- allplays$matchup.batSide.description[[x]]
        out$matchup.pitcherId <- allplays$matchup.pitcher.id[[x]]
        out$matchup.pitcherFullName <- allplays$matchup.pitcher.fullName[[x]]
        out$matchup.pitchHandCode <- allplays$matchup.pitchHand.code[[x]]
        out$matchup.pitchHandDes <- allplays$matchup.pitchHand.description[[x]]
        out
    }) 
}


z = filter(playevents, !is.na(hitData.location))


