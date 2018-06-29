#' This is DEV and needs to be transformed into a payload_method
#' @param start A start date passed as a character in ISO 8601 format. \code{"2017-05-01"}
#' @param end An end date passed as a character in ISO 8601 format. \code{"2017-09-01"}
#' @param player_id The MLBAM ID of a specific player.
#' @param player_type The role of the player as a string. The default is "batter". \code{"batter"} or \code{"pitcher"}
#' @importFrom purrr set_names
#' @importFrom data.table fread
#' @export
#' @examples
#' \dontrun{
#' statcast_payload(start = "2016-04-06", end = "2016-04-15", player_id = 621043, player_type='batter')
#'
#' statcast_payload(start = "2016-04-06", end = "2016-04-15", player_id = 592789, player_type='pitcher')
#'
#' statcast_payload(start = "2016-04-06", end = "2016-04-06")
#' }

statcast_payload <- function(start, end, player_id=NULL, player_type=NULL) {
    # Check to make sure args are in the correct format.
    if(is.null(player_type)) player_type <- "batter"
    if(!is.character(start) | !is.character(end)) message("Please wrap your dates in quotations in 'yyyy-mm-dd' format.")
    if(as.Date(start)<="2015-03-01") message("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
    if(as.Date(start)<="2008-03-25") message("The data are limited to the 2008 MLB season and after.")
    if(as.Date(start)==Sys.Date()) message("The data are collected daily at 3 a.m. Some of today's games may not be included.")
    if(as.Date(start)>as.Date(end)) message("The start date is later than the end date.")

    args <- list(
        start <- start,
        end <- end,
        year <- substr(start, 1,4),
        player_type <- player_type,
        player_id <- player_id) %>% purrr::set_names("start", "end", "year", "player_type", "player_id")
    
    for(i in seq_along(args)){
        if(is.null(args[[i]])) args[[i]] <- as.character("")
    }
    
    base_url <- "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea="
    elem1 <- "%7C&hfSit=&player_type="
    elem2 <- "&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt="
    elem3 <- "&game_date_lt="
    elem4 <- "&team=&position=&hfRO=&home_road=&"
    ifelse(!is.null(player_id), playerelem <- paste0("s_", "batters_lookup%5B%5D="), playerelem <- "")
    elem5 <- "&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&"
    
    url <- paste0(base_url, args$year, elem1, player_type, elem2,
                  args$start, elem3, args$end, elem4, playerelem, args$player_id, elem5)

    if(isTRUE(checkurl(url))) out <- data.table::fread(url, data.table=FALSE)
    else message("Could not execute query. Please check your connection or try another query.")
}

