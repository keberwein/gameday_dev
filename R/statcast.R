
#' This is DEV and needs to be transformed into a payload_method.
#' 
#' @param start Date of first game for which you want data. Format must be in YYYY-MM-DD format.
#' @param end Date of last game for which you want data. Format must be in YYYY-MM-DD format.
#' @param player_id The MLBAM ID for the player who's data you want to query.
#' @param player_type The player type. Can be 'batter' or 'pitcher'
#' @importFrom readr read_csv
#' @export
#' @examples
#' \dontrun{
#' scrape_statcast_savant(start = "2016-04-06", end = "2016-04-15", player_id = 621043, player_type='batter')
#'
#' scrape_statcast_savant(start = "2016-04-06", end = "2016-04-15", player_id = 592789, player_type='pitcher')
#'
#' scrape_statcast_savant(start = "2016-04-06", end = "2016-04-06")
#' }

statcast_payload <- function(start, end, player_id=NULL, player_type=NULL) {
    # Check to make sure args are in the correct format.
    if(is.null(player_type)) player_type <- "batter"
    if(!is.character(start_date) | !is.character(end_date)) message("Please wrap your dates in quotations in 'yyyy-mm-dd' format.")
    if(as.Date(start_date)<="2015-03-01") message("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
    if(as.Date(start_date)<="2008-03-25") message("The data are limited to the 2008 MLB season and after.")
    if(as.Date(start_date)==Sys.Date()) message("The data are collected daily at 3 a.m. Some of today's games may not be included.")
    if(as.Date(start_date)>as.Date(end_date)) message("The start date is later than the end date.")

    args <- list(
        start_date <- start_date,
        end_date <- end_date,
        year <- substr(start_date, 1,4),
        player_type <- player_type,
        playerid <- playerid) %>% purrr::set_names("start_date", "end_date", "year", "player_type", "playerid")
    
    for(i in seq_along(args)){
        if(is.null(args[[i]])) args[[i]] <- as.character("")
    }
    
    base_url <- "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea="
    elem1 <- "%7C&hfSit=&player_type="
    elem2 <- "&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt="
    elem3 <- "&game_date_lt="
    elem4 <- "&team=&position=&hfRO=&home_road=&"
    ifelse(!is.null(playerid), playerelem <- paste0(player_type, "s_", "batters_lookup%5B%5D="), playerelem <- "")
    elem5 <- "&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&"
    
    url <- paste0(base_url, args$year, elem1, player_type, elem2,
                  args$start_date, elem3, args$end_date, elem4, args$playerid, elem5)

    # New version of the below. Need to rename payload since we've got a class named that.
    #out <- checkurl(read_csv(url))
    #
    gamedaydev::checkurl(url)
    
    if(isTRUE(checkurl(url))) out <- readr::read_csv(url)
    else message("Could not execute query. Please check your connection or try another query.")
}
