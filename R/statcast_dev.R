# TODO: Re-write params and examples
# Prot code.


#' This is DEV and needs to be transformed into a payload_method.
#' 
#' @param start_date Date of first game for which you want data. Format must be in YYYY-MM-DD format.
#' @param end_date Date of last game for which you want data. Format must be in YYYY-MM-DD format.
#' @param playerid The MLBAM ID for the player who's data you want to query.
#' @param player_type The player type. Can be 'batter' or 'pitcher'
#' @importFrom readr read_csv
#' @export
#' @examples
#' \dontrun{
#' scrape_statcast_savant(start_date = "2016-04-06", end_date = "2016-04-15", playerid = 621043, player_type='batter')
#'
#' scrape_statcast_savant(start_date = "2016-04-06", end_date = "2016-04-15", playerid = 592789, player_type='pitcher')
#'
#' scrape_statcast_savant(start_date = "2016-04-06", end_date = "2016-04-06")
#' }

scrape_statcast_savant <- function(start_date, end_date, playerid=NULL, player_type=NULL) {
    # Check to make sure args are in the correct format.
    if(!is.character(start_date) | !is.character(end_date)) message("Please wrap your dates in quotations in 'yyyy-mm-dd' format.")
    if(as.Date(start_date)<="2015-03-01") message("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
    if(as.Date(start_date)<="2008-03-25") message("The data are limited to the 2008 MLB season and after.")
    if(as.Date(start_date)==Sys.Date()) message("The data are collected daily at 3 a.m. Some of today's games may not be included.")
    if(as.Date(start_date)>as.Date(end_date)) message("The start date is later than the end date.")
    #if(is.null(plyaer_type)) plyaer_type <- ""
    #if(is.null(playerid)) plyaer_type <- ""
    
    # extract season from start_date
    
    year <- substr(start_date, 1,4)
    
    # Looks like the url parsing could be much cleaner.
    # The else if statements are obnoxious, could probably replace with a single case statement.
    
    base_url <- "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea="
    elem1 <- "%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt="
    elem2 <- "&game_date_lt="
    elem3 <- "&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&"
    elem4 <- "&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&"
    
    ifelse(is.null(playerid) & is.null(player_type), url <- paste0(base_url, year, elem1, start_date, elem2, end_date, elem3), 
           ifelse(!is.null(playerid) & is.null(player_type), url <- paste0(base_url, year, elem1, start_date, elem2, end_date, elem3, playerid, elem4), 
                  ifelse(!is.null(playerid) & player_type=='batter', url <- paste0(base_url, year, elem1, player_type, start_date, elem2, end_date, elem3, playerid, elem4), 
                         ifelse(!is.null(playerid) & player_type=='pitcher', url <- paste0(base_url, year, elem1, player_type, start_date, elem2, end_date, elem3, playerid, elem4),
                                ifelse(is.null(playerid) & player_type=='pitcher', url <- base_url, year, elem1, start_date, elem2, end_date, elem3,
                                       url <- base_url, year, elem1, start_date, elem2, end_date, elem3)))))
    
    
    if(is.null(playerid) & is.null(player_type)) {
        message("No player_type specified. Player_type will default to 'batter'.")
        message("No playerid specified. Collecting data for all batters.")
        url <- paste0(base_url, 
                      year, "%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=", 
                      start_date, "&game_date_lt=", 
                      end_date, "&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
        
    } else if (!is.null(playerid) & is.null(player_type)) {
        message("No player_type specified. Player_type will default to 'batter'.")
        url <- paste0(base_url, 
                      year, "%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=", 
                      start_date, "&game_date_lt=", 
                      end_date, "&team=&position=&hfRO=&home_road=&batters_lookup%5B%5D=", 
                      playerid, "&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
        
    } else if (!is.null(playerid) & player_type=='batter') {
        url <- paste0(base_url, 
                      year, "%7C&hfSit=&player_type=", 
                      player_type, "&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=", 
                      start_date, "&game_date_lt=", 
                      end_date, "&team=&position=&hfRO=&home_road=&batters_lookup%5B%5D=", 
                      playerid, "&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
        
    } else if (!is.null(playerid) & player_type=='pitcher') {
        url <- paste0(base_url, 
                      year, "%7C&hfSit=&player_type=", 
                      player_type, "&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=", 
                      start_date, "&game_date_lt=", 
                      end_date, "&team=&position=&hfRO=&home_road=&pitchers_lookup%5B%5D=", 
                      playerid, "&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
        
    } else if (is.null(playerid) & player_type=='pitcher'){
        message("Collecting data for all pitchers for dates specified.")
        url <- paste0(base_url, 
                      year, "%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=", 
                      start_date, "&game_date_lt=", 
                      end_date, "&team=&position=&hfRO=&home_road=&&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
        
    } else {
        message("Collecting data for all batters for dates specified.")
        url <- paste0(base_url, 
                      year, "%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=", 
                      start_date, "&game_date_lt=", 
                      end_date, "&team=&position=&hfRO=&home_road=&&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
    }
    
   
    
    # New version of the below. Need to rename payload since we've got a class named that.
    #out <- try_url(read_csv(url))
    
    
    tryCatch(
        {
            print("These data are from BaseballSevant and are property of MLB Advanced Media, L.P. All rights reserved.")
            print("Grabbing data, this may take a minute...")
            payload <- read_csv(url)
            processed_payload <- process_statcast_payload(payload)
            message("URL read and payload acquired successfully.")
            return(processed_payload)
        },
        error=function(cond) {
            message(paste("URL does not seem to exist, please check your Internet connection:"))
            message("Original error message:")
            message(cond)
            return(NA)
        },
        message=function(cond) {
            message(paste("URL caused a message. Make sure your playerid, player_type, and date range are correct:"))
            message("Original message message:")
            message(cond)
            return(NULL)
        }
    )
}


#z <- scrape_statcast_savant(start_date = "2016-04-06", end_date = "2016-04-15", playerid = 621043, player_type='batter')