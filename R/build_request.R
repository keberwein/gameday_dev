#' Method for url objects provided from the payload class
#' @param args An object returned by \code{get_payload()}.
#' @param ... additional arguments
#' @keywords internal
#' @export

build_request <- function(args, ...) UseMethod("build_request", args)

#' @rdname build_request
#' @method build_request pitchfx
#' @export

build_request.pitchfx <- function(args=NULL, ...) {
    # Reuse the make_gids function here instead of deprecating it since it's still useful as a util.
    if(is.null(args$game_ids)) urlz <- make_gids(start = args$start, end = args$end, dataset = args$dataset)
    
    if(!is.null(args$game_ids)) urlz <- make_gids(game_ids = args$game_ids, dataset = args$dataset)
    urlz <- structure(urlz, class= c(args$dataset, "pitchfx"))
    
    return(urlz)
}

build_request.statcast <- function(args=NULL, ...) {
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
    
    urlz <- paste0(base_url, args$year, elem1, player_type, elem2,
                  args$start, elem3, args$end, elem4, playerelem, args$player_id, elem5)
    
    urlz <- structure(urlz, class= c(args$dataset, "statcast"))
    
    return(urlz)
}