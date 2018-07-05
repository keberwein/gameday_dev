#' Get Gameday data from MLBAM.
#' @param start A start date passed as a character in ISO 8601 format. \code{"2017-05-01"}
#' @param end An end date passed as a character in ISO 8601 format. \code{"2017-09-01"}
#' @param league The league to gather gids for. The default is \code{"mlb"}. Other options include \code{"aaa"} and \code{"aa"}
#' @param dataset The dataset to be scraped. The default is "inning_all." Other options include, "inning_hit", "linescore".
#' @param game_ids A list of user-supplied gameIds.
#' @param db_con A database connection from the \code{DBI} package.
#' @param overwrite Logical. Should current database be overwritten? Inherited from the \code{dbWriteTable} function from the \code{DBI} package.
#' The default value is FALSE.
#' @param ... additional arguments
#' @importFrom DBI dbWriteTable
#' @import utils
#' @export
#' @examples
#' 
#' \dontrun{
#' # Make a request for a single day.
#' df <- get_payload(start = "2016-06-01", end = "2016-06-01")
#' 
#' 
#' # Run larger requests in parallel.
#' library(doParallel)
#' library(foreach)
#' 
#' no_cores <- detectCores() - 2
#' cl <- makeCluster(no_cores) 
#' registerDoParallel(cl)
#' 
#' df <- get_payload(start = "2016-01-01", end = "2017-01-01")
#' 
#' stopImplicitCluster()
#' rm(cl)
#' 
#' }
#'
#' # Supply your own custom vector of game ids.
#' 
#' mygids <- search_gids(team = "indians", start = "2016-05-01", end = "2016-05-01")
#' 
#' df <- get_payload(game_ids = mygids)
#' 
#' 

get_payload <- function(start=NULL, end=NULL, league="mlb", source="statcast",
                        dataset = NULL, game_ids = NULL, db_con = NULL, overwrite = FALSE, ...){
    args <- list(start=start, end=end, league=league, source=source, 
                 dataset=dataset, game_ids=game_ids, db_con=db_con, overwrite=overwrite)
    # Set query class, so we know what to do with the request.
    ifelse(source==tolower("pitchfx"), args <- structure(args, class="pitchfx"), args <- structure(args, class="statcast"))
    innings_df <- payload(args)
    return(innings_df)
}

#' Method for get_payload objects.
#' @param args An object returned by \code{get_payload()}.
#' @param ... additional arguments
#' @keywords internal
#' @export

payload <- function(args, ...) UseMethod("payload", args)

#' @rdname payload
#' @importFrom data.table data.table
#' @method payload statcast
#' @export

payload.statcast <- function(args, ...) {
    if(is.null(args$player_type)) args$player_type <- "batter"
    if(!is.character(args$start) | !is.character(args$end)) message("Please wrap your dates in quotations in 'yyyy-mm-dd' format.")
    if(as.Date(args$start)<="2015-03-01") message("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
    if(as.Date(args$start)<="2008-03-25") message("The data are limited to the 2008 MLB season and after.")
    if(as.Date(args$start)==Sys.Date()) message("The data are collected daily at 3 a.m. Some of today's games may not be included.")
    if(as.Date(args$start)>as.Date(args$end)) message("The start date is later than the end date.")
    
    urlz <- build_request(args)
    
    if(isTRUE(checkurl(url))) out <- data.table::fread(url, data.table=FALSE)
    else message("Could not execute query. Please check your connection or try another query.")
    
}

# Main method for pitchfx data.
#' @rdname payload
#' @importFrom data.table data.table
#' @method payload pitchfx
#' @export

payload.pitchfx <- function(args, ...) {
    if(is.null(args$dataset)) args$dataset <- "inning_all"
    message("Gathering Gameday data, please be patient...")
    
    if(!is.null(args$start) & !is.null(args$end)){
        if(args$start < as.Date("2008-01-01")){
            stop("Please select a later start date. The data are not dependable prior to 2008.")
        }
        if(args$end >= Sys.Date()) stop("Please select an earlier end date.")
        if(args$start > args$end) stop("Your start date appears to occur after your end date.")
    }

    urlz <- build_request(args)
    
    if(!is.null(args$db_con)){
        # Chunk out URLs in groups of 300 if a database connection is available.
        url_chunks <- split(urlz, ceiling(seq_along(urlz)/500))
        innings_df=NULL
        
        for(i in seq_along(url_chunks)){
            message(paste0("Processing data chunk ", i, " of ", length(url_chunks)))
            urlz <- unlist(url_chunks[i])
            # inning_all and linescore contain multiple tables, so those need to be written in a loop.
            if(args$dataset == "inning_all" | args$dataset=="linescore"){
                if(args$dataset == "inning_all") innings_df <- payload.gd_inning_all(urlz)
                if(args$dataset=="linescore") innings_df <- payload.gd_linescore(urlz)
                
                if(isTRUE(args$overwrite)){
                    for (i in names(innings_df)) DBI::dbWriteTable(conn = args$db_con, value = innings_df[[i]], name = i, overwrite = TRUE)
                }
                if(!isTRUE(args$overwrite)){
                    for (i in names(innings_df)) DBI::dbWriteTable(conn = args$db_con, value = innings_df[[i]], name = i, append = TRUE)
                }
                
            } else {
                innings_df <- get_pload(urlz)
                if(isTRUE(args$overwrite)) DBI::dbWriteTable(conn = args$db_con, value = innings_df, name = args$dataset, overwrite = TRUE)
                if(!isTRUE(args$overwrite)) DBI::dbWriteTable(conn = args$db_con, value = innings_df, name = args$dataset, append = TRUE)
            }
            
            # Manual garbage collect after every loop of 500 games.
            rm(innings_df); gc()
        }
        
        DBI::dbDisconnect(args$db_con)
        message(paste0("Transaction complete, disconnecting from the database.", " ", Sys.time()))
    }
    
    if(is.null(args$db_con)){
        # If no database connection, just return a dataframe.
        # If the returned dataframe looks like it's going to be large, warn the user.
        if(length(urlz) > 3500) { # One full season including spring training and playoffs is around 3000 games.
            if(utils::menu(c("Yes", "No"), 
                           title="Woah, that's a lot of data! Are you sure you want to continue without a database connection?")!=1){
                stop(message("Download stopped. Try a database connection or a smaller data set."))
            }else{ 
                message("Starting download, this may take a while...") 
            }
        }
        
        innings_df <- get_pload(urlz)
        
        return(innings_df)
    }
}