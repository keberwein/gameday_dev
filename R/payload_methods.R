#' Method for url objects provided from the payload class
#' @param urlz_obj An object returned by \code{playload()}.
#' @param ... additional arguments
#' @keywords internal
#' @export
#' 

get_pload <- function(urlz_obj, ...) UseMethod("get_pload", urlz_obj)

#' @rdname get_pload
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom data.table data.table setnames rbindlist
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @importFrom tidyr fill
#' @importFrom stats na.omit
#' @import foreach
#' @method get_pload inning_all
#' @export

get_pload.inning_all <- function(urlz, ...) {
    # Make some place-holders for the function.
    atbat <- list(); action <- list(); pitch <- list(); runner <- list(); po <- list()
    lnames <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)
    out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=TRUE,
                            .final = function(x) stats::setNames(x, names(lnames)),
                            .init=list(list(), list(), list(), list(), list())) %dopar% {
                                file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
                                if(!isTRUE(is.null(file))){
                                    atbat_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat"), 
                                                     xml2::xml_find_all(file, "./inning/bottom/atbat")) 
                                    action_nodes <- c(xml2::xml_find_all(file, "./inning/top/action"), 
                                                      xml2::xml_find_all(file, "./inning/bottom/action"))
                                    
                                    pitch_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/pitch"),
                                                     xml2::xml_find_all(file, "./inning/bottom/atbat/pitch"))
                                    runner_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/runner"), 
                                                      xml2::xml_find_all(file, "./inning/bottom/atbat/runner")) 
                                    po_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/po"), 
                                                  xml2::xml_find_all(file, "./inning/bottom/atbat/po"))
                                    
                                    url <- urlz[[i]]
                                    
                                    date_dt <- stringr::str_sub(urlz[[i]], 70, 81) %>% stringr::str_replace_all("_", "-") %>%
                                        as.Date(format = "%Y-%m-%d")
                                    gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
                                    
                                    list(                        
                                        atbat <- purrr::map_dfr(atbat_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num"))
                                            out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next"))
                                            out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(x)))
                                            out$url <- url
                                            out$date <- date_dt
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        action <- purrr::map_dfr(action_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num"))
                                            out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next"))
                                            out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(x)))
                                            out$url <- url
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        pitch <- purrr::map_dfr(pitch_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
                                            out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
                                            out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
                                            out$url <- url
                                            out$gameday_link <- gameday_link
                                            out$num <- as.numeric(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
                                            out
                                        }),
                                        
                                        runner <- purrr::map_dfr(runner_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
                                            out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
                                            out$num <- as.character(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
                                            out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
                                            out$url <- url
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        po <- purrr::map_dfr(po_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$inning <- as.numeric( xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
                                            out$next_ <-  as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
                                            out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
                                            out$num <- as.numeric(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
                                            out$url <- url
                                            out$gameday_link <- gameday_link
                                            out
                                        })
                                    )
                                }
                            }
    
    # The foreach loop returns a named list of nested data frames. We need to bind the dfs under 
    # each name and pack the binded dfs back into a list that can be returned.
    atbat <- data.table::rbindlist(out$atbat, fill = T)
    action <- data.table::rbindlist(out$action, fill = T)
    pitch <- data.table::rbindlist(out$pitch, fill = T)
    runner <- data.table::rbindlist(out$runner, fill = T)
    po <- data.table::rbindlist(out$po, fill = T)
    
    # Make of game timeline of atbat and action so we know which atbat to assign an action to.
    acts <- action %>% .[, c("tfs_zulu", "inning", "inning_side", "des")]
    bats <- atbat %>% .[, c("start_tfs_zulu", "num", "inning", "inning_side")] %>% 
        data.table::setnames(old = "start_tfs_zulu", new = "tfs_zulu")
    events <- rbind(acts, bats, fill = T) %>%
        .[order(tfs_zulu)] %>% .[, num := as.numeric(num)] %>%
        tidyr::fill(num, .direction = "up") %>% na.omit()
    
    action <- merge(action, events, by = c("tfs_zulu", "inning", "inning_side", "des"))
    
    # Calculate the pitch count for the pitching table.
    pitch <- pitch_count(dat=pitch)
    
    innings_df <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)
    # Add batter and pitcher names to the atbat data frame
    player.env <- environment()
    data(player_ids, package="mlbgameday", envir=player.env)
    player_ids$id <- as.character(player_ids$id)
    
    innings_df$atbat %<>%  merge(player_ids, by.x = "batter", by.y = "id") %>%
        merge(player_ids, by.x = "pitcher", by.y = "id") %>%
        data.table::setnames(old = c("full_name.x", "full_name.y"), new = c("batter_name", "pitcher_name"))
    
    innings_df <- structure(innings_df, class="list_inning_all") %>%
        transform_pload()
    
    return(innings_df)
}


#' An internal function for bis_boxscore payload.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom data.table data.table rbindlist
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @method get_pload bis_boxscore
#' @export
get_pload.bis_boxscore <- function(urlz, ...) {
    batting <- list(); pitching <- list()
    lnames <- list(batting=batting,pitching=pitching)
    out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=TRUE,
                            .final = function(x) stats::setNames(x, names(lnames)),
                            .init=list(list(), list())) %dopar% {
                                file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
                                if(!isTRUE(is.null(file))){
                                    date_dt <- stringr::str_sub(urlz[[i]], 71, 80) %>% stringr::str_replace_all("_", "-") %>%
                                        as.Date(format = "%Y-%m-%d")
                                    gameday_link <- stringr::str_sub(urlz[[i]], 66, -18)
                                    pitch_nodes <- xml2::xml_find_all(file, "/boxscore/pitching/pitcher")
                                    bat_nodes <- xml2::xml_find_all(file, "/boxscore/batting/batter")
                                    
                                    list(
                                        batting <- purrr::map_dfr(bat_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$date <- date_dt
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        pitching <- purrr::map_dfr(pitch_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$date <- date_dt
                                            out$gameday_link <- gameday_link
                                            out
                                        })
                                    )
                                }
                            }
    batting <- data.table::rbindlist(out$batting, fill = T)
    pitching <- data.table::rbindlist(out$pitching, fill = T)
    
    innings_df <- list(batting=batting, pitching=pitching)
    innings_df <- structure(innings_df, class="list_bis_boxscore") %>%
        transform_pload()
    return(innings_df)
    
}

#' An internal function for game_events paylaod.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @method get_pload game_events
#' @export
#' 
get_pload.game_events <- function(urlz, ...) {
    innings_df <- foreach::foreach(i = seq_along(urlz), .combine="rbind", .multicombine=T, .inorder=TRUE) %dopar% {
        file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
        if(!isTRUE(is.null(file))){
            pitch_nodes <- c(xml2::xml_find_all(file, "/game/inning/top/atbat/pitch"), 
                             xml2::xml_find_all(file, "/game/inning/bottom/atbat/pitch")) 
            
            date_dt <- stringr::str_sub(urlz[[i]], 71, 80) %>% stringr::str_replace_all("_", "-") %>%
                as.Date(format = "%Y-%m-%d")            
            gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
            
            events <- purrr::map_dfr(pitch_nodes, function(x) {
                out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                # An inner-loop would be more elegant here, but this way is faster, so...
                out$num <- xml2::xml_parent(x) %>% xml2::xml_attr("num")
                out$b <- xml2::xml_parent(x) %>% xml2::xml_attr("b")
                out$s <- xml2::xml_parent(x) %>% xml2::xml_attr("s")
                out$o <- xml2::xml_parent(x) %>% xml2::xml_attr("o")
                out$start_tfs <- xml2::xml_parent(x) %>% xml2::xml_attr("start_tfs")
                out$start_tfs_zulu <- xml2::xml_parent(x) %>% xml2::xml_attr("start_tfs_zulu")
                out$batter <- xml2::xml_parent(x) %>% xml2::xml_attr("batter")
                out$pitcher <- xml2::xml_parent(x) %>% xml2::xml_attr("pitcher")
                out$des <- xml2::xml_parent(x) %>% xml2::xml_attr("des")
                out$des_es <- xml2::xml_parent(x) %>% xml2::xml_attr("des_es")
                out$event_num <- xml2::xml_parent(x) %>% xml2::xml_attr("event_num")
                out$event <- xml2::xml_parent(x) %>% xml2::xml_attr("event")
                out$event_es <- xml2::xml_parent(x) %>% xml2::xml_attr("event_es")
                out$play_guid <- xml2::xml_parent(x) %>% xml2::xml_attr("play_guid")
                out$home_team_runs <- xml2::xml_parent(x) %>% xml2::xml_attr("home_team_runs")
                out$away_team_runs <- xml2::xml_parent(x) %>% xml2::xml_attr("away_team_runs")
                out$b1 <- xml2::xml_parent(x) %>% xml2::xml_attr("b1")
                out$b2 <- xml2::xml_parent(x) %>% xml2::xml_attr("b2")
                out$b3 <- xml2::xml_parent(x) %>% xml2::xml_attr("b3")
                out$inning <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num")
                out$inning_side <- xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x)))
                out$date <- date_dt
                out$gameday_link <- gameday_link
                out
            })
        }
    }
    innings_df <- structure(innings_df, class="df_game_events") %>%
        transform_pload()
    
    return(innings_df)
}


#' An internal function for inning_hit payload.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @method get_pload inning_hit
#' @export
#' 
get_pload.inning_hit <- function(urlz, ...) {
    innings_df <- foreach::foreach(i = seq_along(urlz), .combine="rbind", .multicombine=T, .inorder=TRUE) %dopar% {
        file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
        if(!isTRUE(is.null(file))){
            date_dt <- stringr::str_sub(urlz[[1]], 70, 79) %>% stringr::str_replace_all("_", "-") %>%
                as.Date(format = "%Y-%m-%d")
            gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
            hip_nodes <- xml2::xml_find_all(file, "/hitchart/hip")
            game <- purrr::map_dfr(hip_nodes, function(x) {
                out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                out$date <- date_dt
                out$gameday_link <- gameday_link
                out
            })
        }
    }
    innings_df <- structure(innings_df, class="df_inning_hit") %>%
        transform_pload()
    return(innings_df)
}

#' An internal function for linescore payload.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom data.table data.table rbindlist
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @method get_pload linescore
#' @export

get_pload.linescore <- function(urlz, ...) {
    game <- list(); game_media <- list()
    lnames <- list(game=game, game_media=game_media)
    out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=TRUE,
                            .final = function(x) stats::setNames(x, names(lnames)),
                            .init=list(list(), list())) %dopar% {
                                file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
                                if(!isTRUE(is.null(file))){
                                    date_dt <- stringr::str_sub(urlz[[i]], 70, 80) %>% stringr::str_replace_all("_", "-") %>%
                                        as.Date(format = "%Y-%m-%d")
                                    gameday_link <- stringr::str_sub(urlz[[1]], 66, -15)
                                    game_nodes <- xml2::xml_find_all(file, "/game")
                                    media_nodes <- xml2::xml_find_all(file, "/game/game_media/media")
                                    
                                    list(
                                        game <- purrr::map_dfr(game_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out$date <- date_dt
                                            out$gameday_link <- gameday_link
                                            out
                                        }),
                                        
                                        game_media <- purrr::map_dfr(media_nodes, function(x) {
                                            out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                            out
                                        })
                                    )
                                }
                            }
    game <- data.table::rbindlist(out$game, fill = T)
    game_media <- data.table::rbindlist(out$game_media, fill = T)
    innings_df <- list(game=game, game_media=game_media)
    innings_df <- structure(innings_df, class="list_linescore") %>%
        transform_pload()
    return(innings_df)
}

#' An internal function for game payload.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @method get_pload game
#' @export
#' 
get_pload.game <- function(urlz, ...) {
    innings_df <- foreach::foreach(i = seq_along(urlz), .combine="rbind", .multicombine=T, .inorder=TRUE) %dopar% {
        file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
        if(!is.null(file)){
            date_dt <- stringr::str_sub(urlz[[i]], 71, 80) %>% stringr::str_replace_all("_", "-") %>%
                as.Date(format = "%Y-%m-%d")
            gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
            game_nodes <- xml2::xml_find_all(file, "/game/team")
            game <- purrr::map_dfr(game_nodes, function(x) {
                out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                out$type <- xml2::xml_parent(x) %>% xml2::xml_attr("type")
                out$local_game_time <- xml2::xml_parent(x) %>% xml2::xml_attr("local_game_time")
                out$game_pk <- xml2::xml_parent(x) %>% xml2::xml_attr("game_pk")
                out$game_time_et <- xml2::xml_parent(x) %>% xml2::xml_attr("game_time_et")
                out$gameday_sw <- xml2::xml_parent(x) %>% xml2::xml_attr("gameday_sw")
                out$date <- date_dt
                out$gameday_link <- gameday_link
                out
            })
        }
    }
    innings_df <- structure(innings_df, class="df_game") %>%
        transform_pload()
    return(innings_df)
}


