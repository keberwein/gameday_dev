grab_bb <- function(id) {
    feed <- sprintf("http://statsapi.mlb.com/api/v1/game/%s/feed/color", id)
    t <- jsonlite::fromJSON(feed, simplifyVector = FALSE)
    get_des <- function(x) {
        des <- x$data$description
        if (is.null(des)) "" else des
    }
    get_id <- function(x) {
        id <- x$id
        if (is.null(id)) "" else id
    }
    des <- unlist(lapply(t$items, get_des))
    
    
    # keep just the descriptions of play results
    # (this should hopefully match the number of at-bats)
    # 
    # 

    des <- des[unlist(lapply(t$items, get_id)) == "playResult"]
    
    ##### Works well up to here. Need to figure out a way to unpack all the pitch and AB data.
    
    
    
    idx <- grepl("[0-9]{2,3} mph", des)
    exit <- as.numeric(sub(".* ([0-9]{2,3}) mph.*", "\\1", des[idx]))
    
    idx2 <- grepl("[0-9]{1,3} feet", des)
    dis <- as.numeric(sub(".* ([0-9]{1,3}) feet.*", "\\1", des[idx2]))
    
    # start a matrix of missing values with one column for exit velocities,
    # one column for distance traveled (and one row for each play)
    m <- matrix(rep(NA, 2 * length(des)), ncol = 2)
    m[idx, 1] <- exit
    m[idx2, 2] <- dis
    df <- setNames(data.frame(m), c("exit", "distance"))
    N <- nrow(df)
    if (N > 0) {
        df$game_pk <- id
        df$num <- seq_len(N)
    }
    df
}


