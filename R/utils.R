
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @export
NULL

#' Compound_pipe
#'
#' @name %<>%
#' @rdname compound_pipe
#' @keywords internal
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
#' @export
NULL

#' dt op
#'
#' @name :=
#' @rdname dt_op
#' @keywords internal
#' @importFrom data.table :=
#' @export
NULL

#' @title comb_pload
#' @description Internal combine function for foreach loop used in get_payload()
#' @param x target
#' @param ... additional args
#' @keywords internal
#' @export
comb_pload <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]],lapply(list(...), function(y) y[[i]])))
}


#' Internal function to add a column of dates to a list of gids.
#' @param gidlist A list from the internal data set \code{game_id}
#' @param ... additional arguments.
#' @importFrom dplyr rename
#' @importFrom stringr str_replace str_sub
#' @keywords internal
#' @export

gid_date <- function(gidlist=NULL, ...){
    gidlist$date_dt <- stringr::str_sub(gidlist$gameday_link, 5, 14) %>% stringr::str_replace_all("_", "-")
    return(gidlist)
}


#' Internal function to cast the first letter of a word to upper-case.
#' @param x A word or string to capitalize
#' @param ... additional arguments.
#' @keywords internal
#' @export
upperfirst <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}


#' Internal function to calculate balls and strikes for an atbat.
#' @param dat A pitch table
#' @param ... additional arguments.
#' @keywords internal
#' @export
pitch_count <- function(dat) {
    balls <- as.numeric(dat[,"type"] %in% "B")
    strikes <- as.numeric(dat[,"type"] %in% "S")
    atbat_id <- paste(dat[,"gameday_link"], dat[,"num"])
    atbat_list <- factor(atbat_id, levels=unique(atbat_id))
    bbs <- unlist(tapply(balls, INDEX=atbat_list, function(x){ 
        n <- length(x); pmin(cumsum(c(0, x[-n])), 3) 
    }))
    ks <- unlist(tapply(strikes, INDEX=atbat_list, function(x) { 
        n <- length(x); pmin(cumsum(c(0, x[-n])), 2) 
    }))
    count <- paste(bbs, ks, sep = "-")
    return(cbind(dat, count))
}

#' @title checkurl
#' @description A utility function to run a tryCatch on a URL.
#' @param target url
#' @importFrom utils capture.output
#' @export 
checkurl <- function(target) {  
    tryCatch({  
        con <- url(target)  
        a  <- capture.output(suppressWarnings(readLines(con)))  
        close(con)  
        TRUE;  
    },  
    error = function(err) {  
        occur <- grep("cannot open the connection", capture.output(err));  
        if(length(occur) > 0) {
            close(con)
            FALSE;
        }  
    })  
}
