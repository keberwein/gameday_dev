xml_path = "http://gd2.mlb.com/components/game/mlb/year_2016/month_06/day_01/gid_2016_06_01_arimlb_houmlb_1/inning/inning_all.xml"

# helper function to convert the XML files to data frames
#' @importFrom stringr str_split
#' @importFrom xslt xml_xslt
#' @importFrom xml2 read_xml
#' @importFrom magrittr extract2
#' @importFrom readr read_delim
#' @importFrom dplyr select

xml2df <- function(xml_path, ...) {
    # Find the XSLT template
    xslt_filename <- gsub("\\.xml", "\\.xsl", basename(xml_path))
    
    xsl <- xml2::read_xml(system.file("xsl", xslt_filename, package = "gamedaydev"))
    xml_path <- xml2::read_xml(xml_path)
    
    # Alternative within R apply the stylesheet to the XML
    dat <- xslt::xml_xslt(xml_path, xsl) %>%
        stringr::str_split(pattern = "\n") %>%
        magrittr::extract2(1)
    
    # Remove any blank lines
    dat <- dat[dat != ""]
    
    if (!is.null(attr(dat, "status")) | length(dat) < 2) {
        stop(paste(xml_path, "No data returned, likely a postponed game."))
    }
    
    df <- suppressWarnings(
        paste0(dat, collapse = "\n") %>% 
            readr::read_delim(delim = "|")
    )
    
    # remove any columns that don't have a name
    df <- df[ , !is.na(names(df))]
    # Check for extra columns that may have been appended as a result of a double-pipe delimiter.
    if("X10" %in% colnames(df)) df <- dplyr::select(df, - one_of("X10"))
    
    
    if (nrow(df) == 0) {
        stop(paste(xml_path, "resulted in a data frame with 0 rows."))
    }
    return(df)
}

