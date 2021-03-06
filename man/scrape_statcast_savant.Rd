% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statcast_dev.R
\name{scrape_statcast_savant}
\alias{scrape_statcast_savant}
\title{This is DEV and needs to be transformed into a payload_method.}
\usage{
scrape_statcast_savant(start_date, end_date, playerid = NULL,
  player_type = NULL)
}
\arguments{
\item{start_date}{Date of first game for which you want data. Format must be in YYYY-MM-DD format.}

\item{end_date}{Date of last game for which you want data. Format must be in YYYY-MM-DD format.}

\item{playerid}{The MLBAM ID for the player who's data you want to query.}

\item{player_type}{The player type. Can be 'batter' or 'pitcher'}
}
\description{
This is DEV and needs to be transformed into a payload_method.
}
\examples{
\dontrun{
scrape_statcast_savant(start_date = "2016-04-06", end_date = "2016-04-15", playerid = 621043, player_type='batter')

scrape_statcast_savant(start_date = "2016-04-06", end_date = "2016-04-15", playerid = 592789, player_type='pitcher')

scrape_statcast_savant(start_date = "2016-04-06", end_date = "2016-04-06")
}
}
