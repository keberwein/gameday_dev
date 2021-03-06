% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/game_urls.R
\name{game_urls}
\alias{game_urls}
\title{Create urls from game_ids.}
\usage{
game_urls(url_gids = NULL, dataset = NULL, ...)
}
\arguments{
\item{url_gids}{A list of gid urls formatted by the \code{make_gids()} function.}

\item{dataset}{The dataset to be scraped. The default is "inning_all." Other options include, "inning_hit", "players",
"game", and "game_events".}

\item{...}{additional arguments}
}
\description{
Create urls from game_ids.
}
\examples{
gids <- make_gids(start = "2016-06-01", end = "2016-06-01", dataset="inning_all")
urls <- game_urls(gids)

}
