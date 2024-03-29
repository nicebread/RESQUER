% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{get_indicators}
\alias{get_indicators}
\title{Collate the indicator list into a data.frame}
\usage{
get_indicators(sc, pattern = ".*")
}
\arguments{
\item{sc}{Scores object (as provided by \code{read_RESQUE})}

\item{pattern}{regexp pattern that filters the results. The default selects all indicators}
}
\description{
Collate the indicator list into a data.frame
}
