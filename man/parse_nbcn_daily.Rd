% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{parse_nbcn_daily}
\alias{parse_nbcn_daily}
\title{Parse parse_nbcn_daily from MeteoSwiss}
\usage{
parse_nbcn_daily(
  p = NULL,
  remote = FALSE,
  stn = "SMA",
  type = c("current", "previous")
)
}
\arguments{
\item{p}{File path or URL}

\item{remote}{Load from remote (URL)}

\item{stn}{Station identifier (e. g. "SMA" or "BAS")}

\item{type}{"current" for current year or "previous" for historical}
}
\value{
data table
}
\description{
Parse parse_nbcn_daily from MeteoSwiss
}
\examples{
dat <- parse_nbcn_daily(p = dir.data, remote = TRUE, stn = "SMA", type = "current")
}
\author{
M. Saenger
}
