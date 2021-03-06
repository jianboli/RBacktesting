% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Drawdown.R
\name{Drawdown}
\alias{Drawdown}
\title{Drawdown}
\usage{
Drawdown(perf)
}
\arguments{
\item{perf}{An historical performance time series}
}
\value{
The percentage draw down of the historical performance
}
\description{
The Drawdown of a time series
}
\examples{
\dontrun{
require(quantmod)
google <- getSymbols("GOOGL", auto.assign = F)
dd <- Drawdown(Ad(google))
op <- par(mfrow=c(2,1))
plot(Ad(google))
plot(dd)
par(op)
}
}

