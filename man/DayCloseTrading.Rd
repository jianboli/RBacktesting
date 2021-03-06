% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/DayCloseTrading.R
\name{DayCloseTrading}
\alias{DayCloseTrading}
\title{Trade at closing price of every day}
\usage{
DayCloseTrading(dates, DailyGainFun, TradingFun)
}
\arguments{
\item{dates}{The dates that are aviable for the trading}

\item{DailyGainFun}{Function that calculate daily gain based on today's
holding and yesterday's holding, the parameters are listed as following
\itemize{
      \item previous day's holding positions
      \item today's date
      \item previous date
      }}

\item{TradingFun}{Function that calculate the trading needed.
Input parameters should be current holdings (NULL will mean empty portfolio), and date; output parameters should includes "HoldingEnd", "TradingCost", and "Tradings",
where "TradingCost" should be in percentage of AUM}
}
\value{
A list that contains the following structure: Portfolio: portfolio history list; AUM: aum history vector; Trading: trading history list; TradingCost: trading cost hostiry vector
}
\description{
Trade at closing price of every day
}
\note{
This function does not do much. Most of the work is done by the parameter functions. It is constructed to standardize the back testing process

This function can also be applied to event driven based backtesting as there is no requirement of the dates passed in to be continues

This function cannot handel the trades happened during the day
}

