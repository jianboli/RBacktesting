\name{RBacktesting-package}
\alias{RBacktesting-package}
\alias{RBacktesting}
\docType{package}
\title{
A backtesting toolkit
}
\description{
Some tools to facilitate process of trading stratergies backtesting. It is not intented to replace any of existing sophistic backtesting packages like Rmetrics. It is just something that I use daily.
}
\details{
\tabular{ll}{
Package: \tab RBacktesting\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2014-10-17\cr
License: \tab What license is it under?\cr
}
This package includes functions belongs two the following section:
\itemize{
\item Getting data
\item Back testing data
\item Evaluated portfolio performance
\item Evaluate per trade performance
\item Option portfolio
}}
\author{
Bob Li
Maintainer:{ }
}
\references{
}
\keyword{Backtesting}
\examples{
\dontrun{
 yahoo <- getSymbols('YHOO',auto.assign = F)
 perf <- Ad(yahoo)
 pnl <- Performance2Pnl(perf)
 spy <- getSymbols('SPY',auto.assign = F)
 indexPnl <- Performance2Pnl(Ad(spy))
 pnls <- AlignPnl(pnl, indexPnl) 
 PerformanceSummary(pnls[,1], pnls[,2], r= 0.003)
 YearlyPerformanceSummary(index(pnls), pnls[,1], pnls[,2], r=0.003)
 AnnualizedPerformanceSummary(pnls[,1], pnls[,2], r=0.003, freq=1/265)
 }
 }
