#' A summary of Performance
#' 
#' @param pnl The P&L 
#' @param indexPnl The P&L of benchmark, used to calucated the information ratio
#' @param r Interest rate
#' @return A summary of the performance, include the following names:
#' \itemize{
#'        \item Return The total return
#'        \item Sd The standard derivation
#'        \item Mdd The maximium draw down
#'        \item SharpeRatio The Sharpe ratio
#'        \item IR The information ratio
#'        \item PostiveRate The postive performance ratio
#'        \item tValue The student t value
#'        \item pValue The p value assuming two tail test. For one tail, divided by 2
#'        }
#' @examples
#' \dontrun{
#' yahoo <- getSymbols('YHOO',auto.assign = F)
#' perf <- Ad(yahoo)
#' pnl <- Performance2Pnl(perf)
#' spy <- getSymbols('SPY',auto.assign = F)
#' indexPnl <- Performance2Pnl(Ad(spy))
#' pnls <- AlignPnl(pnl, indexPnl) 
#' PerformanceSummary(pnls[,1], pnls[,2], r= 0.003)
#' YearlyPerformanceSummary(index(pnls), pnls[,1], pnls[,2], r=0.003)
#' AnnualizedPerformanceSummary(pnls[,1], pnls[,2], r=0.003, freq=1/265)
#' }
#' @export
PerformanceSummary <- function(pnl, indexPnl, r=0){
  exPnl <- pnl - indexPnl
  t <- mean(exPnl)*sqrt(length(pnl))/sd(expnl)
  return(c(Return=prod(1+pnl), 
           Sd=sd(pnl), 
           Mdd=MaxDrawdown(CumReturn(pnl))$maxdrawdown, 
           SharpeRatio=SharpeRatio(pnl, r), 
           IR = InformationRatio(pnl, indexPnl),
           PostiveRate = sum(exPnl > 0)/length(exPnl),
           t = t,
           p = 2*pt(-abs(t),df=length(exPnl)-1) # assume two tail 
         )
        )
}

#' @rdname PerformanceSummary
#' @title The month performance summary if the corresponding dates are given
#' @param dates The corresponding dates of the P&L
#' @importFrom lubridate month
#' @export
MonthlyPerformanceSummary <- function(dates, pnl, indexPnl, r=0){
  allMonths <- as.yearmon(dates)
  uniqueAllMonths <- unique(allMonths)
  perfSummary <- data.frame(Month=numeric(0), Return=numeric(0), Sd=numeric(0), MDD=numeric(0), SharpeRatio=numeric(0), IR=numeric(0))
  for(i in uniqueAllMonths){
    idx <- allMonths==i
    summary <- PerformanceSummary(pnl[idx], indexPnl[idx], r)
    perfSummary <- rbind(perfSummary, c(i,summary))  
  }
  colnames(perfSummary) <- c("Month", "Return", "Sd", "MDD", "SharpeRatio", "IR", "PositveRate", "tValue", "pValue")
  return(perfSummary) 
}

#' @rdname PerformanceSummary
#' @title The yearly performance summary if the corresponding dates are given
#' @importFrom lubridate year
#' @export
YearlyPerformanceSummary <- function(dates, pnl, indexPnl, r=0){
  allYears <- year(dates)
  uniqueAllYears <- unique(allYears)
  perfSummary <- data.frame(Year=numeric(0), Return=numeric(0), Sd=numeric(0), MDD=numeric(0), SharpeRatio=numeric(0), IR=numeric(0))
  for(i in uniqueAllYears){
    idx <- allYears==i
    summary <- PerformanceSummary(pnl[idx], indexPnl[idx], r)
    perfSummary <- rbind(perfSummary, c(i,summary))  
  }
  colnames(perfSummary) <- c("Year", "Return", "Sd", "MDD", "SharpeRatio", "IR", "PositveRate", "tValue", "pValue")
  return(perfSummary) 
}
#' @rdname PerformanceSummary
#' @title The Annulized performance summary if the sample frequency is given
#' @param freq The sample frequency
#' @export
AnnualizedPerformanceSummary <- function(pnl, indexPnl, r=0, freq=1/12){ 
  exPnl <- (pnl-indexPnl)
  annulizeMean <- function(x) mean(x)/freq
  annulizeSd <- function(x) sd(x)/sqrt(freq)
  t <- annulizeMean(exPnl)*sqrt(length(exPnl))/annulizeSd(exPnl)
  return(c(Return=annulizeMean(pnl), 
           Sd=annulizeSd(pnl), 
           Mdd=MaxDrawdown(cumprod(1+pnl))$maxdrawdown, 
           SharpeRatio=SharpeRatio(pnl, r)/sqrt(freq), 
           IR = InformationRatio(pnl, indexPnl)/sqrt(freq), 
           PostiveRate = sum(exPnl > 0)/length(exPnl),
           tValue = t,
           pValue = 2*pt(-abs(t),df=length(exPnl)-1) # assume two tail 
  ) # c
  ) # return
}
