CumReturn <- function(pnl){
  return(cumprod(pnl+1))
}

Performance2Pnl <- function(perf){
  return(c(0, tail(perf, -1)/head(perf, -1)-1))
}

PerformanceSummary <- function(pnl, indexPnl, r=0){
  return(c(Return=prod(1+pnl), 
           Sd=sd(pnl), 
           Mdd=MaxDrawdown(cumprod(1+pnl))$maxdrawdown, 
           SharpeRatio=SharpeRatio(pnl, r), 
           IR = InformationRatio(pnl, indexPnl)))  
}

YearlyPerformanceSummary <- function(dates, pnl, indexPnl, r=0){
  require(lubridate)
  allYears <- year(dates)
  uniqueAllYears <- unique(allYears)
  perfSummary <- data.frame(Year=numeric(0), Return=numeric(0), Sd=numeric(0), MDD=numeric(0), SharpeRatio=numeric(0), IR=numeric(0))
  for(i in uniqueAllYears){
    idx <- allYears==i
    summary <- PerformanceSummary(pnl[idx], indexPnl[idx], r)
    perfSummary <- rbind(perfSummary, c(i,summary))  
  }
  colnames(perfSummary) <- c("Year", "Return", "Sd", "MDD", "SharpeRatio", "IR")
  return(perfSummary) 
}

AnnualizedPerformanceSummary <- function(pnl, indexPnl, r=0, freq=1/12){ 
  return(c(Return=mean(pnl)/freq, 
           Sd=sd(pnl)/sqrt(freq), 
           Mdd=MaxDrawdown(cumprod(1+pnl))$maxdrawdown, 
           SharpeRatio=SharpeRatio(pnl, r)/sqrt(freq), 
           IR = InformationRatio(pnl, indexPnl)/sqrt(freq)))  
}



RiskSummary <- function(){
  
}

ExposureSummary <- function(long, short){
  
}

TurnOverSummary <- function(){
}