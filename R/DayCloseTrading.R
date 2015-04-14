#' Trade at closing price of every day
#' @param dates The dates that are aviable for the trading
#' @param DailyGainFun Function that calculate daily gain based on today's holding and yesterday's holding
#' @param TradingFun Function that calculate the trading needed. Input parameters should be current holdings, and date; output parameters should includes "HoldingEnd", "TradingCost", "Tradings"
#' @return A list that contains the following structure: Portfolio: portfolio history list; AUM: aum history vector; Trading: trading history list; TradingCost: trading cost hostiry vector
#' @note This function does not do much. Most of the work is done by the parameter functions. It is constructed to standardize the back testing process
#' @note This function can also be applied to event driven based backtesting as there is no requirement of the dates passed in to be continues
#' @note This function cannot handel the trades happened during the day
#' @export
DayCloseTrading <- function(dates,
                            DailyGainFun,
                            TradingFun)
{
  n <- length(dates)
  portfolio <- list()
  trading <- list()
  pnl <- double(n)
  aum <- double(n)
  tradingCost <- double(n)
  # first day trading
  aum[1] <- 1
  trades <- TradingFun(todayHolding, dates[1])
  tradingCost[1] <- aum[1]*trades$TradingCost
  trading[[1]] <- trades$Tradings
  portfolio[[1]] <- trades$HoldingEnd
  pnl[1] <- 0
  aum[1] <- aum[1]*(1-trades$TradingCost)
  # daily trading
  if(n > 1)
  {  
    for(i in 2:n){
      # maket movement
      date <- dates[i]
      yesterdayHolding <- portfolio[[i-1]]
      pnl[i] <- DailyGainFun(yesterdayHolding, date, dates[i-1])
      aum[i] <- aum[i-1]*(pnl[i] + 1)
      # trading
      trades <- TradingFun(todayHolding, date)
      portfolio[[i]] <- trades$HoldingEnd
      aum[i] <- aum[i]*(1-trades$TradingCost)
      tradingCost[i] <- aum[i]*trades$TradingCost
      trading[[i]] <- trades$Tradings
    }
  }
  return(list(Portfolio = portfolio, AUM = aum, PNL = pnl, Trading = trading, TradingCost = tradingCost))
}