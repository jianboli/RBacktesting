#' Trade at closing price of every day
#' @param dates The dates that are aviable for the trading
#' @param EmptyPortfolioFun Function that initializes an empty portfolio
#' @param EmptyTradingBookFun Function that intialize an empty trading book
#' @param UpdateMarketDataFun Function that update market data given a holding and the maket date, returns the updated holding
#' @param DailyGainFun Function that calculate daily gain based on today's holding and yesterday's holding
#' @param TradingFun Function that calculate the trading needed. Input parameters should be current holdings, and date; output parameters should includes "HoldingEnd", "TradingCost", "Tradings"
#' @return A list that contains the following structure: Portfolio: portfolio history list; AUM: aum history vector; Trading: trading history list; TradingCost: trading cost hostiry vector
#' @note This function does not do much. Most of the work is done by the parameters function. It is constructed to standardize the back testing process
#' @note This function can also be applied to event driven based backtesting as there is no requirement of the dates passed in to be continues
#' @note This function cannot handel the trades happened during the day
#' @export
DayCloseTrading <- function(dates, EmptyPortfolioFun, 
                            EmptyTradingBookFun, UpdateMarketDataFun, 
                            DailyGainFun, TradingFun)
{
  n <- length(dates)
  portfolio <- list()
  trading <- list()
  aum <- double(n)
  tradingCost <- double(n)
  # first day trading
  portfolio[[1]] <- EmptyPortfolioFun() 
  trading[[1]] <- EmptyTradingBookFun()
  aum[1] <- 1
  tradingCost[1] <- 0
  # daily trading
  if(n > 1)
  {  
    for(i in 2:n){
      # maket movement
      yesterdayHolding <- portfolio[[i-1]];
      todayHolding <- UpdateMarketDataFun(yesterdayHolding, dates);
      pnl <- DailyGainFun(todayHolding, yesterdayHolding);
      aum[i] <- aum[i-1]*(pnl + 1)
      # trading
      trades <- TradingFun(todayHolding, date)
      portfolio[[i]] <- trades$HoldingEnd
      aum[i] <- aum[i]*(1-trades$TradingCost)
      tradingCost[i] <- aum[i]*trades$TradingCost
      trading[[i]] <- trades$Tradings
    }
  }
  return(list(Portfolio = portfolio, AUM = aum, Trading = trading, TradingCost = tradingCost))
}