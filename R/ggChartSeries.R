#' ggplot version of Candle chart
#' @param x An OHLC object to be charted
#' @param start The starting time
#' @param end The ending time 
ggChartSeries <- function(x, start=NA, end=NA){
  require(ggplot2)
  if(is.na(start))
    start = as.Date(index(x)[1])
  if(is.na(end))
    end = as.Date(tail(index(x),1))
  
  # the below is done redundantly for ease of maintenance later on
  #First, strip OHLC data (need to vectorize)
  date <- as.Date(time(x))
  open <- as.vector(Op(x))
  high <- as.vector(Hi(x))
  low <- as.vector(Lo(x))
  close <- as.vector(Cl(x))
  
  #Then build the data frame
  xSubset <-data.frame('date'=date,'open'=open,'high'= high,'low'=low,'close'=close)
  
  #We want to construct our candlesticks  
  xSubset$candleLower <- pmin(xSubset$open, xSubset$close)
  xSubset$candleMiddle <- NA
  xSubset$candleUpper <- pmax(xSubset$open, xSubset$close)
  xSubset$fill <- ''
  xSubset$fill[xSubset$open < xSubset$close] = 'white'
  xSubset$fill[xSubset$fill ==''] = 'red'
  
  #Add Moving Averages
  xSubset$ma200 <- SMA(xSubset$close, 200)
  xSubset$ma50 <- SMA(xSubset$close, 50)
  
  #Trim Data
  xSubset <-subset(xSubset, xSubset$date > start & xSubset$date < end)
  
  #Graphing Step
  g <- ggplot(xSubset, aes(x=date, lower=candleLower, middle=candleMiddle, upper=candleUpper, ymin=low, ymax=high)) 
  g <- g + geom_boxplot(stat='identity', aes(group=date, fill=fill))
  g <- g + geom_line(aes(x=date, y=ma50))+ geom_line(aes(x=date, y=ma200))
  g 
}
