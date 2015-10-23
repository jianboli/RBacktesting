#' Drawdown
#' 
#' The Drawdown of a time series
#' @param perf An historical performance time series
#' @return The percentage draw down of the historical performance
#' @export
#' @examples  
#' \dontrun{
#' require(quantmod)
#' google <- getSymbols("GOOGL", auto.assign = F)
#' dd <- Drawdown(Ad(google)) 
#' op <- par(mfrow=c(2,1))
#' plot(Ad(google))
#' plot(dd)
#' par(op)
#' }
Drawdown <- function(perf)
{
  if(is(perf, 'xts'))
  {
    idx <- index(perf)
    perf <- drop(coredata(perf))  
    isXts <- T
  }else
    isXts <- F
  
  peak <- -Inf
  dd <- double()
  for(i in seq(along.with=perf)){
    if(perf[i] > peak){
      peak <- perf[i]
      peakPos <- i
    }
    dd[i]  <- (peak - perf[i])/peak
  }
 
  if(isXts)
    dd <- xts(dd, idx)
  
  return(dd)
}

