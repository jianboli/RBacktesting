#' The Drawdown of a time series
#' @param perf An historical performance time series
#' @return The draw down of the historical performance
#' @export
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

