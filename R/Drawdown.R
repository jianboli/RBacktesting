#' The Drawdown of a time series
#' @param perf An historical performance time series
#' @return The draw down of the historical performance
Drawdown <- function(perf)
{
  peak <- -Inf
  dd <- double()
  for(i in seq(along.with=perf)){
    if(perf[i] > peak){
      peak <- perf[i]
      peakPos <- i
    }
    dd[i]  <- (peak - perf[i])/peak
  }
  return(dd)
}