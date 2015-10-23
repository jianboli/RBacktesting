#' Maximum drawdown
#' 
#' The maximium drawdown of a time seris in the percentage values. 
#' The maxdrawdown function in tseries 
#' does not seem to be right given the fact that it is counting the absolute 
#' value changes 
#' 
#' @param perf The historical performance of a time series
#' @return A list includes \itemize{\item maxdrawdown: The MDD value \item from: The starting time of the MDD \item to: The end time of the MDD}
#' @export
#' @examples 
#' \dontrun{
#' require(quantmod)
#' google <- getSymbols("GOOGL", auto.assign = F)
#' perf <- Ad(google)
#' mdd <- MaxDrawdown(perf)
#' plot.zoo(perf)
#' AddMaxDrawdownArrow(perf, mdd)
#' }
MaxDrawdown <- function(perf)
{
  if(is(perf, 'xts'))
  {
    idx <- index(perf)
    perf <- drop(coredata(perf))  
  }else
  {
    idx <- seq(along.with=perf)
  }    
  mdd <- 0
  peak <- -Inf
  from <- 1
  to <- 1
  dd <- double()
  for(i in seq(along.with=perf)){
    if(perf[i] > peak){
      peak <- perf[i]
      peakPos <- i
    }
    dd[i]  <- (peak - perf[i])/peak
    if(dd[i] > mdd)
    {  
      mdd <- dd[i]
      to <- i
      from <- peakPos
    }
  }
  return(list(maxdrawdown = mdd, from = idx[from], to = idx[to]))  
}