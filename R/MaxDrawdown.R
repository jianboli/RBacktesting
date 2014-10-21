#' The maximium drawdown of a time seris
#' 
#' @param perf The historical performance of a time series
#' @return A list includes \itemize{\item maxdrawdown: The MDD value \item from: The starting time of the MDD \item to: The end time of the MDD}
#' @export
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