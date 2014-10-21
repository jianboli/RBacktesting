#' P&L from performance
#' 
#' @param perf Performance or the cumulative Peformance
#' @return P&L
#' @export
Performance2Pnl <- function(perf){
  if(is(perf, 'xts')){
    res <- lag(perf)/perf-1
    res[1] <- 0
    return (res)
  }
  return(c(0, tail(perf, -1)/head(perf, -1)-1))
}