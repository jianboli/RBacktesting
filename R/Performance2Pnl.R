#' P&L from performance
#' 
#' @param perf Performance or the cumulative Peformance
#' @param firstDayPNL the P&L of first day
#' @return P&L
#' @export
Performance2Pnl <- function(perf, firstDayPNL = 0){
  if(is(perf, 'xts')){
    res <- lag(perf)/perf-1
    res[1] <- firstDayPNL
    return (res)
  }
  return(c(0, tail(perf, -1)/head(perf, -1)-1))
}