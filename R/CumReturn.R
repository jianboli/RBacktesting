#' Cumulative Return, or the performance of a portfolio given the P&L
#' 
#' @param pnl The P&L
#' @return The cumulative return
#' @references Performance2Pnl
#' @export
CumReturn <- function(pnl){
  return(cumprod(pnl+1))
}

