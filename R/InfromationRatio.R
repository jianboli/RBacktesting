#' Infromation ratio
#' 
#' @param pnl The P&L of the performance
#' @param indexPnl The P&L of the benchmark
#' @param freq The sample frequency
#' @param na.rm A boolean value to show if NA's should be removed during calcuation 
#' @return Infromation Ratio: \url{https://en.wikipedia.org/wiki/Information_ratio}
#' 
InformationRatio <- function(pnl, indexPnl, freq = 1, na.rm = F){
  activePnl <- pnl-indexPnl
  return(mean(activePnl, na.rm = na.rm)/sd(activePnl, na.rm = na.rm) / sqrt(freq))
}
