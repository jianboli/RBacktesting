#' Calcuate the Sharpe Ratio
#' 
#' @param pnl The Sharpe ratio based on pnl
#' @param r The interest rate
#' @param freq The frequency of the sample. e.g. if daily data are used but to calucate annulized Shape ratio, freq = 1/365 or 1/265
#' @param na.rm A boolean value used to show if NA's should be removed during the calculation
#' @return The Shape ratio
#' @export 
SharpeRatio <- function(pnl, r=0, freq=1, na.rm = T){
  return((mean(pnl, na.rm=na.rm)-r)/sd(pnl, na.rm=na.rm)*sqrt(1/freq))
}