#' Add maximum drawdown 
#' 
#' Add the maximum draw down chart onto a given plot. (Does not work with plot.xts)
#'
#' @param perf The performance data
#' @param mdd The maximum drawdown calcauted from the performance data
#'
#' @return Trivial values
#' @export
#'
#' @note see \code{\link{MaxDrawdown}} for examples
AddMaxDrawdownArrow <- function(perf, mdd){
    lines(c(mdd$from, mdd$to), c(perf[mdd$from], perf[mdd$from]), col="grey")
    lines(c(mdd$from, mdd$to), c(perf[mdd$to], perf[mdd$to]), col="grey")
             
    midx <- (mdd$from - mdd$to)/2 + mdd$to
    arrows(midx, perf[mdd$from], midx, perf[mdd$to], col="red", length = 0.16)
    midy <- mean(c(perf[mdd$from], perf[mdd$to]))
    text(midx, midy, paste0(format(mdd$maxdrawdown*100, digits = 4), "%"), col='red')
}