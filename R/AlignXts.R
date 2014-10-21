#'@title Alignment of xts objects
#' 
#' @name AlginXts
NULL
 
#'@rdname AlignXts
#'@title Align two Pnl time sereies
#'@param pnl1 A xts object
#'@param pnl2 Another xts object
#'@return A table with two pnl aligned
#'@export
AlignPnl <- function(pnl1, pnl2)
{
  if(!(is(pnl1,'xts') & is(pnl2, 'xts')))
    stop("the two pnl's need to be both xts objects!")  
  res <- cbind(pnl1, pnl2, fill = 0)
}

#'@rdname AlignXts
#'@title Algin two performance time series
#'@param perf1 A xts object
#'@param perf2 Another xts object
#'@return A table with two performance aligned
#'@export
AlignPerf <- function(perf1, perf2)
{
  if(!(is(perf1,'xts') & is(perf2, 'xts')))
    stop("the two pnl's need to be both xts objects!")  
  res <- cbind(perf1, perf2, fill = NA)
  return(na.locf(res))
}