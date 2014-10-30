#' Delta adjust notional value of a portfolio
DeltaAdjNotional <- function(object, ...)
  UseMethod("DeltaAdjNotional")

#' @rdname DeltaAdjNotional
#' @title Delta Adjust notional value of an OptionPos object
#' @param object An OptionPos object
#' @return the Delta Adjust Notional value  
DeltaAdjNotional.OptionPos <- function(object){
  return(sum(object$delta * object$multiplier * object$amount * object$underlyingPrice))
}