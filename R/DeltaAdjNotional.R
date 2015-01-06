#' Delta adjust notional value of a portfolio
#' @param object An portfolio object
#' @param ... Other parameters
#' @export
DeltaAdjNotional <- function(object, ...)
  UseMethod("DeltaAdjNotional")

#' @describeIn DeltaAdjNotional
#' @title Delta Adjust notional value of an OptionPos object
#' @return the Delta Adjust Notional value  
#' @export
DeltaAdjNotional.OptionPos <- function(object, ...){
  return(sum(object$delta * object$multiplier * object$amount * object$underlyingPrice))
}