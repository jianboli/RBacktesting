#' Delta adjust notional value of a portfolio
#' @param object An portfolio object
#' @param ... Other parameters
#' @export
DeltaAdjNotional <- function(object, ...)
  UseMethod("DeltaAdjNotional")

#' @describeIn DeltaAdjNotional
#' @title Delta Adjust notional value of an OptionPtf object
#' @return the Delta Adjust Notional value  
#' @export
DeltaAdjNotional.OptionPtf <- function(object, ...){
  if(!is.OptionPtf(object))
    stop("Object type not correct!")
  return(sum(object$delta * object$multiplier * object$amount * object$underlyingPrice))
}

#' @describeIn DeltaAdjNotional
#' @title Delta Adjust notional value of an OptionPtf object
#' @return the Delta Adjust Notional value  
#' @export
DeltaAdjNotional.EquityPtf <- function(object, ...){
  if(!is.EquityPtf(object))
    stop("Object type not corrected!")
  return(sum(object$multiplier * object$amount * object$underlyingPrice))
}