#' Theta adjust notional value of a portfolio
#' @param object A portfolio object
#' @param ... Other parameters
ThetaAdjNotional <- function(object, ...)
  UseMethod("ThetaAdjNotional")


#' @describeIn ThetaAdjNotional
#' @title Theta Adjust notional value of an OptionPos object
#' @return the Theta Adjust Notional value
ThetaAdjNotional.OptionPos <- function(object){
  return(sum(object$theta * object$multiplier * object$amount * object$underlyingPrice))
}