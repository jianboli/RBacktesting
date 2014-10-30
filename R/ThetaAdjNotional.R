#' Theta adjust notional value of a portfolio
#'
ThetaAdjNotional <- function(object, ...)
  UseMethod("ThetaAdjNotional")


#' @rdname ThetaAdjNotional
#' @title Theta Adjust notional value of an OptionPos object
#' @param An OptionPos object
#' @return the Theta Adjust Notional value
ThetaAdjNotional.OptionPos <- function(object){
  return(sum(object$theta * object$multiplier * object$amount * object$underlyingPrice))
}