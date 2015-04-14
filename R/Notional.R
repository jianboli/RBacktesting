#' The Notional value of a portfolio
#'
#'@param object The portfolio object
#'@param ... Other parameters
#'@export
Notional <- function(object, ...)
  UseMethod("Notional")

#'@describeIn Notional Caluate the notional value of a Option portfolio
#'@title The notional value of the given option portfolio object
#'@return The notioal value
#'@export
Notional.OptionPtf <- function(object, ...){
  return(sum(sign(object$delta) *object$multiplier * object$amount * ifelse(is.na(object$underlyingPrice), object$strike, object$underlyingPrice)))
}
