#' The Notional value of a portfolio
#'
#'@param object The portfolio object
Notional <- function(object, ...)
  UseMethod("Notional")

#'@rdname Notional
#'@title The notional value of the given option portfolio object
#'@param object The option portfolio
#'@return The notioal value
Notional.OptionPos <- function(object){
  return(sum(sign(object$delta) *object$multiplier * object$amount * ifelse(is.na(object$underlyingPrice), object$strike, object$underlyingPrice)))
}
