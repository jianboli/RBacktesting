#' The total permium value of a portfolio
#' 
#' @param object An portfolio object
#' @return The premium value of the portfolio
TotalValue <- function(object, ...)
  UseMethod('TotalValue')

#' @rdname TotalValue
#' @param object An OptionPos object
TotalValue.OptionPos <- function(object){
  return(sum(object$multiplier * object$amount * object$price))
}