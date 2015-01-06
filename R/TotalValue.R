#' The total permium value of a portfolio
#' 
#' @param object An portfolio object
#' @param ... Other parameters
#' @return The premium value of the portfolio
TotalValue <- function(object, ...)
  UseMethod('TotalValue')

#' @describeIn TotalValue
TotalValue.OptionPos <- function(object){
  return(sum(object$multiplier * object$amount * object$price))
}