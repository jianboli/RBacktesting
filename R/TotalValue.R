#' The total permium value of a portfolio
#' 
#' @param object An portfolio object
#' @param ... Other parameters
#' @return The premium value of the portfolio
TotalValue <- function(object, ...)
  UseMethod('TotalValue')

#' @describeIn TotalValue
TotalValue.OptionPtf <- function(object, ...){
  if(!is.OptionPtf(object))
    stop("Object type not correct!")
  return(sum(object$multiplier * object$amount * object$price))
}

#' @describeIn TotalValue
TotalValue.EquityPtf <- function(object, ...){
  if(!is.EquityPtf(object))
    stop("Object type not corrected!")
  return(sum(object$multiplier * object$amount * object$price))
}
