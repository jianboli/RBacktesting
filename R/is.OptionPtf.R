#' @name is
#' @title Check if an object is Corresponding object
NULL 

#' @rdname  is
#' @param object An object to be examed
#' @return A boolean value to indicated if object is OptionPtf
is.OptionPtf <- function(object){
  return('OptionPtf' %in% class(object))
}

#' @rdname is
#' @return A boolean value to indicated if object is OptionPtf
is.EquityPtf <- function(object){
  return('EquityPtf' %in% class(object))
}