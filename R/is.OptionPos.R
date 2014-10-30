#' @title Check if an object is OptionPos
#' 
#' @name  is.OptionPos
#' @param object An object to be examed
#' @return A boolean value to indicated if object is OptionPos
is.OptionPos <- function(object){
  return('OptionPos' %in% class(object))
}