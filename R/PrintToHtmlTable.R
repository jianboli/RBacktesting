#' import xtable
#' 
#' @param table A data.frame object of something that can be convert to a table
#' @param include.rownames A boolean value to decide if the row names should be include in the output
#' @return the return value is not important, this function is used for its side effect 
PrintToHtmlTable <- function(table, include.rownames = F){
  if(!(is.data.frame(table)))
    table <- as.data.frame(table)
  table <- xtable(table)
  print(table, type='html', include.rownames=include.rownames) # there is a lot of parameters that we can play here
}
