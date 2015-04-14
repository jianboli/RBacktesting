#' EquityPtf Class (S3) Constructor: this is a portfolio of equities
#' 
#' @return Generate empty EquityPtf object
#' @export
EquityPtf <- function(){
  x <- data.frame(
    name = character(),
    amount = double(),
    openPrice = double(),
    price = double(),
    multiplier = double()
  )
  class(x) <- c("data.frame","EquityPtf")
  return(x)
}