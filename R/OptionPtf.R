#' OptionPtf Class (S3) Constructor: this is a portfolio of options
#' 
#' @return Generate empty OptionPtf object
#' @export
OptionPtf <- function(){
  x <- data.frame(
    name = character(),
    underlyingName = character(),
    strike = double(),
    expiration = double(),
    type = character(),
    multiplier = double(),
    amount = double(),
    openPrice = double(),
    price = double(),
    delta = double(),
    vega = double(),
    theta = double(),
    underlyingPrice = double(),
    underlyingSigma = double(),
    interestRate = double()
  )
  class(x) <- c("data.frame","OptionPtf")
  return(x)
}