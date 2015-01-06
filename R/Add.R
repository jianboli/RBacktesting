#' S3 generic function  Add a object into a list
#' @param object the object to be working on
#' @param ... Other parameters
#' @export
Add <- function(object, ...)
  UseMethod("Add")

#'@describeIn Add Append an new option object to the end of OptionPos object
#'@param strike The strick of the new option
#'@param expiration The expiration date of the new option
#'@param type What is the type of the option: call or put
#'@param underlyingName The underlying ticker of the option
#'@param name The name of the option, if missing automatically construct one based on the option information
#'@param multiplier The contract size of the option
#'@param amount The quantity of the option contracts
#'@param price The current price of the option
#'@param delta The delta of the option
#'@param vega The vega of the option
#'@param theta The theta of the option
#'@param underlyingPrice The price of the underlying security
#'@param underlyingSigma The volatility of the underlying security
#'@param interestRate the current interestedRate
#'@return The OptionPos with the new option appended
#'@export
Add.OptionPos <- function(object, strike, expiration, type=c('C','P'), underlyingName = "", name = "", multiplier = 100,
                          amount = 0, price = NA, delta = NA, vega = NA, theta = NA, underlyingPrice = NA, underlyingSigma = NA, interestRate = NA, ...){
  if(!is.OptionPos(object))
    return(object)
  
  if(!is.Date(expiration))
    stop("Expiration must be a date!")
  if(!is.numeric(strike))
    stop("Strike must be a number!")
  # construct the name
  if(stringr::str_trim(underlyingName) != "" && name == "")
  {
    strs <- strsplit(underlyingName, ' ')
    yellowKey <- tail(strs[[1]], 1)
    if(yellowKey == 'Equity' || yellowKey == 'Index'){
      ticker <- paste(head(strs[[1]],-1), sep = ' ', collapse=" ")
    } else{
      ticker  <- underlyingName
      yellowkey  <- ''
    }
    name  <- str_trim(paste(ticker, format(expiration, '%m/%d/%Y'), paste(type, strike, sep=''), yellowKey, sep = ' '))
  }
  # append
  res <- rbind(object, data.frame(name = name, underlyingName = underlyingName, strike = strike, expiration = expiration, type = type, multiplier = multiplier,
                                  amount = amount, price = price, delta = delta, vega = vega, theta = theta, underlyingPrice = underlyingPrice, underlyingSigma = underlyingSigma, interestRate = interestRate))
  
  class(res) <- c('data.frame', 'OptionPos')
  return(res)
  
}