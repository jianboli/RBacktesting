#' Price And Delta Update
#' 
#' @param object An portolio object
PriceAndDeltaUpdate <- function(object, ...)
  UseMethod("PriceAndDeltaUpdate")

#' @rdname PriceAndDeltaUpdate
#' @param date The actual as of date of the calculation
#' @return Calcuate the closing price and delta based on black scholes model
PriceAndDeltaUpdate.OptionPos <- function(object, date){
  if(! is.OptionPos(object))
    stop("object is not a PotionPos object")
  if(! is.Date(date))
    stop("date is not a Date object")
  tau  <- as.numeric(as.Date(object$expiration) - date)/365
  
  K <- object$strike
  S <- object$underlyingPrice
  r <- object$interestRate
  sigma <- object$underlyingSigma
  
  d1 <- (log(S/K) + (r + sigma^2/2)*tau)/(sigma*sqrt(tau))
  d2 <- d1 - sigma*sqrt(tau)
  
  object$delta <- ifelse(tau <= 0, 1,
                         ifelse(object$type == 'C', pnorm(d1),
                                ifelse(object$type == 'P', -pnorm(-d1), NA)))
  object$price <- ifelse(tau <= 0, ifelse(object$type == 'C', pmax(S-K,0), pmax(K-S,0)),
                         ifelse(object$type == 'C', object$delta*S - pnorm(d2)*K*exp(-r*tau),
                                ifelse(object$type == 'P', object$delta*S + pnorm(-d2)*K*exp(-r*tau), NA)))
  
  object$vega <- ifelse(tau <=0, 0, dnorm(d1)*S*sqrt(tau))
  object$theta <- ifelse(tau <=0, 0, 
                         ifelse(object$type == 'C', - S*dnorm(d1)*sigma/(2*sqrt(tau)) - r*K*exp(-r*tau)*pnorm(d2),
                                ifelse(object$type == 'P', -S*dnorm(d1)*sigma/(2*sqrt(tau)) + r*K*exp(-r*tau)*pnorm(-d2), NA)))
  return(object)
}