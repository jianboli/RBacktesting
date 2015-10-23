#' Get the historical members from Bloomberg
#'
#' @param indexName The Bloomberg Ticker of the index Name
#' @param dates The given dates that the histrical members will be returned
#' @param firstn Return the first n numbers if n > 0
#'
#' @return A list of the members sorted by Percentage weight (if possible)
#' @export
#'
#' @examples 
#' \dontrun{
#' GetHistoricalMembers("SPX Index", Sys.Date())
#' }
GetHistoricalMembers<- function(indexName, dates, firstn = 0){
  # get the historical dates
  require(Rbbg)
  require(reshape2)
  require(lubridate) 
  conn <- blpConnect()
  SortBy <- function(x, colName, ...) {
    id <- order(x[, colName], ...) 
    return(x[id,])
  }
  
  symbols <- list()
  
  for(i in 1:length(dates)){
    stocks <- bds(conn, indexName, "INDX_MWEIGHT_HIST", 
                  override_fields=c("END_DATE_OVERRIDE"), 
                  override_values=format(dates[i], '%Y%m%d'))
    if(firstn > 0){
      stocks <- SortBy(stocks, "Percent Weight", decreasing = T)[1:firstn, ]
    }
    symbols[[format(dates[i])]] <- stocks[,1]
    print(i/length(dates))
  }
  blpDisconnect(conn)
  return(symbols)
}
