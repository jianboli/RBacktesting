#' Import member history Index Data
#' 
#' @import Rbbg
#' @importFrom reshape2 acast  
#' @note This function requires a lot bloomberg data if the period between stDate 
#' and endDate are very long
#' @param indexName The ticker of the index, e.g. SPX Index
#' @param fields A vector of fields
#' @param stDate Starting date
#' @param endDate Ending date
#' @param monthly If montly data is preferred
#' @param  firstn If specified (not 0), only return the top fistn members with highest weight
#' @return A list including the following items
#' \itemize{
#' \item symbols
#' \item fields
#' \item data
#' \item indexData
#' }
#' @export
GetMemberHistory <- function(indexName, fields, stDate, endDate, monthly=F, 
                             firstn = 0){
  require(Rbbg)
  require(reshape2)
  require(lubridate) 
  conn <- blpConnect()
  SortBy <- function(x, colName, ...) {
    id <- order(x[, colName], ...) 
    return(x[id,])
  }
  
  symbols <- list()
  if(monthly){
    startOfMonth <- stDate + months(1)
    startOfMonth <- as.Date(ISOdate(year(startOfMonth), month(startOfMonth), 1))
    rebalance.date <- seq.Date(from = startOfMonth, to = endDate, by='month') - 1  
  }else
  {
    rebalance.date <- seq.Date(from = stDate, to = endDate, by='day')
  }
  for(i in 1:length(rebalance.date)){
    stocks <- bds(conn, indexName, "INDX_MWEIGHT_PX", 
                  override_fields=c("END_DATE_OVERRIDE"), 
                  override_values=format(rebalance.date[i], '%Y%m%d'))
    if(nrow(stocks) == 700) # the limit of INDX_MWEIGHT_PX
      stocks <- rbind(stocks, bds(conn, indexName, "INDX_MWEIGHT_PX2", 
                                  override_fields=c("END_DATE_OVERRIDE"), 
                                  override_values=format(rebalance.date[i], '%Y%m%d')))
    if(nrow(stocks) == 1400)
      stocks <- rbind(stocks, bds(conn, indexName, "INDX_MWEIGHT_PX3", 
                                  override_fields=c("END_DATE_OVERRIDE"), 
                                  override_values=format(rebalance.date[i], '%Y%m%d')))
    
    if(firstn > 0){
      stocks <- SortBy(stocks, "Percent Weight", decreasing = T)[1:firstn, ]
    }
    
    symbols[[i]] <- stocks[,1]
  }
  
  #get the historical data
  total.symbol <- do.call(c, symbols)
  names(symbols) <- format(rebalance.date)
  total.symbol <- paste(total.symbol, "Equity")
  total.symbol.unique <- unique(total.symbol)
  
  if(monthly)
  {
    data <- bdh(conn, total.symbol.unique, fields, stDate, endDate, 
                option_names = c("periodicitySelection","nonTradingDayFillOption", "nonTradingDayFillMethod"),
                option_values = c("MONTHLY", "ALL_CALENDAR_DAYS", "PREVIOUS_VALUE")) 
    
    data.index <- bdh(conn, indexName, fields, stDate, endDate, 
                      option_names = c("periodicitySelection","nonTradingDayFillOption", "nonTradingDayFillMethod"),
                      option_values = c("MONTHLY", "ALL_CALENDAR_DAYS", "PREVIOUS_VALUE"))  
  }else{
    data <- bdh(conn, total.symbol.unique, fields, stDate, endDate, 
                option_names = c("nonTradingDayFillOption", "nonTradingDayFillMethod"),
                option_values = c("ALL_CALENDAR_DAYS", "PREVIOUS_VALUE"))  
    
    data.index <- bdh(conn, indexName, fields, stDate, endDate, 
                      option_names = c("nonTradingDayFillOption", "nonTradingDayFillMethod"),
                      option_values = c("ALL_CALENDAR_DAYS", "PREVIOUS_VALUE"))
  }
  blpDisconnect(conn)  
  data.wide <- list()
  data.index.wide <- list()
  
  for(i in 1:length(fields))
  {
    data.wide[[i]] <- acast(data, date~ticker, value.var = fields[i])
    data.index.wide[[i]] <- data.index[,fields[i], drop=F]
    colnames(data.index.wide[[i]]) <- indexName
  }
  
  return(list(symbols=symbols, fields=fields, data=data.wide, indexData = data.index.wide))
}
