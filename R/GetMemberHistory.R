#' Import member history Index Data
#' 
#' @import Rbbg
#' @importFrom reshape2 acast  
#' @note This function requires a lot bloomberg data if the period between stDate and endDate are very long
#' @param indexName The ticker of the index, e.g. SPX Index
#' @param fields A vector of fields
#' @param stDate Starting date
#' @param endDate Ending date
#' @param monthly If montly data is preferred
#' @return A list including the following items
#' \itemize{
#' \item symbols
#' \item fields
#' \item data
#' \item indexData
#' }
GetMemberHistory <- function(indexName, fields, stDate, endDate, monthly=F){
  conn <- blpConnect()
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
    symbols[[i]] <- bds(conn, indexName, "INDX_MWEIGHT_PX", override_fields=c("END_DATE_OVERRIDE"), format(rebalance.date[i], '%Y%m%d'))[,1]
  }
  
  #get the historical data
  total.symbol <- do.call(c, symbols)
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