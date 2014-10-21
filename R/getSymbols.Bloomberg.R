#'@import quantmod
#'@import Rbbg
loadSymbols <- getSymbols
loadSymbols.formals <- c(formals(getSymbols)[-(8:9)], alist(auto.assign=getOption("loadSymbols.auto.assign",TRUE),...=))

# Copied from a comment out code of getSymbols.Bloomberg of quantmod
formals(loadSymbols) <- loadSymbols.formals
#' Get symbols from Bloomberg. This function is not suppose to be called directly, but to be called by getSymbols of quantmod
#' 
#'@param Symbols a list of Bloomberg tickers, for consistency with other load symbols method in quantmod, it should be only the name of the tickers
#'@param env where to create objects. (.GlobalEnv)
#'@param return.class class of returned object
#'@param from Retrieve no earlier than this date
#'@param to Retrieve though this date 
#'@param bb.suffix The Bloomberg yellow ticker
#'@param bb.interval Time interval for intraday data
#'@param ... Other parameters passed in by getSymbols
#'@return Depends on auto.assing
#'@export
"getSymbols.Bloomberg" <- function(Symbols,env,return.class='xts',
                                   #from=as.POSIXlt(Sys.time()-60*60,"GMT"),
                                   #to=as.POSIXlt(Sys.time(),"GMT"),
                                   from=as.Date('2007-01-01'),
                                   to=Sys.Date(),
                                   bb.suffix="Equity",
                                   bb.interval="5",
                                   ...) {
  importDefaults("getSymbols.Bloomberg")
  this.env <- environment()
  for(var in names(list(...))) {
    # import all named elements that are NON formals
    assign(var, list(...)[[var]], this.env)
  }
  if ((class(from)=="Date" && class(to)=="Date") ||
        (class(from)=="character" && length(from)<=8 &&
           class(to)=="character" && length(to)<=8 )) {
    bb.intraday <- FALSE
    bb.call <- bdh
    bb.fields <- c("OPEN", "HIGH", "LOW", "PX_LAST", "VOLUME", "TOT_RETURN_INDEX_GROSS_DVDS")
  } else {
    bb.intraday <- TRUE
    bb.call <- bar
    bb.fields <- "TRADE"
  }
  if(missing(verbose)) verbose <- FALSE
  if(missing(auto.assign)) auto.assign <- TRUE
#  if('package:Rbbg' %in% search() || require('Rbbg',quietly=TRUE)) {
#{}
#  } else {
#    stop(paste("package:",dQuote('Rbbg'),"cannot be loaded."))
#  }
bbconn <- blpConnect()
for(i in 1:length(Symbols)) {
  bbsym <- paste(Symbols[[i]],bb.suffix)
  if(verbose) {
    cat(paste('Loading ',bbsym, ' from BB ', from,' to ',to,
              paste(rep('.',18-nchar(Symbols[[i]])),collapse=''),
              sep=''))
  }
  tryCatch (
{
  if (bb.intraday) {
    fromStr <- paste(as.character(from),".000",sep="")
    toStr <- paste(as.character(to),".000",sep="")
    b <- bb.call(bbconn, bbsym, bb.fields,
                 fromStr, toStr, bb.interval)
    b$datetime <- as.POSIXct(strptime(b$time, format="%Y-%m-%dT%H:%M:%S"))
    bxo <- as.xts(b$open, order.by=b$datetime)
    fr <- merge(bxo,  b$high, b$low, b$close, b$volume)
  } else {
    if (class(from)=="character") {
      fromStr <- from
    } else {
      fromStr <- strftime(from,format="%Y%m%d")
    }
    if (class(to)=="character") {
      toStr <- to
    } else {
      toStr <- strftime(to,format="%Y%m%d")
    }
    
    b <- bb.call(bbconn, bbsym, bb.fields,
                 fromStr, toStr)
    b$datetime <- as.POSIXct(strptime(b$date, format="%Y-%m-%d"))
    bxo <- as.xts(b$OPEN, order.by=b$datetime)
    fr <- merge(bxo,  b$HIGH, b$LOW, b$PX_LAST, b$VOLUME, b$TOT_RETURN_INDEX_GROSS_DVDS)
  }
  if(verbose) {
    cat(paste(length(fr),'points '))
  }
  colnames(fr) <- paste(Symbols[[i]],
                        c('Open','High','Low','Close','Volume', "Adjusted"),
                        sep='.')
  fr <- quantmod:::convert.time.series(fr=fr,return.class=return.class)
  if(auto.assign)
    assign(Symbols[[i]],fr,env)
},
error=function(e) {print(e);fr <- data.frame()},
finally=function () {if(verbose) {cat('done\n')}}
  )
}
blpDisconnect(bbconn)
if(auto.assign)
  return(Symbols)
return(fr)
}
