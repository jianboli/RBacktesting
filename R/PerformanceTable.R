#'Summarize the performance of serveral portfolio
#'
#'@param res 
#'@param perf
#'@param size
#'@param index.pnl
#'@import matrixStats
#'@note  TODO: need some work to standarlize it 
PerformanceTable <- function(res, perf, size, index.pnl){
  nport <- ncol(perf)
  # information ratio
  IR <- apply(perf, 2, function(x){return(InformationRatio(x, index.pnl[pred.idx])*sqrt(12))})
  aum <- apply(perf+1, 2, cumprod)
  MDD <- apply(perf, 2, function(x){MaxDrawdown(cumprod(x+1))$maxdrawdown})*100
  turnOver <- sapply(as.list(1:nport), 
                     function(j){
                       mean(sapply(as.list(1:(length(res)-1)), 
                                   function(i){
                                     TurnOver(res[[i]]$ptf[[j]], res[[i+1]]$ptf[[j]], aum[i,j], aum[i+1,j])
                                   }
                       )
                       )
                     }
  ) 
  
  
  table <- data.frame(Port = 1:nport, 
                      AvgRet = colMeans(perf)*100, 
                      AnnRet = colMeans(perf)*12*100, 
                      AnnStDev = colSds(perf)*sqrt(12)*100, 
                      Size = colMeans(size),
                      Mdd = MDD,
                      IR = IR,
                      TurnOver = turnOver*12
  )
  colnames(table) <- c("Port","Month Ret (%)","Ann Ret (%)", "Ann St Dev(%)", "Avg Size", "MDD (%)", "IR", "Turnover")
  return(table)
}