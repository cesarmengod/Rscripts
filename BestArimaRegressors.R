BestArimaRegressors <- function(targetTr, targetTe, targetFo, regressorsTr, regressorsTe, regressorsFo, ncores = 1){

  library(forecast)
  library(foreach)
  library(doParallel)
  
  Nregressors <- ncol(regressorsTr)
  argsGrid <- gsub(',$', '', paste0(rep('0:1,',Nregressors), collapse = ''))
  exprGrid <- paste0('expand.grid(', argsGrid, ')[-1,]')
  
  Grid <- eval(parse(text = exprGrid))
  names(Grid) = names(regressorsTe)
  
  fit.model <- function(targetTr, targetTe, regressorsTr, regressorsTe, Grid, i, output = ''){
    regTr <- regressorsTr[,which(Grid[i,]==T)]
    regTe <- regressorsTe[,which(Grid[i,]==T)]
    model <- auto.arima(y = targetTr, xreg = as.matrix(regTr))
    predTe <- forecast(object = model, h = nrow(regTe), xreg = as.matrix(regTe))$mean
    RMSE <- mean((as.numeric(targetTe) - as.numeric(predTe))^2)
    if (output == 'RMSE'){return(RMSE)} else {return(model)}
  }
  
  if (ncores == 1){ 
    RMSE <- foreach(i = 1:nrow(Grid)) %do% {fit.model(targetTr, targetTe, regressorsTr, regressorsTe, Grid, i, 'RMSE')}
  }
  else {
    cl <- makeCluster(min(ncores, detectCores() - 1))
    registerDoParallel(cl)
    #clusterExport(cl = cl, c('forecast', 'auto.arima', 'fit.model'))
    RMSE <- foreach(i = 1:nrow(Grid), .packages=c("forecast")) %dopar% {fit.model(targetTr, targetTe, regressorsTr, regressorsTe, Grid, i, 'RMSE')}
    stopCluster(cl)
  }
  
  RMSE = unlist(RMSE)  
  winner <- which(RMSE == min(RMSE))
  selected <- which(Grid[winner,] == T)
  
  model <- fit.model(targetTr, targetTe, regressorsTr, regressorsTe, Grid, winner, 'model')
  predTe <- forecast(object = model, h = nrow(regTe), xreg = as.matrix(regressorsTe[, selected]))
  predFo <- forecast(object = model, h = nrow(regFo), xreg = as.matrix(regressorsFo[, selected]))
  return(list(features = Grid[winner,], model = model, predTe = predTe, predFo = predFo))
}
