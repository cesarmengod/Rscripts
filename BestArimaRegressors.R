BestArimaRegressors <- function(targetTr, targetTe, targetFo, regressorsTr, regressorsTe, regressorsFo){

  library(forecast)
  library(foreach)
  library(MLmetrics)

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
  
  RMSE <- numeric()
  foreach(i = 1:nrow(Grid)) %do% {
    RMSE[i] <- fit.model(targetTr, targetTe, regressorsTr, regressorsTe, Grid, i, 'RMSE')
  }
  
  winner <- which(RMSE == min(RMSE))
  selected <- which(Grid[winner,] == T)
  
  model <- fit.model(targetTr, targetTe, regressorsTr, regressorsTe, Grid, winner, 'model')
  predTe <- forecast(object = model, h = nrow(regTe), xreg = as.matrix(regressorsTe[, selected]))
  predFo <- forecast(object = model, h = nrow(regFo), xreg = as.matrix(regressorsFo[, selected]))
  return(list(features = Grid[winner,], model = model, predTe = predTe, predFo = predFo))
}

