calculateRegression  <- function(target, predictors, level = 95){
  B = coefsRegression(target, predictors)
  sigma2 = sigma2Regression(target, predictors, B)
  varMatrix = varBRegression(predictors, sigma2)
  ciPar = ciParRegression(B, varMatrix, level, length(target))
  tvalues = tvalueRegression(B, varMatrix)
  pvalues = 2 * pt(tvalues, df = length(target) - length(B))
  return(list(B = B, sigma2 = sigma2, varMatrix = varMatrix, ciPar = ciPar, 
              tvalues = tvalues))
}

coefsRegression <- function(target, predictors){
  
  X = cbind(1, predictors)
  X = as.matrix(X)
  #### B = (X' * X)^-1 * X' * y
  B = solve(t(X) %*% X) %*% t(X) %*% target
  return(B)
}

sigma2Regression <- function(target, predictors, B){
  #### sigma2 = (1 / (n - (p + 1)))  *  (y - XB)' * (y - XB) 
  X = cbind(1, predictors)
  X = as.matrix(X)
  sigma2 = 1 / (length(target) - (length(B))) * 
           t(target - X %*% B) %*% (target - X %*% B)
  return(as.numeric(sigma2))
}

varBRegression <- function(predictors, sigma2){
  X = cbind(1, predictors)
  X = as.matrix(X)
  varMatrix = sigma2 * solve(t(X) %*% X)
  return(varMatrix)
}

ciParRegression <- function(B, varMatrix, level, obs){
  lower = numeric()
  upper = numeric()
  
  for (i in 1:length(B)){
    lower[i] = B[i] - qt((100 + level) / 200, obs - length(B)) * sqrt(varMatrix[i,i])
    upper[i] = B[i] - qt(1 - (100 + level) / 200, obs - length(B)) * sqrt(varMatrix[i,i])
  }
  return(data.frame(lower = lower, upper = upper))
}

tvalueRegression <- function(B, varMatrix){
  tstd = numeric()
  for (i in 1:length(B)){
    tstd[i] = B[i] / sqrt(varMatrix[i,i])
  }
  return(tstd)
}



