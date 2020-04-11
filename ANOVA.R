
funANOVA <- function(dataset){
  stat = calculateStatistics(dataset)
  
  ### Sumas de cuadrados
  SCF = calculateSCF(dataset)
  SCR = calculateSCR(dataset)
  
  ### Grados de libertad
  glSCF = stat$vars - 1
  glSCR = stat$allObs - stat$vars
  
  ### Medias Cuadraticas
  MCF = SCF / glSCF
  MCR = SCR / glSCR
  
  statF = MCF / MCR
  
  pvalue = 1 - pf(q = statF, df1 = glSCF, df2 = glSCR)
  
  return(list(MC = c(MCF, MCR), statF = statF, pvalue = pvalue))
}

calculateStatistics <- function(dataset){
  varMean = colMeans(dataset, na.rm = T)
  allObs = sum(obsGroup)
  vars = ncol(dataset)
  obsGroup = colSums(!apply(dataset, 2, is.na))
  allMean = sum(colSums(dataset, na.rm = T)) / allObs
  return(list(varMean = varMean, obsGroup = obsGroup, allMean = allMean, 
              allObs = allObs, vars = vars))
}

calculateSCT <- function(dataset){
  # Suma de cuadrados total
  stat = calculateStatistics(dataset)
  return(sum((dataset - stat$allMean)**2, na.rm = T))
}

calculateSCF <- function(dataset){
  # Suma de cuadrados del factor o variabilidad explicada
  stat = calculateStatistics(dataset)
  sum(stat$obsGroup * (stat$varMean - stat$allMean) ** 2)
}

calculateSCR <- function(dataset){
  # Suma de cuadrados residual o variabilidad residual
  stat = calculateStatistics(dataset)
  return(sum((t(dataset) - stat$varMean)**2, na.rm = T))
}

