groupByDT <- function(df, byVars, valuesVar, funct, nameNewVar = 'result'){
  byVars <- paste0(byVars, collapse = ',')
  expr <- paste0("setDT(df)[,.(", nameNewVar, " = ", funct, "(", valuesVar, ")), by = ", "'", byVars, "']")
  return(eval(parse(text = expr)))
}
