library(readxl)
library(netmeta)

source("/home/rstudio/src/scripts/rankHeatCircos.R")

#' @param pth path to excel sheet 
extractSheets <- function(pth) {
  sheets <- excel_sheets(pth)
  output <- list()
  for (i in 1:length(sheets)) {
    df <- read_excel(path = pth, sheet = sheets[i])
    output[[sheets[i]]] <- df
  }
  output
}

#' convert arm to contrast
#' TODO: arguments are currently defaults and need to be parsed properly
#' @param dataType can be: r (?), n, mean, sd, TE, SE
#' @param data dataframe
armToContrast <- function(data, dataType) {
  result <-
    pairwise(
      treat = data[['trt']],
      event = data[['r']],
      n = data[['n']],
      studlab = data[['study']],
      data = data,
      sm = "OR"
    )
  
  cons <-
    netconnection(result[['treat1']], result[['treat2']], result[['studlab']], data = result)
  if (cons$n.subnets > 1) {
    # raise error
  }
  result
}

#' @param data contrast-formatted data
#' @param sm: OR, SMD, RR, HR
#' @param ref: AKA reference.group = the reference treatment (?)
#' @param common: boolean, if true then random is false and vice-versa
#' @param method.tau: rml, ml, dl -- only if random=T or NULL(?)
runNetmeta <- function(data, sm, ref, common, method.tau) {
  netmeta(
    data,
    sm = sm,
    ref = ref,
    common = common,
    random = !common,
    method.tau = method.tau
  )
}

#' @param data results of netmeta (a list)
#' @param method: 'P-Score' | 'SUCRA'
#' @param small.values: 'good' | 'bad'
#' @param common: boolean, if true then random is false and vice-versa
getScores <- function(data, method, small.values, common) {
  # run Pscore, then loop through and get rank and return data.table
  netrank(
    data,
    method = method,
    small.values = small.values,
    common = common,
    random = !common
  )
}

#' @param ranking named vector, typically retrieved from netrank() result as
#' result$ranking.common or result$ranking.random
#' @param treament string
rankToDf <- function(ranking, outcome) {
  df <- data.frame(names(ranking), as.numeric(ranking))
  names(df) <- c("Treatment", outcome)
  df
}

#' @oaram data a dataframe (probably an excel sheet)
#' @oaram outcome string, name of outcome
runWithDefaults <- function(data, outcome) {
  contrastData <- armToContrast(data, 'unused')
  netmetaData <- runNetmeta(contrastData, 'OR', "PLAC", T, "REML")
  rankData <- getScores(netmetaData, 'P-Score', 'good', T)
  # note ranking.common v ranking.random should be option
  rankToDf(rankData$ranking.common, outcome)
}

runNetmetaTest <- function (plt=F) {
  sheets <-
    extractSheets('/home/rstudio/src/data/Safety AD Data RCT.xlsx')
  
  outcomes <- names(sheets)
  i <- 0
  results <- lapply(sheets, function(df) {
    i <<- i + 1
    runWithDefaults(df, outcomes[i])
  })
  
  df <- Reduce(function(a, b) merge(a, b, by = "Treatment", all = TRUE),  results)
  
  if(plt){
    data <- rhp.prepData(df)
    rhp.rankheatplotCircos(data)
  }
  
  df
}
