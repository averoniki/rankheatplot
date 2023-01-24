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
#'
#' The data can be categorized in three ways: Binary, Continuous, Time-to-event
#' Category of data will affect the arguments passed to `pairwise`
#' 1. Binary: event = r, n = n
#' Data.pair <- pairwise(treat = t, event = r, n = n, studlab = id, data= Data, sm="OR")
#' 2. Continuous: mean = m, std.dev = sd, n = n
#' Data.pair <- pairwise(treat = t, n = n, mean = m, sd = sd, studlab = id, data=Data, sm="MD")
#' 3. time-to-even: events = d, time = time
#' Data.pair <- pairwise(treat = t, event = d, time = time, studlab = id, data=Data, sm="IRR") 
#' For all, treatment = t, data = data, studlab = id
#' 
#' note that all of these arguments are columns
#'
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
  result
}

#' @param data contrast-formatted data
#' @param sm OR, SMD, RR, HR, RD, MD, ROM, IRR
#' Note: the above will depend on data type:
#' Binary: OR, RR, RD
#' Continuous: MD, SMF, ROM
#' Time-to-even: IRR
#' Survival: HR
#' @param common boolean, if true then random is false and vice-versa
#' @param method.tau rml, ml, dl -- only if random=T or NULL(?)
#' @param small.values
runNetmeta <- function(data, sm, ref, common, method.tau) {
  netmetaRes <- netmeta(
    data,
    sm = sm,
    common = common,
    random = !common,
    method.tau = method.tau
  )
  
  cons <-
    netconnection(netmetaRes[['treat1']], netmetaRes[['treat2']], netmetaRes[['studlab']], data = netmetaRes)
  if (cons$n.subnets > 1) {
    stop("User should update their data!")
  }
  
  netmetaRes
  
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

#' @oaram data a dataframe
#' @oaram outcome string, name of outcome
runWithDefaults <- function(data, outcome) {
  contrastData <- armToContrast(data, 'unused')
  netmetaData <- runNetmeta(contrastData, 'OR', "PLAC", T, "REML")
  
  rankData <- getScores(netmetaData, 'P-Score', 'good', T)
  # note ranking.common v ranking.random should be option
  rankToDf(rankData$ranking.common, outcome)
}

#' Sanity check for our test data
runNetmetaTest <- function (plt = F) {
  sheets <-
    extractSheets('/home/rstudio/src/data/Safety AD Data RCT.xlsx')
  
  outcomes <- names(sheets)
  i <- 0
  results <- lapply(sheets, function(df) {
    i <<- i + 1
    runWithDefaults(df, outcomes[i])
  })
  
  df <-
    Reduce(function(a, b)
      merge(a, b, by = "Treatment", all = TRUE),  results)
  
  if (plt) {
    data <- rhp.prepData(df)
    rhp.rankheatplotCircos(data)
  }
  
  df
}
