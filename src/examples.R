library(readxl)
library(ComplexHeatmap)
setwd('/home/rstudio/src')

source('/home/rstudio/src/fun/rankheatPlotComp.R')
source('/home/rstudio/src/fun/rankHeatCircos.R')

#percentage example
run_pct_example <- function() {
  data <- read_excel("./data/allstudy.xlsx",   sheet = 1)
  rankheatplot(data, format = "percentage")
}

#rank example
run_rank_example <- function() {
  data <- read_excel("./data/rank.xlsx",   sheet = 1)
  rankheatplot(data, format = "rank")
}

#rank example
run_nnt_example <- function() {
  data <- read_excel("./data/allstudy.xlsx",   sheet = 1)
  rankheatplot(data, format = "nnt")
}

#circos examplse
run_circos_example <- function() {
  data <- read_excel("./data/allstudy.xlsx",   sheet = 1)
  rankheatplotCircos <- rankheatplotCircos(data, "percentage")
}

run_all_examples <- function() {
  run_nnt_example()
  run_pct_example()
  run_rank_example()
}