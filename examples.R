#### requirements ############
library(readxl)
library(fields)
library(RColorBrewer)
library(circlize)

setwd('/folder/where/this/file/is/')

#percentage example
data <- read_excel("./data/allstudy.xlsx",   sheet = 1)
rankheatplot(data, format="percentage")

#rank example
data <- read_excel("./data/rank.xlsx",   sheet = 1)
rankheatplot(data, format="rank")