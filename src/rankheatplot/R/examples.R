#circos example
run_circos_example <- function(type = 'medium') {
  data <-
    read_excel(paste0("./data/allstudy-", type, ".xlsx"), sheet = 1)
  df <- as.data.frame(data)
  chartData <- rhp.prepData(df)
  rhp.rankheatplotCircos(chartData, "percentage", cexSector=.1)
}
