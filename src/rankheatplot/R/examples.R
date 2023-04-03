#circos example
run_circos_example <- function(type = 'medium', test_file_path = "/home/rstudio/src/data/allstudy-small.xlsx") {
  data <- readxl::read_excel(test_file_path, sheet = 1)
  df <- as.data.frame(data)
  chartData <- rhp.prepData(df)
  rhp.rankheatplotCircos(chartData, "percentage", cexSector=1)
}
