# this will deploy with R alone
# e.g., Rscript deploy.R
library(devtools)
load_all()
if(! is.na(Sys.getenv()['SHINY_PORT'])){
  rankHeatApp(Sys.getenv()['SHINY_PORT'])
} else {
  rankHeatApp()
}

