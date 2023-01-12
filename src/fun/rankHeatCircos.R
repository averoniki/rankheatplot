#### requirements ############
library(circlize)
library(RColorBrewer)

rankheatplotCircos <- function(data, format = "percentage") {
  data <- as.data.frame(data)
  chartData <- prepData(data)
  rns <- rownames(chartData)
  chartData[nrow(chartData) + 1,] <- colnames(chartData)
  rownames(chartData) <- c(rns, "Treatment")
  pal = brewer.pal(11, "RdYlGn") # will return the hex values
  breaks <-
    seq(0, 100, 10) # assume for now, but need checks later (on format)
  col_func <- colorRamp2(breaks, pal)
 
  # wrap color function to swallow string values (i.e., labels) 
  w_cf <- function(val) {
    clr = 'white'
    clr <- ifelse(!is.na(as.numeric(val)),
                  clr <- col_func(as.numeric(val)),
                  clr)
    clr
    
  }
  
  circos.clear()
  circos.par(start.degree = 90, gap.degree = 2)
  
  circos.heatmap(
    chartData,
    # split each row into a separate sector
    split = rownames(chartData),
    col = w_cf,
    cluster = F,
    na.col = "white",
    show.sector.labels = T,
    track.height = .75,

  )
  
  circos.track(
    track.index = get.current.track.index(),
    # this will create graphical elements immediately after creation of cell
    panel.fun = function(x, y) {
      n = ncol(chartData)
      # todo: need min width, smaller font, curve for labels
      # adjust start angle based on number of rows for better label row positioning
      circos.text(
        rep(CELL_META$cell.xlim[2] / 2, n) + convert_x(1, "mm"),
        1:n - 0.5,
        rev(chartData[get.cell.meta.data("sector.index"),]),
        cex = 0.75,
        adj = c(.5, 0),
        facing = "inside",
        niceFacing = T
      )
    },
    bg.border = NA
  )
  
}
prepData <- function(df, format = "percentage") {
  # drop rows that don't have a label
  df <- df[!is.na(df[, 1]), ]
  
  # set first column as row names
  rns <- df[, 1]
  # drop first column
  df <- df[, -c(1)]
  if (format == "percentage") {
    df <- as.data.frame(sapply(df, function(x)
      x * 100))
  } else if (format == "rank") {
    df <- as.data.frame(sapplu(df, function(x)
      (1 - x) / nrow(df)))
  }
  rownames(df) <- rns
  df
}