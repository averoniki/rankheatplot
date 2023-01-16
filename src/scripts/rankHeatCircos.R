#### requirements ############
library(circlize)
library(RColorBrewer)

rhp.rankheatplotCircos <- function(data, format = "percentage") {
  data <- as.data.frame(data)
  chartData <- rhp.prepData(data)
  rns <- rownames(chartData)
  chartData[nrow(chartData) + 1, ] <- colnames(chartData)
  rownames(chartData) <- c(rns, "Outcome")
  pal = brewer.pal(11, "RdYlGn") # will return the hex values
  breaks <-
    seq(0, 100, 10) # assume for now, but need checks later (on format)
  colFun <- colorRamp2(breaks, pal)

  # wrap color function to swallow string values (i.e., labels)
  w_cf <- function(val) {
    clr = 'white'
    clr <- ifelse(!is.na(as.numeric(val)),
                  clr <- colFun(as.numeric(val)),
                  clr)
    clr
  }
  
  m = nrow(chartData)
  # offset start so label row (last row) is at the top
  gapDegree = 2
  startDegree = 90 - (((360 / m) + gapDegree) / 2)

  circos.clear()
  circos.par(start.degree = startDegree, gap.degree = gapDegree)
  
  circos.heatmap(
    chartData,
    # split each row into a separate sector
    split = rownames(chartData),
    col = w_cf,
    cluster = F,
    na.col = "white",
    show.sector.labels = T,
    track.height = .65,
  )
  
  circos.track(
    track.index = get.current.track.index(),
    # this will create graphical elements immediately after creation of cell
    panel.fun = function(x, y) {
      n = ncol(chartData)
      if (get.cell.meta.data("sector.index") != "Outcome") {
        circos.text(
          rep(CELL_META$cell.xlim[2] / 2, n) + convert_x(1, "mm"),
          1:n - 0.5,
          rev(chartData[get.cell.meta.data("sector.index"), ]),
          cex = 0.75,
          adj = c(.5, .5),
          facing = "inside",
          niceFacing = T
        )
      } else {
        # 'bending' requires a loop rather than vector args
        lbls = rev(names(chartData[get.cell.meta.data("sector.index"), ]))
        for (i in 1:m) {
          circos.text(
            CELL_META$cell.xlim[1],
            i - .65,
            lbls[i],
            cex = 0.75,
            adj = c(0, .5),
            facing = "bending",
            niceFacing = T
          )
        }
      }
    },
    bg.border = NA
  )
  
  lgd = ComplexHeatmap::Legend(col_fun = colFun, title = "Legend")
  draw(lgd, x = unit(6, "mm"), y = unit(6, "mm"), just = c("left", "bottom"))
  
}

rhp.prepData <- function(df, format = "percentage") {
  # drop rows that don't have a label
  df <- df[!is.na(df[, 1]),]
  
  # set first column as row names
  rns <- df[, 1]
  # drop first column
  df <- df[,-c(1)]
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

# Shiny options:  
# toggle which outcomes are visible
# select palette (ryg, greyscale, colorblind)
# toggle treatment labels and values
# show outcomes as abbreviations with guide/legend outside plot