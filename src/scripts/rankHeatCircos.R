#### requirements ############
library(circlize)
library(RColorBrewer)
library(grid)

# ensure that it contains only numbers and/or NA
isAllNumeric <- function(vec) {
  length(vec[is.na(as.numeric(vec))]) == length(vec[is.na(vec)])
}

#' @param vec a character or numeric vector
formatData <- function(vec) {
  # check all are numeric
  if (isAllNumeric(vec)) {
    round(as.numeric(vec), 0)
  } else {
    vec
  }
}

rhp.rankheatplotCircos <-
  function(chartData, format = "percentage") {
    chartData = as.data.frame(chartData) # will collapse to vector if only one row
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
      if (isAllNumeric(val)) {
        clr <- colFun(as.numeric(val))
      }
      clr
    }
    
    m = nrow(chartData)
    # offset start so label row (last row) is at the top
    gapDegree = 2
    startDegree = 90 - (((360 / m) + gapDegree) / 2)
    
    circos.clear()
    circos.par(
      start.degree = startDegree,
      gap.degree = gapDegree,
      canvas.ylim = c(-1.3, 1)
    )
    
    circos.heatmap(
      cell.border = "grey",
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
      bg.border = NA,
      # this will create graphical elements immediately after creation of cell
      panel.fun = function(x, y) {
        n = ncol(chartData)
        if (get.cell.meta.data("sector.index") != "Outcome") {
          circos.text(
            rep(CELL_META$cell.xlim[2] / 2, n) + convert_x(1, "mm"),
            1:n - 0.5,
            # get data by rowname
            formatData(as.vector(t(
              rev(chartData[get.cell.meta.data("sector.index"), ])
            ))),
            cex = 0.75,
            adj = c(.5, .5),
            facing = "inside",
            niceFacing = T
          )
        } else {
          # facing='bending' requires a loop rather than vector args
          lbls = rev(names(chartData[get.cell.meta.data("sector.index"), ]))
          for (i in 1:m) {
            circos.text(
              CELL_META$cell.xlim[1] + convert_x(1, "mm"),
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
    )
    
    lgd = ComplexHeatmap::Legend(col_fun = colFun, direction = "horizontal")
    ComplexHeatmap::draw(
      lgd,
      x = unit(.5, "npc"),
      y = unit(.1, "npc"),
      just = c("center")
    )
    
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
