# ensure that it contains only numbers and/or NA
isAllNumeric <- function(vec) {
  length(vec[is.na(as.numeric(vec))]) == length(vec[is.na(vec)])
}

#' Format the data for display
#' @param vec a character or numeric vector
formatData <- function(vec) {
  # check all are numeric
  if (isAllNumeric(vec)) {
    round(as.numeric(vec), 0)
  } else {
    vec
  }
}
#' Plot the rank-heat graphic
#' @param chartData, dataframe
#' @param format, chr
#' @param cexLabel, float, scale of label text
#' @param cexValue, float, scale of value text
#' @param cexSector, float, scale of sector text
#' @return void
#' @export
rhp.rankheatplotCircos <-
  function(chartData,
           format = "percentage",
           cexLabel = .65,
           cexValue = .75,
           cexSector = 1) {
    rns <- rownames(chartData)
    # add explicit row for outcomes
    chartData[nrow(chartData) + 1, ] <- colnames(chartData)
    rownames(chartData) <- c(rns, "Outcome")
    pal = RColorBrewer::brewer.pal(11, "RdYlGn") # will return the hex values
    breaks <-
      seq(0, 100, 10) # assume for now, but need checks later (on format)
    colFun <- circlize::colorRamp2(breaks, pal)

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

    circlize::circos.clear()
    circlize::circos.par(
      start.degree = startDegree,
      gap.degree = gapDegree,
      canvas.ylim = c(-1.3, 1.3),
      points.overflow.warning = FALSE
    )

    circlize::circos.heatmap(
      cell.border = "grey",
      chartData,
      # split each row (outcome) into a separate sector
      split = rownames(chartData),
      col = w_cf,
      cluster = F,
      na.col = "white",
      track.height = .65,
    )

    circlize::circos.track(
      track.index = circlize::get.current.track.index(),
       bg.border = NA,
       #this will create graphical elements immediately after creation of cell
       panel.fun = function(x, y) {
         n = ncol(chartData)

         if (circlize::get.cell.meta.data("sector.index") != "Outcome") {
           circlize::circos.text(
             rep(circlize::CELL_META$cell.xlim[2] / 2, n),
             1:n - 0.5,
             # get data by rowname
             formatData(as.vector(t(
               rev(chartData[circlize::get.cell.meta.data("sector.index"), ])
             ))),
             cex = cexValue,
             adj = c(.5, .5),
             facing = "inside",
             niceFacing = T
           )
         } else {
           # facing='bending' requires a loop rather than vector args
           lbls = rev(names(chartData))
           for (i in 1:n) {
             circlize::circos.text(
               circlize::CELL_META$cell.xlim[2] / 2 ,
               i - .5,
               lbls[i],
               cex = cexLabel,
               adj = c(.5, .5),
               facing = "bending",
               niceFacing = T
             )
           }
         }
      },
    )

    # we'll roll our own sector labels so we have control over cex
     for (nm in rownames(chartData)) {
       circlize::set.current.cell(sector.index = nm, track.index = 1)

       circlize::circos.text(
         circlize::CELL_META$xcenter,
         circlize::CELL_META$ylim[2] + 0.1,
         nm,
         facing = "bending.inside",
         adj = c(0.5, 0),
         cex=cexSector
       )

     }

    lgd = ComplexHeatmap::Legend(col_fun = colFun, direction = "horizontal")
    ComplexHeatmap::draw(
      lgd,
      x = grid::unit(.5, "npc"),
      y = grid::unit(.1, "npc"),
      just = c("center")
    )

  }

rhp.prepData <- function(df, format = "percentage") {
  # drop rows that don't have a label
  df <- df[!is.na(df[, 1]),]
  # save these in case they get dropped in
  #colns <- names(df)[0:-1]
  # set first column as row names
  rns <- df[, 1]

  # drop first column (if only has one col, will be converted to vec, so cast and set names...)
  df <- as.data.frame(df[,-c(1), drop=F])

  if (format == "percentage") {
    df <- as.data.frame(sapply(df, function(x)
      x * 100))
  } else if (format == "rank") {
    df <- as.data.frame(sapply(df, function(x)
      (1 - x) / nrow(df)))
  }
  #colnames(df) <- colns
  rownames(df) <- rns
  df
}
