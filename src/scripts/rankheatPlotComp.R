#### requirements ############
library(circlize)
library(RColorBrewer)

# Main function
rankheatplot <- function(data, format = "percentage") {
  chartData <- prepareData(data, format)
  
  cex <- drawDonughts(chartData$treatments , chartData$outcomeCount)
  
  # we know the domain is (0,100), but in the future we'll want to compute it on the fly
  pal = brewer.pal(11, "RdYlGn") # will return the hex values
  breaks <- seq(0, 100, 10) # say that 0,1 is our domain
  col_func <- colorRamp2(breaks, pal)
  
  # add color
  addColorBackground(
    chartData$outcomes,
    treatments = chartData$treatments,
    data = chartData$df,
    col_func,
    cex = cex
  )
  
}

nnt <- function(data, format) {
  firstrow <- data[, 1]
}


#
# Build network treatments
# format can be "percentage","nnt","rank"
prepareData <- function(data, format) {
  df <- as.data.frame(data) #data as data frame
  df <- df[!is.na(df[, 1]),] # drop rows that don't have a label
  if (format == "nnt") {
    # return column 1 as a vector (treatment labels), minus nulls
    outcomes <- df[!is.na(df[[1]]) , 1]
    treatments <- c(df[[1]])
    
  } else {
    # remove treatment colname and get vector of outcome labels
    colnames(df)[1] <- ""
    outcomes <- colnames(df)[-1]
    # treatment labels are 'treatments'
    treatments <- c(df[[1]]) 
    # move treatment labels into rownames and drop treatment label column
    rownames(df) <- treatments
    df <- df[, -1]
  }
  
  outcomeCount <- length(outcomes)
  treatmentCount <- length(treatments)
  
  dat <- transformNumbers(df, format, treatmentCount, outcomeCount)
  dat <-
    list(
      df = dat,
      treatments = treatments,
      outcomes = outcomes,
      outcomeCount = outcomeCount
    )
  
  return(dat)
}

#
# Convert data depending the format
#
transformNumbers <- function(df,
                             format,
                             treatmentCount,
                             outcomeCount) {
  if (format == "percentage") {
    for (i in 1:treatmentCount) {
      for (j in 1:outcomeCount) {
        df[i, j] <- round(df[i, j] * 100 , 0)
      }
    }
    
  } else if (format == "nnt") {
    df <- df
    
  } else if (format == "rank") {
    for (j in 1:outcomeCount) {
      lengthoftreatment <- sum(!is.na(df[j]))
      
      for (i in 1:treatmentCount) {
        if (!is.na(df[i, j]) && df[i, j] == 1) {
          df[i, j] <- 100
        } else{
          df[i, j] <- round((1 - df[i, j] / lengthoftreatment) * 100, 0)
        }
      }
    }
    
  } else{
    stop("Please select a valid format: 'percentage', 'nnt', 'rank'",
         call. = FALSE)
  }
  
  return(df)
}


#
# Draw rankheatplot donughts
#
drawDonughts <- function(treatments, outcomeCount) {
  circos.clear()
  
  if (outcomeCount <= 2) {
    trHigh = 0.3
    cex = 0.8
  } else if (outcomeCount <= 4) {
    trHigh = 0.17
    cex = 0.8
  } else if (outcomeCount < 7) {
    trHigh = 0.11
    cex = 0.6
  } else if (outcomeCount == 7) {
    trHigh = 0.09
    cex = 0.6
  } else if (outcomeCount <= 9) {
    trHigh = 0.07
    cex = 0.6
  } else if (no >= 10) {
    trHigh = 0.05
    cex = 0.4
  }
  
  addtreatment <- c("Outcomes", treatments)
  circos.par(
    points.overflow.warning = FALSE,
    track.margin = c(0, 0),
    start.degree = 100
  )
  circos.initialize(factors = addtreatment, xlim = c(0, 10))
  
  circos.trackPlotRegion(
    addtreatment,
    ylim = c(0, 100),
    bg.border = NA,
    track.height = 0.1,
    panel.fun = function(x, y) {
      circos.text(5,
                  100,
                  facing = "bending",
                  cex = cex,
                  get.cell.meta.data("sector.index"))
    }
  )
  
  for (i in 1:outcomeCount) {
    circos.trackPlotRegion(
      addtreatment,
      ylim = c(0, 100),
      bg.border = NA,
      track.height = trHigh
    )
  }
  return(cex)
}

#
# rad background color
#
addColorBackground <-
  function(outcomes,
           treatments,
           data,
           col_func,
           cex) {
    for (k in 1:length(outcomes)) {
      start = get.cell.meta.data("cell.start.degree", "Outcomes",  k + 1)
      end = get.cell.meta.data("cell.end.degree", "Outcomes", k + 1)
      top = get.cell.meta.data("cell.top.radius", "Outcomes", k + 1)
      bottom = get.cell.meta.data("cell.bottom.radius", "Outcomes", k +
                                    1)
      draw.sector(
        start.degree = start,
        end.degree = end,
        rou1 = top,
        rou2 = bottom,
        border = NA
      )
      circos.text(
        5,
        50,
        sector.index = "Outcomes",
        facing = "downward",
        track.index = k + 1,
        labels = outcomes[k],
        cex = cex
      )
    }
    
    for (i in 1:dim(data)[1]) {
      for (j in 1:length(outcomes)) {
        start = get.cell.meta.data("cell.start.degree", treatments[i],  j + 1)
        end = get.cell.meta.data("cell.end.degree", treatments[i], j +
                                   1)
        top = get.cell.meta.data("cell.top.radius", treatments[i], j +
                                   1)
        bottom = get.cell.meta.data("cell.bottom.radius", treatments[i], j +
                                      1)
        
        if (length(data[i, j]) > 0 && is.na(data[i, j]) == TRUE) {
          #case data is NA: draw a white sector
          draw.sector(
            start.degree = start,
            end.degree = end,
            rou1 = top,
            rou2 = bottom,
            border = "#f2f2f2"
          )
          
        } else{
          for (k in 1:32) {
            if (length(data[i, j]) > 0 &&
                !is.na(as.numeric(data[i, j]))) {
              draw.sector(
                start.degree = start,
                end.degree = end,
                rou1 = top,
                rou2 = bottom,
                border = NA,
                col = col_func(as.numeric(data[i, j]))
              )
            }
          }
          circos.text(
            5,
            50,
            sector.index = treatments[i],
            facing = "downward",
            track.index = j + 1,
            labels = round(as.numeric(data[i, j]), 2),
            cex = cex
          )
        }
      }
    }
  }
