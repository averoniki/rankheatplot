
#
# Main function
#
rankheatplot<-function(data, format="percentage") {
    
    results<-build_components(data, format)
      
    cex<-drawDonughts(results$components , results$numOfOutcomes)  
     
    gen_intervals<-generateIntervals(results$df, format ) 
    
    addColorBackground(results$outcomes, components=results$components, data=results$df, gen_intervals$intervals, gen_intervals$mycolors, cex=cex)  #add the background color on cells

}

nnt<-function(data, format){
  firstrow<-data[,1]
}
  
#
# Build network components
#
build_components<-function(data, format){
  df<-as.data.frame(data) #data as data frame
  if(format=="nnt"){
    outcomes <- df[  !is.na(df[[1]]) , 1]
    
  }else{
    colnames(df)[1]<-"" 
    outcomes<- colnames(df)[-1]
    components <- c(df[[1]])
    rownames(df) <- c(components)
    df<-df[,-1]
  }
  
 
  numOfOutcomes<- length(outcomes)
  numOfcomponents<- length(components)
  
  dat<-convertData(df, format, numOfcomponents, numOfOutcomes)
  dat<-list(df= dat,components= components, outcomes=outcomes, numOfOutcomes = numOfOutcomes )
  
  return(dat)
}



#
# Convert data depending the format
#
convertData<-function(df, format, numOfcomponents, numOfOutcomes){
  if(format == "percentage"){
    for(i in 1:numOfcomponents){
      for(j in 1:numOfOutcomes){
        df[i,j]<- round(df[i,j] *100 , 0)
      }
    }
    
  }else if(format == "nnt"){
    df<-df
    
  }else if(format == "rank"){
  
    for(j in 1:numOfOutcomes){ 
        lengthoftreatment<-sum(!is.na(df[j]))
      
        for(i in 1:numOfcomponents){
       
            if(!is.na(df[i,j]) && df[i,j]==1){
                df[i,j]<-100
            }else{
                df[i,j]<-round( (1-df[i,j]/lengthoftreatment)*100, 0)
            }
        }
    }

  }else{
    
    stop("Please select a valid format: 'percentage', 'nnt', 'rank'", call. = FALSE)
  }
  
  return(df)
}


#
# Draw rankheatplot donughts
#
drawDonughts <- function(components, numOfOutcomes ) {
  circos.clear() 
  
  if(numOfOutcomes==1|numOfOutcomes==2){
    trHigh = 0.3 
    cex = 0.8
  }else if(numOfOutcomes==3|numOfOutcomes==4){
    trHigh = 0.17 
    cex = 0.8
  }else if(numOfOutcomes>=5 & numOfOutcomes<7){
    trHigh = 0.11
    cex = 0.6
  }else if(numOfOutcomes==7){
    trHigh = 0.09 
    cex = 0.6
  }else if(numOfOutcomes==8 | numOfOutcomes==9){
    trHigh = 0.07 
    cex = 0.6
  }else if(no>=10){
    trHigh = 0.05 
    cex = 0.4
  }
 
  addComponent<- c("Outcomes", components)
  circos.par(points.overflow.warning = FALSE, track.margin=c(0,0), start.degree = 100)
  circos.initialize(factors = addComponent, xlim=c(0, 10))
  
  circos.trackPlotRegion(addComponent, ylim = c(0, 100),  bg.border = NA, track.height = 0.1, panel.fun = function(x, y) {circos.text(5, 100, facing = "bending", cex = cex, get.cell.meta.data("sector.index"))})
  
  for (i in 1 : numOfOutcomes){
    circos.trackPlotRegion(addComponent, ylim = c(0,100), bg.border = NA, track.height = trHigh)
  }
  return(cex)
}



#
# Generate intervals
#
generateIntervals<-function (data, format){
  #dat<-abs(data)
  #maxim <- round(max(apply(dat, 2, max, na.rm = TRUE)),0)  
  # intervals<-seq(from = -maxim, to = maxim, (maxim *2 )/32 )
  intervals<-c(0, 3.1, 6.2, 9.3, 12.4, 15.5, 18.6, 21.7,  24.8, 27.9, 31, 34.1, 37.2, 40.3, 43.4, 46.5, 49.6, 52.7, 55.8, 58.9, 62, 65.1, 68.2, 71.3, 74.4, 77.5, 80.6, 83.7, 86.8 , 89.9, 93, 96.1, 100)
  mycolors<-c('#e00000','#ef0000',"#FF0000","#FF3000","#FF4000","#FF5000","#FF6000","#FF7000","#FF8000","#FF9000","#FFA000","#FFB000","#FFC000","#FFD000","#FFE000","#FFF000","#FFFF00","#F0FF00","#d0ff00","#C0FF00","#B0FF00","#A0FF00","#90FF00","#70FF00","#50FF00","#0ffc00","#0ff200","#0ee800","#0ee000","#0ed800","#0ecc00","#0dc100")
  gen_intervals<-list(intervals= intervals,mycolors= mycolors)
  return(gen_intervals)
}




#
# rad background color
#
addColorBackground<- function(outcomes, components, data, intervals, mycolors, cex ){
    
    for(k in 1:length(outcomes)){
      start = get.cell.meta.data("cell.start.degree", "Outcomes",  k+1)
      end = get.cell.meta.data("cell.end.degree", "Outcomes", k+1)
      top = get.cell.meta.data("cell.top.radius", "Outcomes", k+1)
      bottom = get.cell.meta.data("cell.bottom.radius", "Outcomes", k+1)
      draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,  border = NA)
      circos.text(5, 50, sector.index = "Outcomes",facing = "downward", track.index= k+1, labels=outcomes[k], cex=cex)
    }
    
    for(i in 1:dim(data)[1]){ 
      for(j in 1:length(outcomes)){  
      
          start = get.cell.meta.data("cell.start.degree", components[i],  j+1)
          end = get.cell.meta.data("cell.end.degree", components[i], j+1)
          top = get.cell.meta.data("cell.top.radius", components[i], j+1)
          bottom = get.cell.meta.data("cell.bottom.radius", components[i], j+1)
          
          if(is.na(data[i,j])==TRUE){		#case data is NA: draw a white sector
            draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,  border = "#f2f2f2")
            
          }else{	
              for(k in 1:32){
                  if(as.numeric(data[i,j])>=intervals[k] && as.numeric(data[i,j])<intervals[k+1]){
                    draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom, border = NA,  col = mycolors[k])	 
                  }
              }
              circos.text(5, 50, sector.index = components[i],facing = "downward", track.index= j+1, labels=round(data[i,j],2),cex=cex)
          }
      }
    }
}

