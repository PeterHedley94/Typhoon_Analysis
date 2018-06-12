#02_25_Create_Paths.R

#####################################################################################################

get_LL <- function(storm_path,DLO,DLA,LL){
  #new longitude is old long + change in long predicted by regression
  if (LL == 1){
    Longitude <- tail(storm_path,1)$Longitude + DLO
    return(Longitude)
    
  }else{
    Latitude <- tail(storm_path,1)$Latitude + DLA
    return(Latitude)
  }
}


#####################################################################################################

create_paths <- function(gen_typhoons,regression_model){

  count = 1
  
  for (storm in c(1:dim(gen_typhoons))){
    #for each storm in the generated data
    count2 <- 0
    storm_path <- gen_typhoons[storm,]
    storm_path$ID <- storm
    print(paste('Storm Number: ' , storm, sep = ''))
    
    #Long and lat conatin the values to look up where the storm is on the grid created earlier 02_24
    Long = round_any(tail(storm_path,1)$Longitude, granularity , f= floor)
    Lat = round_any(tail(storm_path,1)$Latitude, granularity, f= floor)
    
    #return the regression model for that particular cell
    data <- regression_model[[as.character(Long)]][[as.character(Lat)]]
  
    #as of yet there is no way to determine if the storm has lost all of its energy/ point of death
    while(length(data) > 0 & count2<600){
      
      #select the relevant models from that cell
      DLA_model <- data[['DLA']]
      DLO_model <- data[['DLO']]
      I_model <- data[['IM']]
      WKT_model <- data[['WKTM']]
      
      count2 <- count2 +1
      
      #predict the next values from the last set of values created. then iterate through.
      #create new events then predict off those new events etc.
      DLA <- (predict(DLA_model,tail(storm_path,1)))
      DLA6 <- (tail(storm_path,1)['DLA'])$DLA
      DLA12 <- (tail(storm_path,1)['DLA6'])$DLA6
      DLO <- predict(DLO_model,tail(storm_path,1))
      DLO6 <- tail(storm_path,1)['DLO']$DLO
      DLO12 <- tail(storm_path,1)['DLO6']$DLO6
      I <- predict(I_model,tail(storm_path,1))
      I6 <- tail(storm_path,1)['I']$I
      I12 <- tail(storm_path,1)['I6']$I6
      WKT <- predict(WKT_model,tail(storm_path,1))
      WKT6 <- tail(storm_path,1)['WKT']$WKT
      WKT12 <- tail(storm_path,1)['WKT6']$WKT6
      
      
      Month <- tail(storm_path,1)['Month']$Month
      ID <- tail(storm_path,1)['ID']$ID
      
      Longitude <- get_LL(storm_path,DLO,DLA,1)
      Latitude <- get_LL(storm_path,DLO,DLA,2)
      
      tempdf <- data.frame(Longitude,Latitude,Month,DLO,DLO6,DLO12,DLA,DLA6,DLA12,I,I6,I12,WKT,WKT6,WKT12,ID)
      storm_path <- rbind(storm_path,tempdf)
      
      #Now check if the cell has changed so use a different regression model
      Long = round_any(tail(storm_path,1)$Longitude, granularity , f= floor)
      Lat = round_any(tail(storm_path,1)$Latitude, granularity , f= floor)
      data <- regression_model[[as.character(Long)]][[as.character(Lat)]]
    }
      
    #add all of the storm paths to a larger dataframe
    if (count == 1){
      new_storm_data <- storm_path
      count = count + 1
    }else{
      new_storm_data <- rbind(new_storm_data,storm_path)
    }
  }
  
  #plot the results with the filenames needed for ffmpeg to work 000,001,etc.
  
  count = 0
  rownames(new_storm_data) <- c(1:dim(new_storm_data)[1])
  new_storm_data['Wind.kt'] <- new_storm_data$WKT
  
  for(i in unique(new_storm_data$ID)){
    
    #get the output names correct for ffmpeg
    if (count < 10){
      name <- paste('00', count, sep = '')
    }else if (count < 100){
      name <- paste('0', count, sep = '')
    }else if (count <1000){
      name <- paste('',count, sep = '')
    }else{
      print('ERROR!!! TOO MANY PLOTS')
    }
    
    count = count + 1
    
  
    storm <- new_storm_data[new_storm_data$ID == i,]
    knots_range = c(0, round_any(max(storm$Wind.kt),10, f = ceiling))
    plot <- graph_base3 + geom_path(aes(x = Longitude, y = Latitude,colour=Wind.kt),alpha=1/2, size=1.2, data = storm)
    plot <- plot + theme(text=element_text(size=rel(5)),
                         panel.background = element_rect(fill = "gray10", colour = "gray30"),
                         panel.spacing = unit(c(0,0), "lines"),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         plot.margin = unit(c(0,0,0,0), "lines"),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         legend.position = c(0.2, 0.1),
                         legend.background = element_rect(fill="gray10", color="gray10"),
                         legend.text = element_text(color="white", size=rel(2)),
                         legend.title = element_text(color="white", size=rel(5)),
                         legend.direction = "horizontal") + scale_color_gradientn(colours=rev(brewer.pal(n=9, name="RdBu")),
                                                                                space="Lab", limits=knots_range)
    plot <- plot + geom_text(label= paste('Generated Typhoon Paths No:', i), aes(x=95, y=50), size=rel(4), color="white", vjust=1)
    
    png(paste('Gen/img',name,'.png',sep = ''))
    print(plot)
    dev.off()
  }
  
  return(new_storm_data)

}

