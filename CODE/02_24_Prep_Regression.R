#02_24_Prep_Regression.R

#####################################################################################################
#Just return the change in Longitude over the time step

get_delta_Long <- function(time_step,time,storm){
  Delta_Long <- (storm$Longitude[time-time_step]-storm$Longitude[time-time_step-1])
  return(Delta_Long)
}

#####################################################################################################
#Same as above

get_delta_Lat <- function(time_step,time,storm){
  Delta_Lat <- (storm$Latitude[time-time_step]-storm$Latitude[time-time_step-1])
  return(Delta_Lat)
}

#####################################################################################################
#Get the required values to create a regression model off

get_regression_data <- function(storms_inbasin){
  
  #I is intensity I6 is intensity at t-6 etc.
  # DLO is delta longitude DLA delta Latitude.
  #Perhaps could put these into a list which would be better
  I <- vector(mode = "numeric")
  DLA <- vector(mode = "numeric")
  DLO <- vector(mode = "numeric")
  WKT <- vector(mode = "numeric")
  I6 <- vector(mode = "numeric")
  DLA6 <- vector(mode = "numeric")
  DLO6 <- vector(mode = "numeric")
  WKT6 <- vector(mode = "numeric")
  I12 <- vector(mode = "numeric")
  DLA12 <- vector(mode = "numeric")
  DLO12 <- vector(mode = "numeric")
  WKT12 <- vector(mode = "numeric")
  
  
  #the count just avoids trying to assign the dataframe in a weird way
  count <- 1
  
  #for every unique storm
  for (id2 in unique(storms_inbasin$ID)){
    #variable storm now contains data for that storm
    storm <- storms_inbasin[storms_inbasin$ID == id2,]
    
    #check there is enough data in that storm
    if (dim(storm)[1] > 3){
      #get all the data except the first three points
      if(count == 1){
        temp_storms_inbasin <- tail(storm,-3)
        count <- count +1
      }else if(count == 2){
        print(dim(temp_storms_inbasin))
        temp_storms_inbasin <- rbind(temp_storms_inbasin, tail(storm,-3))
      }
      
      #assign the values to their respective vectors
      for (time in  c(4:(dim(storm)[1])) ){
        I <- append(I,storm[time,"Pressure.mb"])
        DLA <- append(DLA,get_delta_Lat(0,time,storm))
        DLO <- append(DLO,get_delta_Long(0,time,storm))
        WKT <- append(WKT,storm[time,"Wind.kt"])
        I6 <- append(I6,storm[(time-1),"Pressure.mb"])
        DLA6 <- append(DLA6,get_delta_Lat(1,time,storm))
        DLO6 <- append(DLO6,get_delta_Long(1,time,storm))
        WKT6 <- append(WKT6,storm[time,"Wind.kt"])
        I12 <- append(I12,storm[(time-2),"Pressure.mb"])
        DLA12 <- append(DLA12,get_delta_Lat(2,time,storm))
        DLO12 <- append(DLO12,get_delta_Long(2,time,storm))
        WKT12 <- append(WKT12,storm[time,"Wind.kt"])
      }
      }
  }
  #put the values for regression into a dataframe
  df <- data.frame(I,I6,I12,DLO,DLO6,DLO12,DLA,DLA6,DLA12,WKT,WKT6,WKT12)
  cols <- c("Latitude","Longitude","Wind.kt","Pressure.mb","Nino")
  temp_storms_inbasin <- temp_storms_inbasin[,cols]

  #add these extra values to the temp dataframe.
  for(column in colnames(df)){
    temp_storms_inbasin[column] <- df[column]
  }
  
  return(temp_storms_inbasin)
}


#####################################################################################################


divide_into_grid <- function(storms_inbasin,origin,granularity){
  

  
  #Deal with storms that have a Longitude change from - to +
  for (storm in unique(storms_inbasin$ID)){
    storm_id = storm
    storm = storms_inbasin[storms_inbasin$ID == storm,]
    if ((min(storm$Longitude)<0) & (max(storm$Longitude>0))){
      storms_inbasin[storms_inbasin$Longitude < 0 & storms_inbasin$ID == storm_id,]$Longitude <- storms_inbasin[storms_inbasin$Longitude < 0 & storms_inbasin$ID == storm_id,]$Longitude + 180*2
    }
  }
  
  #run the function above to return the data needed for the regression model
  storms_inbasin <- get_regression_data(storms_inbasin)
  
  #Create a list for the longitudinal grid
  Regression_Long_Grid <- vector("list", length(seq(origin[1],220,granularity)))
  names(Regression_Long_Grid) <- seq(origin[1],220,granularity)
  
  #the idea is that for each cell in the longitudinal list there is another list inside that cell
  # inside each cell there is the latitude list and in each cell of the latitude list there is a regression model (3 of them)
  #So if you want to find the regression model for a particular location you choose that longitude cell followed by that latitude cell to find the models
  
  #220 longitude equates to -130 ( we hve dealt with negatives above)
  for (Long in seq(origin[1],220,granularity)){
    #create the latitude list
    Regression_Lat_Grid <- vector("list", length(seq(origin[2],70,granularity)))
    names(Regression_Lat_Grid) <- seq(origin[2],70,granularity)
    
    #for each 5x5 cell find the regression models
    for (Lat in seq(origin[2],70,granularity)){
      
      #only select data within that cell's space/ location
      temp_regression_data <- storms_inbasin[storms_inbasin$Longitude >= Long & storms_inbasin$Longitude <= Long+5,]
      temp_regression_data <- temp_regression_data[temp_regression_data$Latitude >= Lat & temp_regression_data$Latitude <= Lat+5,]

      #check that data exists before assigneing a model to no data
      if (dim(temp_regression_data)[1] >3){
        #assign the models to the data
        DLOm <- lm(DLO ~  Latitude + DLO6 + DLO12 + Longitude, data=temp_regression_data)
        DLAm <- lm(DLA ~  Latitude + DLA6 + DLA12 + Longitude, data=temp_regression_data)
        Im <- lm(I ~  Latitude + I6 + I12 + Longitude, data=temp_regression_data)
        WKTm <- lm(WKT ~ Latitude + WKT6 + WKT12 + Longitude, data = temp_regression_data)
        Regression_Model <- list(DLOm,DLAm,Im,WKTm)
        names(Regression_Model) <- c("DLO","DLA","IM",'WKTM')
        Regression_Lat_Grid[as.character(Lat)] <- list(Regression_Model)
      }
    }
    #once all models for that slice (value of longitude are created) for all values of latitude append it to the list.
    Regression_Long_Grid[as.character(Long)] <- list(Regression_Lat_Grid)
  }
  return(Regression_Long_Grid)
  
}