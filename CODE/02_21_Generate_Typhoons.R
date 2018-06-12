#02_21_Generate_Typhoons.R


ret_kernel <- function(gengrid,genprob,month,number){
  
  #select a random point on grid dependent on probability of that point.
  Selected <- sample(genprob$Selection,prob = genprob$values,size = number,replace = TRUE)
  Selected <- genprob[Selected,]
  
  #This bit is due to the long and lat grid being in a matrix so each col is a longitude and each
  # row a lattitude. The list is stacked so all columns are appended into one big column
  #so one step of longitude is 1 col over so (+number of rows) in position on the long list
  Longitude <- gengrid$x1[(Selected$Selection - (dim(gengrid)[1] * (as.numeric(Selected$ind)-1)))]
  Latitude <- gengrid$x2[Selected$ind]
  
  Coordinates <- data.frame(Longitude,Latitude)
  return(Coordinates)
}

#####################################################################################################

generate_typhoons <- function(generated_kernel,created_events,origin){
  
  
  #tidy up starting data
  created_events <- t(created_events)
  generated_grid <- generated_kernel[[1]]
  generated_probability <- generated_kernel[[2]]
  
  #create empty variables to store data in
  generated_longitude <- vector(mode = 'numeric')
  generated_latitude <- vector(mode = 'numeric')
  generated_month <- vector(mode = 'numeric')
  generated_year <- vector(mode = 'numeric')
  
  
  #now append data to relevant vectors
  for (month in c(1:12)){
    number <- sum(created_events[,month])
    gen_Coords <- ret_kernel(generated_grid[[month]],generated_probability[[month]],colnames(created_events)[month],number)
    generated_longitude <- append(generated_longitude,gen_Coords$Longitude)
    generated_latitude <- append(generated_latitude,gen_Coords$Latitude)
    generated_month <- append(generated_month, rep(month,number))
  }
  
  #dataframe from the vectors
  generated_storms <- data.frame(generated_longitude,generated_latitude,generated_month)
  colnames(generated_storms) <- c('Longitude','Latitude','Month')
  
  #add back the origin that created the adjusted data.
  generated_storms$Longitude <- generated_storms$Longitude + origin[1]
  generated_storms$Latitude <- generated_storms$Latitude + origin[2]
  generated_storms$Longitude[generated_storms$Longitude > 180] <- -180 + (generated_storms$Longitude[generated_storms$Longitude > 180] - 180)
  
  return(generated_storms)
}

