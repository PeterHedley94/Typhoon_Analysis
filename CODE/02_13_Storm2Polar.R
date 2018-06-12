#'02_13_Storm2Polar.R'


latlon2pol <- function(storms,origin){
  
  #Deal with storms that have a Longitude change from - to +
  for (storm in unique(storms$ID)){
    storm_id = storm
    storm = storms[storms$ID == storm,]
    if ((min(storm$Longitude)<0) & (max(storm$Longitude>0))){
      storms[storms$Longitude < 0 & storms$ID == storm_id,]$Longitude <- storms[storms$Longitude < 0 & storms$ID == storm_id,]$Longitude + 180*2
    }
  }
  
  
  long_rep = rep(origin[1],length(storms$Longitude))
  lat_rep = rep(origin[2],length(storms$Latitude))
  
  #Aim is to get polar coordinates so find y from the change in Lattitude only!
  # x from dist across surface from change in Longitude only and R dist across surface from change in lat and long.
  temp_storms_R = storms[,c('Longitude','Latitude')]
  temp_storms_deltax1 = data.frame(storms[,c('Longitude')],lat_rep)
  temp_storms_deltay1 = data.frame(long_rep,storms[,c('Latitude')])
  
  origin = rbind(origin)
  
  #Use package 'geosphere' to determine distance across surface of earth from origin - then angle
  temp_storms_R = distm(temp_storms_R,origin, fun=distVincentyEllipsoid)
  temp_storms_deltax2 = distm(temp_storms_deltax1,origin, fun=distVincentyEllipsoid)
  temp_storms_deltay2 = distm(temp_storms_deltay1,origin, fun=distVincentyEllipsoid)
  angle = atan(temp_storms_deltay2/temp_storms_deltax2)

  rownames(storms) <- NULL
  
  storms['R_Polar'] <- as.numeric(temp_storms_R)
  storms['Angle_Polar'] <- as.numeric(angle)
  
  return(storms)
}
