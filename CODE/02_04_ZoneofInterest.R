#'02_02b_ZoneofInterest'

#Check if any part of storm is in the Zone of Interest
#If yes - keep, if not then discard

storms_format <- function(storms,basin_long,basin_lat){

  count = 1
  
  for (storm in unique(storms$ID)){
    
    storm_id = storm
    storm = storms[storms$ID == storm,]
    if ( (any(storm$Longitude < basin_long[2])) & (any(storm$Longitude > basin_long[1]))){
      
      if ( (any(storm$Latitude < basin_lat[2])) & (any(storm$Latitude > basin_lat[1]))){
        
        if (count == 1){
          basin_storms <- storms[storms$ID == storm_id,]
          count = 2
        }else{
          basin_storms <- rbind(basin_storms,storms[storms$ID == storm_id,])
        }
        
      }
      
    }
    
  }

  return(basin_storms)
}