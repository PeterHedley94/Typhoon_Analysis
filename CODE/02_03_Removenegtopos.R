#'02_03_Removenegtopos.R'

split_storms <- function(storms){
  
  #take each individual storm
  for (storm in unique(storms$ID)){
    storm_id = storm
    storm = storms[storms$ID == storm,]
    #if the storm contains a Longitude below and above 0
    if ((min(storm$Longitude)<0) & (max(storm$Longitude>0))){
      #rename the storm with the suffix 'a'
      storms[storms$Longitude < 0 & storms$ID == storm_id,]$ID <- paste(storm_id, 'a')
    }
  }
  return(storms)
}