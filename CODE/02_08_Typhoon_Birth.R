#'02_08_Typhoon_Birth.R'


birth_loc <- function(storms){

  storm_Long = vector(mode = 'numeric')
  storm_Lat = vector(mode = 'numeric')
  storm_year = vector(mode = 'numeric')
  storm_id = vector(mode = 'character')
  
  #Take first recorded position of typhoon - genesis location
  for (year in unique(storms$Year)) {
    storm_ids <- unique(storms[storms$Year==year,])
    for (storm in unique(storm_ids$ID)){
      storm = unique(storms[storms$ID==storm,])
      storm_year = append(storm_year,as.numeric(as.character(storm['Year'][1,1])))
      storm_Long = append(storm_Long,as.numeric(as.character(storm['Longitude'][1,1])))
      storm_Lat = append(storm_Lat,as.numeric(as.character(storm['Latitude'][1,1])))
      storm_id = append(storm_id,storm['ID'][1,1])
    }
  }
  
  #Storm Id vs Year vs Birth Location
  locations_vs_yr <- data.frame(storm_id,storm_year,storm_Long,storm_Lat)
  colnames(locations_vs_yr) <- c('ID','Year','Longitude', 'Latitude')
  return(locations_vs_yr)
}
