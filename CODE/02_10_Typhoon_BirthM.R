#'02_10_Typhoon_BirthM.R'

# extract the months
extractMonth <- function(x) {
  months <- format(as.POSIXct(x), "%m")
  months <- as.numeric(months)
}

#####################################################################################################

mbirth_loc <- function(storms){
  
  storm_Long = vector(mode = 'numeric')
  storm_Lat = vector(mode = 'numeric')
  storm_month = vector(mode = 'numeric')
  storm_nino = vector(mode = "numeric")
  storm_id = vector(mode = 'character')
  
  storms <- storms %>%
    mutate(Month <- extractMonth(ISO_time))
  
  colnames(storms)[colnames(storms) == "Month <- extractMonth(ISO_time)"] <- 'Month'
  
  for (year in unique(storms$Year)) {
    storm_ids <- unique(storms[storms$Year==year,])
    for (storm in unique(storm_ids$ID)){
      storm = unique(storms[storms$ID==storm,])
      storm_month = append(storm_month,as.numeric(as.character(storm['Month'][1,1])))
      storm_Long = append(storm_Long,as.numeric(as.character(storm['Longitude'][1,1])))
      storm_nino = append(storm_nino,as.numeric(as.character(storm['Nino'][1,1])))
      storm_Lat = append(storm_Lat,as.numeric(as.character(storm['Latitude'][1,1])))
      storm_id = append(storm_id,storm['ID'][1,1])
      
    }
  }
  locations_vs_yr <- data.frame(storm_id,storm_month,storm_Long,storm_Lat,storm_nino)
  colnames(locations_vs_yr) <- c('ID','Month','Longitude', 'Latitude','Nino')
  return(locations_vs_yr)
}
