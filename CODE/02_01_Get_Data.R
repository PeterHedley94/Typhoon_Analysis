#02_01_Get_Data.R


# extract the months
extractMonth <- function(x) {
  months <- format(as.POSIXct(x), "%m")
  months <- as.numeric(months)
}

######################################################################################################

# extract the years
extractYear <- function(x) {
  years <- substr(x,1,4)
}

######################################################################################################

get_data <- function(){
  
  #read in the storm data
  hurcat_all <- read.csv("./data/Allstorms.ibtracs_all.v03r09.csv",skip = 2,stringsAsFactors=F)
  #slice to relevant area
  hurcat_all <- hurcat_all[,1:12]
  col_names <- c("ID","Year", "Num", "Basin", "Sub_basin", "Name", "ISO_time", "Nature",
                 "Latitude", "Longitude", "Wind.kt", "Pressure.mb")
  #re-set the column names of the dataframe
  colnames(hurcat_all) <- col_names
  
  
  #Selects only hurricanes since 1950
  hur_min_year<-1950
  hurcat <- hurcat_all[which(hurcat_all$Year >hur_min_year),]
  #can use higher wind speed as minimum here!
  hur_min_wind<-0
  hurcat <- hurcat[which(hurcat$Wind.kt >= hur_min_wind),]
  
  #Create global varaibles of range and knots
  season_range <<- paste(range(hurcat$Year), collapse=" - ")
  knots_range <<- range(hurcat$Wind.kt)
  
  #get the data for ENSO signal
  nino <- read.csv("./data/La_NinavsEl_Nino.csv",header = TRUE)
  colnames(nino) <- c("Year",1:12)
  rownames(nino) <- nino$Year
  ninovec <- vector(mode = "numeric")
  
  #Assign the ENSO signal to typhoon data
  for (iso_time in hurcat$ISO_time){
    month <- extractMonth(iso_time)
    year <- extractYear(iso_time)
    ninovec = append(ninovec,nino[toString(year),toString(month)])
  }
  hurcat['Nino'] <- ninovec
  
  #return the data
  return(hurcat)
}


