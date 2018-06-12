#02_05_Events_Per_Year.R

# Get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

######################################################################################################

# extract the months
extractMonth <- function(x) {
  months <- format(as.POSIXct(x), "%m")
  months <- as.numeric(months)
}

######################################################################################################

events_per_year <- function(storms){
  
  #use dplyr package
  storms <- storms %>%
    mutate(Month <- extractMonth(ISO_time))
  #change Column name
  colnames(storms)[colnames(storms) == "Month <- extractMonth(ISO_time)"] <- 'Month'
  
  #Create vectors to append data to
  #Number per year - simple count
  number_per_year = vector(mode = 'numeric')
  #Average month - gets the average month of occurance
  average_month = vector(mode = 'numeric')
  
  #Split storms into respective years
  for (year in unique(storms$Year)) {
    storm_ids <- unique(storms[storms$Year==year,]$ID)
    months_of_year = vector(mode = "numeric")
    
    #find which months the storms of that year occurred in
    for (storm in unique(storm_ids)){
      months_of_year = append(months_of_year,getmode(storms[storms$ID == storm,]$Month))
    }
    #take the months and find the average
    average_month <- append(average_month, mean(months_of_year))
    
    rm(months_of_year)
    
    #now count the number of events in that year
    number_per_year = append(number_per_year,length(storm_ids))
  }
  
  #Add all of the data into a dataframe
  events_vs_year <- data.frame(list(unique(storms$Year)),number_per_year,average_month)
  colnames(events_vs_year) <- c('Year','No.Events',"Av.Month")
  
  return(events_vs_year)
}

