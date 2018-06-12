#02_18_Create_Profiles.R

# extract the months
extractMonth <- function(x) {
  months <- format(as.POSIXct(x), "%m")
  months <- as.numeric(months)
}

#####################################################################################################

Create_profiles <- function(storms_inbasin, normalised){
  
  storms <- storms_inbasin
  storms <- storms %>%
    mutate(Month <- extractMonth(ISO_time))
  colnames(storms)[colnames(storms) == "Month <- extractMonth(ISO_time)"] <- 'Month'
  
  count = 1
  number_per_year <- vector(mode = 'numeric')
  
  #For every historical year
  for (year in unique(storms$Year)) {
    stormstemp = storms[storms$Year==year,]
    number_per_month = vector(mode = 'numeric')
    
    #Find number of typhoons for each month in that historical year
    for (month in c(1:12)){
      storm_ids <- unique(stormstemp[stormstemp$Month==month,]$ID)
      number_per_month = append(number_per_month,length(storm_ids))
    }
    
    #total number of events in that year
    number_per_year = append(number_per_year,sum(number_per_month))
    
    #If we want a density divide by total number in that year
    if (normalised == 1){
      number_per_month <- number_per_month / sum(number_per_month)
    }else{
      number_per_month <- number_per_month
    }
    
    #Create and append to a dataframe
    if( count == 1){
      storms_dissag_per_month <- data.frame(data = number_per_month)
      rownames(storms_dissag_per_month) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      colnames(storms_dissag_per_month) = as.character(year)
      count = count +1
    }else{
      storms_dissag_per_month[,as.character(year)] <- number_per_month
    }
  }
  plot_check(storms_dissag_per_month)
  return(storms_dissag_per_month)
}

#####################################################################################################
#Total number of plots per month check against previous plots

plot_check <- function(storms_dissag_per_month){
  storms_dissag_per_month <- t(storms_dissag_per_month)
  months <- factor(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  values <- colSums(storms_dissag_per_month)
  data_to_plot <- data.frame(months,values)
  base <- ggplot(aes(months,values), data = data_to_plot) + geom_bar(stat = 'identity',position = "dodge")
  base <- base + xlab("Months") + ylab("Events | Month") + ggtitle("Typhoon Cycle") + theme_minimal() #+ coord_fixed(ratio = 0.5)
  pdf("Typhoon_Cycle_Total.pdf")
  print(base)
  dev.off()
}
