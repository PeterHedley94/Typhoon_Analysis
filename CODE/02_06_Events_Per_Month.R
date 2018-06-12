#02_06_Events_Per_Month.R

# Get the Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#####################################################################################################

# extract the months
extractMonth <- function(x) {
  months <- format(as.POSIXct(x), "%m")
  months <- as.numeric(months)
}

#####################################################################################################

ret_no_months <- function(){
  
  #Read in ENSO signal
  nino <- read.csv("./data/La_NinavsEl_Nino.csv",header = TRUE)
  colnames(nino) <- c("Year",1:12)
  nino <- subset(nino, select = -Year)
  
  number_nino <- vector(mode = 'numeric')
  number_nina <- vector(mode = 'numeric')
  number_neither <- vector(mode = 'numeric')
  
  #Find times that months have been at a certain ENSO signal
  for ( i in colnames(nino)){
    number_nino <- append(number_nino, dim(nino[nino[i]> 0.4,][i])[1])
    number_nina <- append(number_nina, dim(nino[nino[i]< -0.4,][i])[1])
    number_neither <- append(number_neither, dim(nino[which(nino[i] >= -0.4 & nino[i] <= 0.4),][i])[1])
  }
  return(data.frame(number_nino,number_nina,number_neither))
}

##################################################################################################


ret_no_per_month <- function(storms){

  number_per_month = vector(mode = 'numeric')
  for (month in c(1:12)) {
    storm_ids <- unique(storms[storms$Month==month,]$ID)
    number_per_month = append(number_per_month,length(storm_ids))
  }
  
  return(number_per_month)
}

################################################################################################

events_per_month <- function(storms){
  
  #dplyr
  storms <- storms %>%
    mutate(Month <- extractMonth(ISO_time))
  colnames(storms)[colnames(storms) == "Month <- extractMonth(ISO_time)"] <- 'Month'
  
  #Return number of storms in each month of the all the years e.g. (All events in Jan since 1950)
  number_months <- ret_no_months()
  #Split months into signals.
  nino <- ret_no_per_month(storms %>% filter(Nino > 0.4))
  nina <- ret_no_per_month(storms %>% filter(Nino < -0.4))
  neither <- ret_no_per_month(storms %>% filter(Nino >= -0.4, Nino <= 0.4))
  all <- ret_no_per_month(storms)
  
  #Normalise - if Feb has been a La Nina more often than an El Nino - not fair comparison
  nino <- nino / number_months$number_nino
  nina <- nina / number_months$number_nina
  neither <- neither / number_months$number_neither

  events_vs_month <- data.frame(c(1:12),nino,nina,neither, all)
  colnames(events_vs_month) <- c('Month','Nino',"Nina","Neither","All")
  
  return(events_vs_month)
}






