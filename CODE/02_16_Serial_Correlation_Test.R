#02_16_Serial_Correlation_Test.R

# extract the months
extractMonth <- function(x,Year) {
  months <- format(as.POSIXct(x), "%m")
  year_diff <- Year-1951
  months <- as.numeric(months) + year_diff*12
}


#Plot Number_of_Events(m+1) vs Number_of_Events(m) where m is months since 1950 from 1 to 700 ish

Serial_Correlation <- function(storms){
  
  storms <- storms %>%
    mutate(Month <- extractMonth(ISO_time,Year))
  colnames(storms)[colnames(storms) == "Month <- extractMonth(ISO_time)"] <- 'Month'
  
  number_per_month = vector(mode = 'numeric')
  
  #get number of storms per month since 1950 - different than before
  for (month in unique(storms$Month)) {
    storm_ids <- unique(storms[storms$Month==month,]$ID)
    number_per_month = append(number_per_month,length(storm_ids))
  }
  
  events_vs_month <- data.frame(list(unique(storms$Month)),number_per_month)
  colnames(events_vs_month) <- c('Month','No.Events')
  
  
  Nt = events_vs_month[-1,2]
  Ntplus = head(events_vs_month,-1)[,2]
  
  data_plot = data.frame(Nt,Ntplus)
  
  base <- ggplot(aes(x = Nt, y = Ntplus), data = data_plot) + geom_point() + xlim(-2,15) + ylim(-2,15) + theme_minimal()
  base <- base + coord_fixed(ratio = 1)
  
  pdf('Serial_Correlation.pdf')
  plot(base)
  dev.off()
}

