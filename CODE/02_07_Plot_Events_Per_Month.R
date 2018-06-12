#02_03b_Plot_Events_Per_Month.R

#Return the month as a string
extractMonth <- function(x) {
  strmonth <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  months <- format(as.POSIXct(x), "%m")
  months <- strmonth[as.numeric(months)]
}

##################################################################################################


plot_events_per_month <- function(storms_per_month){
  
  #This bit allows months to be in order on X axis
  months <- rep(factor(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),3)
  
  #Create a large dataframe of the information
  Type <- c(rep("Nino",12),rep("Nina",12),rep("Neither",12))
  values <- c(storms_per_month$Nino,storms_per_month$Nina,storms_per_month$Neither)
  data_to_plot <- data.frame(months,values)
  
  #Plot the data - save to variable 'base'
  storms_per_month$Month <-factor(storms_per_month$Month,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  base <- ggplot(aes(months,values), data = data_to_plot) + geom_bar(stat = 'identity',aes(fill = Type),position = "dodge")
  base <- base + xlab("Months") + ylab("Events | Month") + ggtitle("Typhoon Cycle") + theme_minimal() #+ coord_fixed(ratio = 0.5)
  #Output to pdf file
  pdf("Typhoon_Cycle.pdf")
  print(base)
  dev.off()

}

##################################################################################################
#Called later!!

plot_total_events_per_month <- function(storms_per_month){
  
  months <- factor(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  values <- c(storms_per_month$All)
  data_to_plot <- data.frame(months,values)
  base <- ggplot(aes(months,values), data = data_to_plot) + geom_bar(stat = 'identity',position = "dodge")
  base <- base + xlab("Months") + ylab("Events | Month") + ggtitle("Typhoon Cycle") + theme_minimal() #+ coord_fixed(ratio = 0.5)
  base
  pdf("Typhoon_Cycle_Total.pdf")
  print(base)
  dev.off()
  
}

##################################################################################################

Create_Boxplot <- function(storms_dissag_per_month1){
  
  storms <- t(storms_dissag_per_month1)
  events <- vector(mode = "numeric")
  months <- vector(mode = "numeric")
  for(i in colnames(storms)){
    print(i)
    events <- append(events,storms[,i])
    months <- append(months,rep(c(i), dim(storms)[1]))
  }
  months <- factor(months,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  to_plot <- data.frame(months,events)
  
  base <- ggplot(aes(months,events), data = to_plot) + geom_boxplot()#(stat = 'identity',position = "dodge")
  base <- base + xlab("Months") + ylab("Events | Month") + ggtitle("Typhoon Cycle") + theme_minimal() + coord_fixed(ratio = 0.8)
  pdf('Typhoon_boxplot_original.pdf')
  plot(base)
  dev.off()
}
