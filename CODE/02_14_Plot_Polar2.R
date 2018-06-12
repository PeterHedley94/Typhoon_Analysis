#02_14_Plot_Polar.R

#Initial Plot of data; Plot Angle vs Time then R vs Time for typhoons.
#Plot Change in angle and change in R over time: Minus the first (Angle or R) from other (Angles, Rs) then plot. Therefore e.g. Angle(t)- Angle(0)
#Scatter plot of change in angle vs R: Angle(t)-Angle(t-1) So change in angle vs Distance from Origin.
#Scatter plot of change in angle vs change in R: [Angle(t)-Angle(t-1)] vs [R(t)-R(t-1)]


plot_polar <- function(converted_to_polar){
  
  ############################################################################################
  #Initial Plot of data; 
  converted_to_polar_temp = converted_to_polar
  
  converted_to_polar_temp$Hour <- c(1:length(converted_to_polar_temp$Longitude))
  
  for (storm in unique(converted_to_polar_temp$ID)){
    converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$Hour = c(1:length(converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$Year))
  }
  
  graph_Angle_Polar <- ggplot() + geom_path(aes(x = Hour, y = Angle_Polar, group = ID,color = Nino), data = converted_to_polar_temp)
  pdf('Angle_Polar.pdf')
  plot(graph_Angle_Polar + theme_classic())
  dev.off()
  
  
  graph_R_Polar <- ggplot() + geom_path(aes(x = Hour, y = R_Polar/1000, group = ID,color = Nino), data = converted_to_polar_temp) + theme_classic()
  pdf('R_Polar.pdf')
  plot(graph_R_Polar + theme_classic())
  dev.off()
  
  
  
  ############################################################################################
  #Plot Change in angle and change in R over time
  
  for (storm in unique(converted_to_polar_temp$ID)){
    values <- converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$Angle_Polar - converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$Angle_Polar[1]
    converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$Angle_Polar = values
  }
  
  for (storm in unique(converted_to_polar_temp$ID)){
    values <- converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$R_Polar - converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$R_Polar[1]
    converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$R_Polar = values
  }
  
  Change_Angle_Polar <- ggplot() + geom_path(aes(x = Hour, y = Angle_Polar, group = ID,color = Nino), data = converted_to_polar_temp)
  pdf('Change_Angle_Polar.pdf')
  plot(Change_Angle_Polar + theme_classic())
  dev.off()
  
  
  Change_Angle_R <- ggplot() + geom_path(aes(x = Hour, y = R_Polar/1000, group = ID,color = Nino), data = converted_to_polar_temp)
  pdf('Change_R_Polar.pdf')
  plot(Change_Angle_R + theme_classic())
  dev.off()
  
  
  
  
  ##############################################################################################
  #scatter plot of change in angle vs R
  
  converted_to_polar_temp = converted_to_polar
  
  converted_to_polar_temp$Hour <- c(1:length(converted_to_polar_temp$Longitude))
  
  for (storm in unique(converted_to_polar_temp$ID)){
    converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$Hour = c(1:length(converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$Year))
  }
  
  for (storm in unique(converted_to_polar_temp$ID)){
    for (angle in c(1:length(converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$Angle_Polar))){
      if (angle == 1){
        prevval = 0
        converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$Angle_Polar[angle] = prevval
      }else{
        prevval = converted_to_polar[converted_to_polar_temp$ID == storm,]$Angle_Polar[angle-1]
        curval = converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$Angle_Polar[angle] 
        converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$Angle_Polar[angle] = curval - prevval
      }

    }
  }
  
  R_vs_Angle_Polar <- ggplot() + geom_point(aes(x = R_Polar, y = Angle_Polar,color = Nino), data = converted_to_polar_temp)
  
  pdf('R vs Change_Angle_Polar.pdf')
  plot(R_vs_Angle_Polar + theme_classic())
  dev.off()
  
  
  
  ############################################################################################
  #scatter plot of change in angle vs change in R
  
  for (storm in unique(converted_to_polar_temp$ID)){
    for (angle in c(1:length(converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$R_Polar))){
      if (angle == 1){
        prevval = 0
        converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$R_Polar[angle] = prevval
      }else{
        prevval = converted_to_polar[converted_to_polar_temp$ID == storm,]$R_Polar[angle-1]
        curval = converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$R_Polar[angle] 
        converted_to_polar_temp[converted_to_polar_temp$ID == storm,]$R_Polar[angle] = curval - prevval
      }

    }
  }
  
  R_vs_Angle_Polar <- ggplot() + geom_point(aes(x = R_Polar, y = Angle_Polar,color = Nino), data = converted_to_polar_temp)
  
  pdf('Change_R vs Change_Angle_Polar.pdf')
  plot(R_vs_Angle_Polar + theme_classic())
  dev.off()
  

}
