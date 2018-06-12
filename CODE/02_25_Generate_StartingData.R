#02_25_Generate_StartingData.R


fit_data <- function(storms_inbasin,gen_typhoons){
  
  #####################################################################################################
  #Ignore this bit unless graphs are needed -  not relevant to how the program works.
  
  Pressure <- storms_inbasin[storms_inbasin$Pressure.mb<2000 & storms_inbasin$Pressure.mb>800,]$Pressure.mb
  Pressure_fw <- fitdist(Pressure, "weibull")
  Pressure_flogn <- fitdist(Pressure, 'lnorm')
  Pressure_fgamma <- fitdist(Pressure, 'gamma')
  
  # gofstat(fw)
  # gofstat(flogn)
  # gofstat(fgamma)
  
  #If you want a pretty graph comparing models
  # par(mfrow = c(2, 2))
  # plot.legend <- c("Weibull","Logn","gamma")
  # denscomp(list(Pressure_fw,Pressure_flogn,Pressure_fgamma), legendtext = plot.legend)
  # qqcomp(list(Pressure_fw,Pressure_flogn,Pressure_fgamma), legendtext = plot.legend)
  # cdfcomp(list(Pressure_fw,Pressure_flogn,Pressure_fgamma), legendtext = plot.legend)
  # ppcomp(list(Pressure_fw,Pressure_flogn,Pressure_fgamma), legendtext = plot.legend)

  #Weibull is best I think
  
  #gen_typhoons['Pressure.mb'] <- round(rweibull(dim(gen_typhoons)[1], Pressure_fw$estimate[1], scale = Pressure_fw$estimate[2]))
  
  
  #####################################################################################################
  #Important from here!!
  
  
  # In order to have data that the regression model can iterate off they must be created.
  # A weibull distribution is used from the first 4 points of the historical storm tracks to generate the data
  # Intensity and t,t-6,t-12, change in lat change in long etc.
  #For translation and heading find the historical starting values
  DLO <- vector(mode='numeric')
  DLO6 <- vector(mode='numeric')
  DLO12 <- vector(mode='numeric')
  DLA <- vector(mode='numeric')
  DLA6 <- vector(mode='numeric')
  DLA12 <- vector(mode='numeric')
  I <- vector(mode='numeric')
  I6 <- vector(mode='numeric')
  I12 <- vector(mode='numeric')
  WKT <- vector(mode='numeric')
  WKT6 <- vector(mode='numeric')
  WKT12 <- vector(mode='numeric')
  
  source('02_24_Prep_Regression.R')

  for (id2 in unique(storms_inbasin$ID)){
    hist_storm <- storms_inbasin[storms_inbasin$ID == id2,]
    
    if(dim(hist_storm)[1]> 3){
      DLA <- append(DLA,get_delta_Lat(0,4,hist_storm))
      DLA6 <- append(DLA6,get_delta_Lat(1,4,hist_storm))
      DLA12 <- append(DLA12,get_delta_Lat(2,4,hist_storm))
      
      DLO <- append(DLO,get_delta_Long(0,4,hist_storm))
      DLO6 <- append(DLO6,get_delta_Long(1,4,hist_storm))
      DLO12 <- append(DLO12,get_delta_Long(2,4,hist_storm))
      
      I <- append(I,hist_storm[4,"Pressure.mb"])
      I6 <- append(I6,hist_storm[3,"Pressure.mb"])
      I12 <- append(I12,hist_storm[2,"Pressure.mb"])
      
      WKT <- append(WKT,hist_storm[4,"Wind.kt"])
      WKT6 <- append(WKT6,hist_storm[3,"Wind.kt"])
      WKT12 <- append(WKT12,hist_storm[2,"Wind.kt"])
      
    }
  }
  
  #weibul distributions require all positive values so make sure it is all positive by adding the smallest value
  #then minus that value back off after the distribution is created.
  DLOmin <<- round(abs(min(DLO)-1))
  DLAmin <<- round(abs(min(DLA)-1))
  
  #Fit the distribution using MLE
  DLOw <- fitdist(DLO+DLOmin, "weibull")
  DLO6w <- fitdist(DLO6+DLOmin, "weibull")
  DLO12w <- fitdist(DLO12+DLOmin, "weibull")
  
  DLAw <- fitdist(DLA + DLAmin, "weibull")
  DLA6w <- fitdist(DLA6+ DLAmin, "weibull")
  DLA12w <- fitdist(DLA12+ DLAmin, "weibull")
  
  Iw <- fitdist(I[I<2000 & I >800], "weibull")
  I6w <- fitdist(I6[I6<2000 & I6 >800], "weibull")
  I12w <- fitdist(I12[I12<2000 & I12 >800], "weibull")
  
  #might be cheating a bit too much here - not sure as a lot of points are removed
  WKTw <- fitdist(WKT[WKT >0], "weibull")
  WKT6w <- fitdist(WKT6[WKT6 > 0], "weibull")
  WKT12w <- fitdist(WKT12[WKT12 > 0], "weibull")
  
  #hist(WKT[WKT>0])
  
  #Use the distributions to generate strating data.
  
  gen_typhoons['DLO'] <- round_any(rweibull(dim(gen_typhoons)[1], DLOw$estimate[1], scale = DLOw$estimate[2]),0.1) -DLOmin
  gen_typhoons['DLO6'] <- round_any(rweibull(dim(gen_typhoons)[1], DLO6w$estimate[1], scale = DLO6w$estimate[2]),0.1) -DLOmin
  gen_typhoons['DLO12'] <- round_any(rweibull(dim(gen_typhoons)[1], DLO12w$estimate[1], scale = DLO12w$estimate[2]),0.1)-DLOmin
  
  gen_typhoons['DLA'] <- round_any(rweibull(dim(gen_typhoons)[1], DLAw$estimate[1], scale = DLAw$estimate[2]),0.1) - DLAmin
  gen_typhoons['DLA6'] <- round_any(rweibull(dim(gen_typhoons)[1], DLA6w$estimate[1], scale = DLA6w$estimate[2]),0.1)- DLAmin
  gen_typhoons['DLA12'] <- round_any(rweibull(dim(gen_typhoons)[1], DLA12w$estimate[1], scale = DLA12w$estimate[2]),0.1)- DLAmin
  
  gen_typhoons['I'] <- round_any(rweibull(dim(gen_typhoons)[1], Iw$estimate[1], scale = Iw$estimate[2]),0.1)
  gen_typhoons['I6'] <- round_any(rweibull(dim(gen_typhoons)[1], I6w$estimate[1], scale = I6w$estimate[2]),0.1)
  gen_typhoons['I12'] <- round_any(rweibull(dim(gen_typhoons)[1], I12w$estimate[1], scale = I12w$estimate[2]),0.1)
  
  gen_typhoons['WKT'] <- round_any(rweibull(dim(gen_typhoons)[1], WKTw$estimate[1], scale = WKTw$estimate[2]),0.1)
  gen_typhoons['WKT6'] <- round_any(rweibull(dim(gen_typhoons)[1], WKT6w$estimate[1], scale = WKT6w$estimate[2]),0.1)
  gen_typhoons['WKT12'] <- round_any(rweibull(dim(gen_typhoons)[1], WKT12w$estimate[1], scale = WKT12w$estimate[2]),0.1)
  
  return(gen_typhoons)
}