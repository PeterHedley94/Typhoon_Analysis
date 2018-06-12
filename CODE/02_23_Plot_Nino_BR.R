#02_11_Plot_Nino_BR.R


getmonth <- function(month){
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  return (months[month])
}

#####################################################################################################

getplot <- function(tempmonthly_birth_locations,month){
  tempdata = tempmonthly_birth_locations[tempmonthly_birth_locations$Month==month,3:4]
  print(paste(month,' | Number |', dim(tempdata)[1]))
  colnames(tempdata) = c('lon','lat')
  plotted <- graph_base + xlim(-180,180) + ylim(-90,90) + geom_text(label=paste('Month |', getmonth(month), '|', dim(tempdata)[1]), aes(x=75, y=40), size=rel(10), color="white", vjust=1)+ stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha = ..level..),size = 2, bins = 15, data = tempdata,geom = "polygon") + scale_alpha_continuous(limits = c(0,0.05)) + scale_fill_gradient(limits = c(0,0.05),low = "orange",high= "red")
}

#####################################################################################################
#See Below and output for explanation of what is going on here!

plot_birth_rates <- function(monthly_birth_locations,storms){
  for (month in unique(monthly_birth_locations$Month)){
  

    tempmonthly_birth_locations <- monthly_birth_locations
    plot <- getplot(tempmonthly_birth_locations,month)

    message(sprintf("%s", month))
    png(filename=sprintf(paste("output/generated/img%s.png" , sep = ''), month),
        width=1920, height=1080, type="cairo", bg="gray25")
    print(plot)
    dev.off()
    
  }
}

  
# So the mp4 file contains the stat2d plot of typhoon genesis for each month of the year. So as if all of the data was split into respective months and joined i.e. all typhoons since 1950 that occurred in March are in the March plot. Then the 4 panels represent:
#   
# 1. All - all typhoons recorded in that month since 1950
# 2. Nino - all typhoons recorded in that month since 1950 that have a Nino signal = ENSO >= 0.5
# 3. Nina - all typhoons recorded in that month since 1950 that have a Nina signal = ENSO <= -0.5
# 4. Neither - all typhoons recorded in that month since 1950 that have neither a La Nina or an El Nino signal = ENSO <= 0.5 && = ENSO >= -0.5
# 
# The number in the text of each plot is the number of recorded events in that subset since 1950. So for example there have been 131 typhoons in January when January was in an EL Nino phase since 1950. Quite hard to explain but hope that makes sense?
