#02_22_Plot_Generated_Typhoons.R

#####################################################################################################
#Plot contours as generated data and white points as historic data

plot_gen_birth_locations <- function(gen_typhoons,monthly_birth_locations){
  
  tempdata = gen_typhoons[,c('Longitude','Latitude')]
  colnames(gen_typhoons) <- c('lon','lat','Month')
  colnames(tempdata) = c('lon','lat')
  
  plotted <- graph_base3 + geom_text(label='Generated Data vs Original', aes(x=90, y=40), size=rel(5), color="white", vjust=1)+ stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha = ..level..),size = 2, bins = 15, data = tempdata,geom = "polygon") +scale_fill_gradient(low = "orange",high= "red")
  plotted <- plotted + geom_point(aes(x = Longitude, y = Latitude),alpha = 0.8,size = 0.3,color = "white",data = monthly_birth_locations)
  plotted <- plotted + theme(legend.position = c(0.15, 0.1),legend.background = element_rect(fill="gray10", color="gray10"),legend.text = element_text(color="white", size=rel(0.5)),legend.title = element_text(color="white", size=rel(5)),legend.direction = "horizontal")
  plotted <- plotted + scale_alpha(guide = 'none') + scale_colour_gradient(guide = 'none')
  png(filename="Generated_Data_Birth_Loc.png",
      width=1920, height=1080, type="cairo", bg="gray25")
  print(plotted)
  dev.off()
}

#####################################################################################################
#Contours as historic data and points as generated data

plot_birth_locations <- function(gen_typhoons,monthly_birth_locations){
  
  tempdata = monthly_birth_locations[,c('Longitude','Latitude')]
  colnames(tempdata) = c('lon','lat')
  colnames(gen_typhoons) <- c('lon','lat','Month')
  plotted <- graph_base2 + geom_text(label='Generated Data vs Original', aes(x=75, y=40), size=rel(5), color="white", vjust=1)+ stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha = ..level..),size = 2, bins = 4, data = tempdata,geom = "polygon") +scale_fill_gradient(low = "orange",high= "red")
  plotted <- plotted + geom_point(aes(x = lon, y = lat),alpha = 0.8,size = 0.3,color = "white",data = gen_typhoons)

  png(filename="Original_Data_Birth_Loc.png",
      width=1920, height=1080, type="cairo", bg="gray25")
  print(plotted)
  dev.off()
}