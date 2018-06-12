#'02_12_Plot_Storms.R'


plot_world_storms <- function(storms){
  base <- graph_base + geom_path(data=storms,aes(x=Longitude, y=Latitude, group=ID, colour=Wind.kt),size=0.1, alpha=1/4) + xlim(-180,180) + ylim(-90,90)
  base <- base + geom_text(label='Paths of Historical Typhoons 1851-2016', aes(x=0, y=75), size=rel(6), color="white", vjust=1)
  pdf('World_Typhoon_Plot.pdf')
  print(base)
  dev.off()
}

#####################################################################################################

plot_basin_storms <- function(storms){
  base = graph_base3 + geom_path(data=storms, aes(x=Longitude, y=Latitude, group=ID ,colour=Wind.kt),alpha=1/4, size=0.25)
  base <- base + geom_text(label='Paths of Historical Typhoons 1971-2016', aes(x=0, y=75), size=rel(6), color="white", vjust=1)
  pdf('Basin_Typhoon_Plot_v.pdf')
  print(base)
  dev.off()
}
