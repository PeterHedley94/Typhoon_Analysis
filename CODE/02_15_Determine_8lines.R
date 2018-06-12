#02_15_Determine_8lines.R

det_8_Lines <- function(storms){
  
  storm_idcurved = c('2012279N15145','2014184N08147','2014275N06166','2015122N07144')
  storm_idstraight = c('2014254N10142','2014190N08154','2013316N06130','2013282N14132')
  
  count = 1
  for (storm in c(storm_idcurved,storm_idstraight)){
    storm_id = storm
    if (count == 1){
      stormstemp <- storms[storms$ID == storm_id,]
      count = 2
    }else{
      stormstemp <- rbind(stormstemp,storms[storms$ID == storm_id,])
    }
  }
  
  #Can do this if you wish - PDF overwrites ones plotted before though!!
  
  # source('02_06_Plot_Storms.R')
  # plot_basin_storms(stormstemp)
  # dev.off()
  # 
  # source('02_07_Storm2Polar.R')
  # #lat =0 long = 95
  # origin = c(95,0)
  # #note distances are currently in m
  # converted_to_polar = latlon2pol(stormstemp,origin)
  # 
  # source('02_08_Plot_Polar2.R')
  # plot_polar(converted_to_polar)
  
}
