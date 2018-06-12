#'02_09_Plot_Birth_Rates.R'

#Plot each year genesis locations as a Stat_2d plot.
#Output to .png. then use ffmpeg to convert to video. Need .png names as 000,001,002 etc.
#need to download ffmpeg - hard to do but can find online.
#https://trac.ffmpeg.org/wiki/Slideshow
# think I used this 'ffmpeg -framerate 1/5 -i img%03d.png -c:v libx264 -vf fps=25 -pix_fmt yuv420p out.mp4'

plot_birth_rates <- function(storms){
  count = 0

  for (year in unique(yearly_birth_locations$Year)){
    tempdata = yearly_birth_locations[yearly_birth_locations$Year==year,3:4]
    colnames(tempdata) = c('lon','lat')
    plotted <- graph_base2 + geom_text(label=paste('Year |',year), aes(x=75, y=40), size=rel(10), color="white", vjust=1)+ stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha = ..level..),size = 2, bins = 4, data = tempdata,geom = "polygon") +scale_fill_gradient(low = "orange",high= "red")
    message(sprintf("%s", year))
    
    #get the output names correct for ffmpeg
    if (count < 10){
      name <- paste('00', count, sep = '')
    }else if (count < 100){
      name <- paste('0', count, sep = '')
    }else if (count <1000){
      name <- paste('',count, sep = '')
    }else{
      print('ERROR!!! TOO MANY PLOTS')
    }
    
    png(filename=sprintf("output/birth_loc/img%s.png", name),
        width=1920, height=1080, type="cairo", bg="gray25")
    print(plotted)
    dev.off()
    
    count = count + 1
  }
}
