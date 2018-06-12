#02_23_Plot_Gen_Typhoon_Births.R


getmonth <- function(month){
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  return (months[month])
}

#####################################################################################################

# Multiple plot function - COPIED FROM ONLINE (http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout)),gp = gpar(fill = "black")))
    grid.rect(gp = gpar(fill = "black"))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#####################################################################################################
#Get plot for generated data

getplot_gen <- function(tempgen_typhoons,month){
  tempdata = tempgen_typhoons[tempgen_typhoons$Month==month,1:2]
  print(paste('Generated : ',month,' | Number |', dim(tempdata)[1]))
  colnames(tempdata) = c('lon','lat')
  plotted <- graph_base3 + xlim(-180,180) + ylim(-90,90) + geom_text(label=paste('Month |', getmonth(month), '|', dim(tempdata)[1]), aes(x=95, y=40), size=rel(8), color="white", vjust=0)+ stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha = ..level..),size = 2, bins = 15, data = tempdata,geom = "polygon") + scale_alpha_continuous(limits = c(0,0.05)) + scale_fill_gradient(limits = c(0,0.05),low = "orange",high= "red")
  plotted <- plotted + geom_text(label='Generated Data', aes(x=93, y=45), size=rel(8), color="white", vjust=0)
}

#####################################################################################################
#get plot for historical data

getplot_hist <- function(tempmonthly_birth_locations,month,type){
  tempdata = tempmonthly_birth_locations[tempmonthly_birth_locations$Month==month,3:4]
  print(paste('Historical : ',month, ' | Number |', dim(tempdata)[1]))
  colnames(tempdata) = c('lon','lat')
  plotted <- graph_base3 + xlim(-180,180) + ylim(-90,90) + geom_text(label=paste('Month |', getmonth(month), '|', dim(tempdata)[1]), aes(x=95, y=40), size=rel(8), color="white", vjust=0)+ stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha = ..level..),size = 2, bins = 15, data = tempdata,geom = "polygon") + scale_alpha_continuous(limits = c(0,0.05)) + scale_fill_gradient(limits = c(0,0.05),low = "orange",high= "red")
  plotted <- plotted + geom_text(label='Historic Data', aes(x=93, y=45), size=rel(8), color="white", vjust=0)
}



#####################################################################################################
#See output for explanation of what is going on here!

plot_birth_rates <- function(gen_typhoons,monthly_birth_locations){
  
  count = 0
  
  
  for (month in unique(gen_typhoons$Month)){
    tempgen_typhoons <- gen_typhoons
    
    plot1 <- getplot_gen(tempgen_typhoons,month)
    plot2 <- getplot_hist(monthly_birth_locations,month)
    
    
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

    message(sprintf("%s", month))
    png(filename=sprintf(paste("output/generated/img%s.png" , sep = ''), name),
        width=1920, height=1080, type="cairo", bg="gray25")
    print(multiplot(plot1,plot2))
    dev.off()
    count = count +1
    
  }
}