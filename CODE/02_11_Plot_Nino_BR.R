#02_11_Plot_Nino_BR.R


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

getplot <- function(tempmonthly_birth_locations,month,type){
  tempdata = tempmonthly_birth_locations[tempmonthly_birth_locations$Month==month,3:4]
  print(paste(month, ' | ', type, ' | Number |', dim(tempdata)[1]))
  colnames(tempdata) = c('lon','lat')
  plotted <- graph_base + xlim(-180,180) + ylim(-90,90) + geom_text(label=paste(type , ' | Month |', getmonth(month), '|', dim(tempdata)[1]), aes(x=75, y=40), size=rel(10), color="white", vjust=1)+ stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha = ..level..),size = 2, bins = 15, data = tempdata,geom = "polygon") + scale_alpha_continuous(limits = c(0,0.05)) + scale_fill_gradient(limits = c(0,0.05),low = "orange",high= "red")
}

#####################################################################################################
#See Below and output for explanation of what is going on here!

plot_birth_rates <- function(monthly_birth_locations,storms,folder){
  for (month in unique(monthly_birth_locations$Month)){
  
    for (type in c("Nino", "Nina", "Neither", "All")){
      if (type == "Nino"){
        tempmonthly_birth_locations <- monthly_birth_locations %>% filter(Nino > 0.4)
        p1 <- getplot(tempmonthly_birth_locations,month,type)
        
      }else if(type == "Nina"){
        tempmonthly_birth_locations <- monthly_birth_locations %>% filter(Nino < -0.4)
        p2 <- getplot(tempmonthly_birth_locations,month,type)
        
      }else if(type == "Neither"){
        tempmonthly_birth_locations <- monthly_birth_locations %>% filter(Nino >= -0.4, Nino <= 0.4)
        p3 <- getplot(tempmonthly_birth_locations,month,type)
        
      }else if(type == "All"){
        tempmonthly_birth_locations <- monthly_birth_locations
        p4 <- getplot(tempmonthly_birth_locations,month,type)
      }
    }

    message(sprintf("%s", month))
    png(filename=sprintf(paste("output/birth_loc/ENSO/",folder,"/img%s.png" , sep = ''), month),
        width=1920, height=1080, type="cairo", bg="gray25")
    print( multiplot(p1, p2, p3, p4, cols=2))
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
