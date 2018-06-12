#02_02_Graph_Style.R

# return different map styles depending on the plot
# note that limits can be changed at a later stage as well

get_style1 <- function(xlimits,ylimits){
  
  base <- ggplot()
  base <- base + geom_polygon(data=map_data("world"),
                              aes(x=long, y=lat, group=group),
                              fill="gray25", colour="gray25", size=0.2)
  base <- base + scale_color_gradientn(colours=rev(brewer.pal(n=9, name="RdBu")),
                                       space="Lab", limits=knots_range)
  base <- base + coord_map()
  base <- base + labs(x=NULL, y=NULL, title=NULL, colour = "Wind (knots)")
  base <- base + theme_bw() + xlim(xlimits[1],xlimits[2]) + ylim(ylimits[1],ylimits[2])
  base <- base + theme(text=element_text(size=rel(5)),
                       panel.background = element_rect(fill = "gray10", colour = "gray30"),
                       panel.spacing = unit(c(0,0), "lines"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.margin = unit(c(0,0,0,0), "lines"),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       legend.position = c(0.25, 0.1),
                       legend.background = element_rect(fill="gray10", color="gray10"),
                       legend.text = element_text(color="white", size=rel(2)),
                       legend.title = element_text(color="white", size=rel(5)),
                       legend.direction = "horizontal")
  return(base)
}

######################################################################################################

get_style2 <- function(xlimits,ylimits){
  
  base <- ggplot()
  #Retrieve world map from the ggplot2 package. See ?map_data for examples
  base <- base + geom_polygon(data=map_data("world"),
                              aes(x=long, y=lat, group=group),
                              fill="gray25", colour="gray25", size=0.2)
  base <- base + scale_color_gradientn(colours=rev(brewer.pal(n=9, name="RdBu")),
                                       space="Lab", limits=knots_range)
  base <- base + coord_map()
  base <- base + labs(x=NULL, y=NULL, title=NULL, colour = "Wind (knots)")
  base <- base + theme_bw() + xlim(xlimits[1],xlimits[2]) + ylim(ylimits[1],ylimits[2])
  base <- base + theme(text=element_text(family="Arial", face="plain", size=rel(5)),
                       panel.background = element_rect(fill = "gray10", colour = "gray30"),
                       panel.spacing = unit(c(0,0), "lines"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.margin = unit(c(0,0,0,0), "lines"),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       legend.position="none")
  return(base)
}

######################################################################################################

get_style3 <- function(xlimits,ylimits){
  
  xlimits= c(80,180)
  ylimits = c(-30,50)
  #Retrieve world map from the ggplot2 package. See ?map_data for examples
  world.map <- map_data("world")
  
  base <- ggplot()+
    geom_polygon(data = world.map, aes(x = long, y = lat, group = group),fill = "white",alpha=0.2) + 
    theme_classic()+
    theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(3, 0, 0, 0),"mm"),legend.text = element_text(size = 6),legend.title = element_text(size = 8, face = "plain"),panel.background = element_rect(fill='black'))+
    coord_fixed(ylim = ylimits, xlim = xlimits) + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank())# +
    #geom_text(aes(x=35,y=-69),label=paste("Hurricanes recorded since 1971"),color="white",hjust=0, size=3.5) #Add text
  base = base + scale_color_gradientn(colours=rev(brewer.pal(n=9, name="RdBu")),space="Lab", limits=knots_range)

  return(base)
}
