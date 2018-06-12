# #Kernel Density Practice
# library(gridExtra)
# library(ggalt)
# library(scales)
# library("KernSmooth")

get_kernel <- function(monthly_birth_locations){
  mbl <- monthly_birth_locations[,c("Longitude","Latitude")]
  birthlocd <- bkde2D(mbl, bandwidth = sapply(mbl, dpik))
  # contour(x = birthlocd$x1, y = birthlocd$x2, z = birthlocd$fhat,xlab = "Longitude",ylab = "Lattitude")
  # persp(x = birthlocd$x1, y = birthlocd$x2, z = birthlocd$fhat,xlab = "Longitude",
  #         xlim = c(60,180), ylim = c(0,50),
  #       ylab = "Lattitude",zlab = "estimated density",theta = -35, axes = TRUE, box = TRUE)
  
  return(birthlocd)

}