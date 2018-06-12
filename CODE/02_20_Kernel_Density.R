#02_20_Kernel_Density.R

get_adjusted <- function(monthly_birth_locations,origin){
  monthly_birth_locations[monthly_birth_locations$Longitude < 0,]$Longitude <- 180 + (180 + monthly_birth_locations[monthly_birth_locations$Longitude < 0,]$Longitude)
  monthly_birth_locations$Longitude <- monthly_birth_locations$Longitude - origin[1]
  monthly_birth_locations$Latitude <- monthly_birth_locations$Latitude - origin[2]
  return(monthly_birth_locations)
}

##################################################################################################

create_probability <- function(birthlocd){
  
  probability <- data.frame(birthlocd['fhat'])
  colnames(probability) <-c(1:dim(probability[1])[1])
  probability <- stack(probability)
  probability$Selection <- c(1:dim(probability[1])[1])
  
  return(probability)
}


##################################################################################################

get_kernel <- function(monthly_birth_locations_adjusted,xrange){
  
  generated_grid <- vector("list", 12)
  generated_probability <- vector("list", 12)
  
  for (month in c(1:12)){
    print(month)
    mbl <- monthly_birth_locations_adjusted[monthly_birth_locations_adjusted$Month == month,c("Longitude","Latitude")]
    birthlocd <- bkde2D(mbl, bandwidth = sapply(mbl, dpik),range.x = (xrange))
    generated_grid[month] <- list(data.frame(birthlocd['x1'],birthlocd['x2']))
    generated_probability[month] <- list(create_probability(birthlocd))
    
    pdf(paste('Gen_',month,'.png',sep = ''))
    contour(x = birthlocd$x1, y = birthlocd$x2, z = birthlocd$fhat,xlab = "Longitude",ylab = "Lattitude")
    dev.off()
  }


  return(list(generated_grid,generated_probability))
}