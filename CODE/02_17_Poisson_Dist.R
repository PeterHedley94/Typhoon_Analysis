#02_17_Poisson_Dist.R


fit_poisson <- function(storms_per_yr){
  
  #Use fitdistrplus package to fit a Poisson distribution.
  Events <- storms_per_yr$No.Events
  fpois <- fitdist(Events, distr = "pois")
  print(summary(fpois))
  
  #Useful Plot
  
  #plotdist(Events,leftNA = 0)
  #x <- data.frame(rpois(500000,fpois$estimate[[1]]))
  #colnames(x) <- c('values')
  #graph <- ggplot(data=storms_per_yr, aes(storms_per_yr$No.Events)) + geom_histogram(aes(y =..density..),bins = 13,fill = NA,color = 'black')# + geom_density(fill = 'red',col=2,alpha = 0.4)
  #graph <- graph +  geom_density(aes(values),alpha = 0.4,fill = 'blue',data = x) + theme_bw() + scale_x_continuous(name = 'No. Events Per Year') + scale_y_continuous(name = 'Density | Probability')
  #storms_dissag_per_month <- data.frame(data = None, colnames <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

  return(fpois$estimate[[1]])
}