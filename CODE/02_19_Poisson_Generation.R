#02_13_Poisson_Generation.R
#no_years = 10000

Create_Boxplot <- function(created_events){
  
  events <- vector(mode = "numeric")
  months <- vector(mode = "numeric")
  for(i in colnames(created_events)){
    print(i)
    events <- append(events,created_events[,i])
    months <- append(months,rep(c(i), dim(created_events)[1]))
  }
  months <- factor(months,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  to_plot <- data.frame(months,events)
  
  base <- ggplot(aes(months,events), data = to_plot) + geom_boxplot()#(stat = 'identity',position = "dodge")
  base <- base + xlab("Months") + ylab("Events | Month") + ggtitle("Typhoon Cycle") + theme_minimal() + coord_fixed(ratio = 0.8)
  pdf('Typhoon_boxplot_generated.pdf')
  plot(base)
  dev.off()
}

#####################################################################################################

bar_plot <- function(created_events){
  Months <- factor(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  to_plot <- data.frame(colnames(created_events), colSums(created_events))
  colnames(to_plot) <- c("Month", "No.Events")
  ggplot() + geom_boxplot(aes(x = Month, y = No.Events), data = to_plot)
  
  base <- ggplot(aes(Months,No.Events), data = to_plot) + geom_bar(stat = 'identity',position = "dodge")
  base <- base + xlab("Months") + ylab("Events | Month") + ggtitle("Typhoon Cycle") + theme_minimal() #+ coord_fixed(ratio = 0.5)
  
  pdf("Typhoon_cycle_generated.pdf")
  plot(base)
  dev.off()
}

#####################################################################################################

create_events <- function(mle_value,storms_dissag_per_month, no_years){
  
  #randomly generate number of events for each year using poisson model
  poisson <- rpois(no_years,mle_value)
  
  count = 1
  
  for (i in c(1:no_years)){
    
    #randomly choose a historical year and its distribution of typhoons over the months
    col_to_add <- poisson[i] * storms_dissag_per_month[,sample(1:dim(storms_dissag_per_month)[2],1)]
    
    #Create and amend the dataframe
    if (count == 1){
      events <- data.frame(data = col_to_add)
      rownames(events) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      colnames(events) = as.character(i)
      count = count +1
    }else{
      events[as.character(i)] = col_to_add
    }
  }
  
  created_events <- t(events)
  colnames(created_events) <- factor(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

  bar_plot(created_events)
  Create_Boxplot(created_events)
  
  return(events)
}