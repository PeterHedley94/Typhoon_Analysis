
#Setup
libraries <- c('evd','plyr','maps','data.table','dplyr','ggplot2','grid','RColorBrewer','ggmap','geosphere','fitdistrplus','KernSmooth')
lapply(libraries, require, character.only = TRUE)

#Get typhoon data
source('02_01_Get_Data.R')
storms = get_data()
storms333 <- storms

#Return Graph styles to be used later
source('02_02_Graph_Style.R')
graph_base = get_style1(xlimits= c(50,180),ylimits = c(-10,60))
graph_base2 = get_style2(xlimits= c(50,180),ylimits = c(-10,60))
graph_base3 = get_style3(xlimits= c(50,180),ylimits = c(-10,60))
rm(knots_range)
rm(season_range)

# Deal with -180 to +180 longitude travelling typhoons
source('02_03_Removenegtopos.R')
storms_split <- split_storms(storms)

#Create a bounding box - return storms the area of interest
source('02_04_ZoneofInterest.R')
storms_inbasin = storms_format(storms, basin_long = c(105,180),basin_lat = c(0,45))
storms_inbasin_split = split_storms(storms_inbasin)

#Calculate the number of typhoons per year
source('02_05_Events_Per_Year.R')
storms_per_yr <- events_per_year(storms_inbasin)

#Number of typhoons in each month
source('02_06_Events_Per_Month.R')
storms_per_month <- events_per_month(storms_inbasin)

#Plot number of typhoons per month
source('02_07_Plot_Events_Per_Month.R')
plot_events_per_month(storms_per_month)

#Get typhoon genesis locations
source('02_08_Typhoon_Birth.R')
yearly_birth_locations <- birth_loc(storms)

#Plot typhoon birth locations - Stat_2d
source('02_09_Plot_Birth_Rates.R')
plot_birth_rates(storms_inbasin)
dev.off()

#Birth Locations Per Month and Nino Signal
source('02_10_Typhoon_BirthM.R')
monthly_birth_locations <- mbirth_loc(storms_inbasin)
global_monthly_birth_locations <- mbirth_loc(storms)

#Plot genisis stat_2d of Months with ENSO signal
source('02_11_Plot_Nino_BR.R')
plot_birth_rates(monthly_birth_locations,storms,folder = 'basin')
dev.off()
plot_birth_rates(global_monthly_birth_locations,storms,folder = 'global')
dev.off()

 #Plot the Storms on a global and basin plot
source('02_12_Plot_Storms.R')
plot_world_storms(storms_split)
dev.off()
plot_basin_storms(storms_inbasin_split)
dev.off()

#Convert the storms to Polar positions with origin 95,0
source('02_13_Storm2Polar.R')
#lat =0 long = 95
origin = c(95,0)
#note distances are currently in m
converted_to_polar = latlon2pol(storms_inbasin,origin)

#Create a variety of plots from Polar coords
source('02_14_Plot_Polar2.R')
plot_polar(converted_to_polar)

#Find 4 typhoons that curve and 4 that go straight
#Look at this before use!
source('02_15_Determine_8lines.R')

#Serial Correlation Test
source('02_16_Serial_Correlation_Test.R')
Serial_Correlation(storms_inbasin)

#Fit a Poisson model to the storm occurances each year.
source('02_17_Poisson_Dist.R')
mle_value <- fit_poisson(storms_per_yr)

#Create Monthly Profiles from historic data
source('02_18_Create_Profiles.R')
normalised <- 1
storms_dissag_per_month <- Create_profiles(storms_inbasin, normalised)
normalised <- 0
storms_dissag_per_month1 <- Create_profiles(storms_inbasin, normalised)

#Generate Events from poisson distribution
source('02_19_Poisson_Generation.R')
created_events <- round(create_events(mle_value,storms_dissag_per_month, 100))

#Plot number of events per month of generated data to check against historical
source('02_07_Plot_Events_Per_Month.R')
plot_total_events_per_month(storms_per_month)
Create_Boxplot(storms_dissag_per_month1)

#Use kernel smoothing to approximate the event genesis locations
source('02_20_Kernel_Density.R')
#lat =0 long = 95
origin2 = c(0,0)
xrange <- list(c(95,250), c(-10,60))
# - add - longitude genesis data as well to kernel
monthly_birth_locations_adjusted <- get_adjusted(monthly_birth_locations,origin2)
#generate the kernel
generated_kernel <- get_kernel(monthly_birth_locations,xrange)

#Now number of typhoons a year and density kernel randomly create typhoon events
source('02_21_Generate_Typhoons.R')
gen_typhoons <- generate_typhoons(generated_kernel,created_events,origin2)

source('02_22_Plot_Generated_Typhoons.R')
plot_gen_birth_locations(gen_typhoons,monthly_birth_locations)
plot_birth_locations(gen_typhoons,monthly_birth_locations)

#Plot genesis stat_2d of Months with ENSO signal
source('02_23_Plot_Gen_Typhoon_Births.R')
plot_birth_rates(gen_typhoons,monthly_birth_locations)
dev.off()


########################################################################################################
#Now event genesis locations are determined try to get path
#save.image('Load_File.RData')
#load('LoadFile.RData')
#rm(list = setdiff(ls(), lsf.str()))

source('02_24_Prep_Regression.R')
#lat =0 long = 95
origin = c(95,0)
granularity <- 5
regression_model <- divide_into_grid(storms_inbasin,origin,granularity)
#save(storms_inbasin,gen_typhoons,regression_model,file = 'Storms_inbasin.RData')
#load('Storms_inbasin.RData')

source('02_25_Generate_StartingData.R')
gen_typhoons <- fit_data(storms_inbasin,gen_typhoons)


source('02_25_Create_Paths.R')
gen_typhoons <- create_paths(gen_typhoons,regression_model)
save.image('Load_File.RData')

