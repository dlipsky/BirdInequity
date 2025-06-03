#####BBS data quality check ####
#####Project: Ecology and Economics####
#####Lina Mühlbauer#####

rm(list = ls())


require(DescTools)
require(vegan)
require(RColorBrewer)

#load raw data
States_combined <- read.csv("States_combined.csv")#load route BCR´s

#load results
results_BBS <- read.csv("results_BBS_complete.csv")

#vector for Route data ID (unique ID for year/route)
routes.ID <- unique(States_combined$RouteDataID)
routes <- routes.ID[-c(3060)]

#assign route data ID to results 
results_BBS$RouteDataID <- routes
            
#only use high quality routes
#load weather data containing quality of each routeID, 1= high quality

weather <- read.csv("weather.csv")
highquality <- weather[weather$QualityCurrentID==1,]
routes.hq <- highquality$RouteDataID

#add column to store quality metric
results_BBS$Quality <- NA

for (i in routes.ID){
  ps <- weather[weather$RouteDataID==i,]
  
  quality <- ps$QualityCurrentID
  
  results_BBS[results_BBS$RouteDataID==i, "Quality"] <- quality
  
}

# quality.check <- function(RouteDataID) {
#   ps <- weather[RouteDataID,]
#   
#   quality <- ps$QualityCurrentID
# 
#   results_BBS[results_BBS$RouteDataID==RouteDataID, "Quality"]
# }
# 
# apply(results_BBS$RouteDataID, 1, quality.check)
