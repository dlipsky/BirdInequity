#####BBS data quality check ####
#####Project: Ecology and Economics####
#####Lina Mhlbauer, modified and updated by Daria Lipsky #####

require(DescTools)
require(vegan)
require(RColorBrewer)

#load raw data
States_combined <- read.csv("data_2024/States_combined_2024.csv")#load route BCR?s

#load results
results_BBS <- read.csv("data_2024/results_BBS_2024_raw.csv")

#only use high quality routes
#load weather data containing quality of each routeID, 1= high quality

weather <- read.csv("data_2024/weather.csv")
highquality <- weather[weather$QualityCurrentID==1,]
routes.hq <- highquality$RouteDataID

#assign route data ID to results 
results_BBS$RouteDataID <- unique(States_combined$RouteDataID)

#remove routes that are not in the weather data
# results_BBS <- results_BBS[!(results_BBS$RouteDataID %in% c(6379693,6379098)),] 
results_BBS <- results_BBS[results_BBS$RouteDataID %in% weather$RouteDataID,]

results_BBS <- merge(results_BBS, weather[, c("RouteDataID", "QualityCurrentID")],
                     by = "RouteDataID", all.x = TRUE)

# Rename for consistency
names(results_BBS)[names(results_BBS) == "QualityCurrentID"] <- "Quality"

#save new results
write.csv(results_BBS, "data_2024/results_BBS_2024.csv", row.names = FALSE)
