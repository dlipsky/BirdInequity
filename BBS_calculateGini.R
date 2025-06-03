#####GINI - BBS ####
#####Project: Ecology and Economics####
#####Lina Mühlbauer#####

rm(list = ls())


require(DescTools)
require(vegan)
require(RColorBrewer)

#load raw data
States_combined <- read.csv("States_combined.csv")#load route BCR´s
routes_all <- read.csv("routes_BCR.csv", sep=";")


#vector for Route data ID (unique ID for year/route)
routes <- unique(States_combined$RouteDataID)


#create matrix for results
results_BBS <- matrix(data=NA,nrow=length(routes), ncol=7) #nrow=number of routes

colnames(results_BBS) <- c("State", "Route", "Year", "S","N", "GINI", "BCR")



#loop data selection for one year/ all data 
for (i in routes){
    temp.data <- States_combined[States_combined$RouteDataID==i,]
    
    
    state.ps <- as.integer(unique(temp.data$State))
    route.ps <- as.integer(unique(temp.data$Route))
    BCR.ps  <- routes_all[routes_all$Route==route.ps&routes_all$StateNum==state.ps,]
    run <- which(routes==i) #loop run nr?
    
      results_BBS[run,1] <- state.ps
      results_BBS[run,2] <- route.ps
      results_BBS[run,3] <- unique(temp.data$Year)      
      results_BBS[run,4] <- length(temp.data$SpeciesTotal)#calculate Nspecies=S
      results_BBS[run,5] <- sum(as.integer(temp.data$SpeciesTotal))#calculate NIndividuals=N
      results_BBS[run,6] <- Gini(as.integer(temp.data$SpeciesTotal))#calculate Gini
      #results_BBS[run,7] <- BCR.ps$BCR #save habitat type
}
