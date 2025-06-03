#####GINI per route over all years - BBS ####
#####Project: Ecology and Economics####
#####Lina Mühlbauer#####
######Version: 29.04.2021#####
rm(list = ls())

require(DescTools)
require(vegan)
require(RColorBrewer)

#load results_BBS
results_BBS <- read.csv("~/Project Economy_Ecology/Results/results_BBS_14-06-21.csv")

#plot results for each route over all years
#pdf(width=10, height=6, file="Gini_Year_BCR.pdf")
plot(c(1966, 2019),c(0.45, 0.85), type = "n",             
     xlab = "Year", ylab = "Gini")
bcr <- unique(results_BBS$BCR)
col <- viridisLite::viridis(length(bcr))


for (i in bcr){
  temp.data <- results_BBS[results_BBS$BCR==i,]
  years <- sort(unique(temp.data$Year))
  y <- rep(NA, length(years))
  
  for (j in years) {
    temp_year <- temp.data[temp.data$Year==j,]
    mean_Gini <- mean(temp_year$GINI)
    run <- which(years==j)
    
    y[run] <- mean_Gini
  }
  lines(years, y, col=col[which(bcr==i)])
  text(years[1], y[1], labels=i, cex= 0.7, pos=1, col=col[which(bcr==i)])
}
#dev.off()

#plot Energy flux for each route over all years
#pdf(width=10, height=6, file="Energyflux_Year_BCR.pdf")
plot(c(1966, 2019),c(1000, 120000), type = "n",             
     xlab = "Year", ylab = "Total Energy flux")
bcr <- unique(results_BBS$BCR)
col <- viridisLite::viridis(length(bcr))


for (i in bcr){
  temp.data <- results_BBS[results_BBS$BCR==i,]
  years <- sort(unique(temp.data$Year))
  y <- rep(NA, length(years))
  
  for (j in years) {
    temp_year <- temp.data[temp.data$Year==j,]
    mean_Gini <- mean(temp_year$E_total)
    run <- which(years==j)
    
    y[run] <- mean_Gini
  }
  lines(years, y, col=col[which(bcr==i)])
  text(years[length(years)], y[length(y)], labels=i, cex= 0.7, pos=1, col=col[which(bcr==i)])
}
#dev.off()


#plot Energy flux/ind. for each route over all years
#pdf(width=10, height=6, file="Energyflux/ind_Year_BCR.pdf")
plot(c(1966, 2019),c(0, 150), type = "n",             
     xlab = "Year", ylab = "Energy flux/ind.")
bcr <- unique(results_BBS$BCR)

col <- viridisLite::viridis(length(bcr))

results_BBS <- head(results_BBS, -1)

for (i in bcr){
  temp.data <- results_BBS[results_BBS$BCR==i,]
  years <- sort(unique(temp.data$Year))
  y <- rep(NA, length(years))
  
  for (j in years) {
    temp_year <- temp.data[temp.data$Year==j,]
    mean_Gini <- mean(temp_year$E_total/temp_year$N)
    run <- which(years==j)
    
    y[run] <- mean_Gini
  }
  lines(years, y, col=col[which(bcr==i)])
  #text(years[length(years)], y[length(y)], labels=i, cex= 0.7, pos=1, col=col[which(bcr==i)])
}

#for each route
results_BBS$StateRoute <- paste(results_BBS$State, results_BBS$Route, sep="_")

pdf(width=10, height=8, file="Energyind_allroutes_time.pdf")
plot(c(1966, 2019),c(10, 90), type = "n",             
     xlab = "Year", ylab = "Energy flux/ind.")
routes <- unique(results_BBS$StateRoute)

col <- viridisLite::viridis(length(bcr))


for (i in routes){
  temp.data <- results_BBS[results_BBS$StateRoute==i,]
  years <- sort(unique(temp.data$Year))
  y <- rep(NA, length(years))
  
  for (j in years) {
    temp_year <- temp.data[temp.data$Year==j,]
    mean_Gini <- mean(temp_year$E_total/temp_year$N)
    run <- which(years==j)
    
    y[run] <- mean_Gini
  }
  lines(years, y, col=col[which(routes==i)])
  #text(years[length(years)], y[length(y)], labels=i, cex= 0.7, pos=1, col=col[which(bcr==i)])
}
