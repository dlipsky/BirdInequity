#####Plot Gini and Metabolic rate ####
#####Project: Ecology and Economics####
#####Lina Mühlbauer#####
######Version: 29.04.2021#####
rm(list = ls())

require(DescTools)
require(vegan)
require(aomisc)

#load analysis data
results_BBS <- read.csv("~/Project Economy_Ecology/Results/results_BBS_complete.csv")

#remove last NA column
results_BBS <- head(results_BBS, -1)

#create new column for storing NPP results

results_BBS$NPP <- NA

#load NPP data/route
routesNPP <- read.csv("~/Project Economy_Ecology/Results/routesNPP.csv")
states <- unique(results_BBS$State)

for (i in states){
  temp.data <- results_BBS[results_BBS$State==i,]
  routes <- unique(temp.data$Route, na.rm = T)
  
  temp.NPP <- routesNPP[routesNPP$StateNum==i,]
  
  for (j in routes){
    route <- temp.NPP[temp.NPP$Route==j,]
    results_BBS[results_BBS$State==i&results_BBS$Route==j,"NPP"] <- rep(route$rvalue_1, length(results_BBS[results_BBS$State==i&results_BBS$Route==j,"NPP"]))
  }
  
}

#par(mfrow=c(1,2))
#plot(results_BBS$GINI, results_BBS$NPP, xlab="Gini", ylab="NPP")
#plot(results_BBS$GINI, results_BBS$E_total/results_BBS$N, xlab="Gini", ylab="Energyflux/ind.")

pdf(width=12, height=10, file="figure4_productivity_wolog.pdf")
par(mfrow=c(2,2))
col <- viridisLite::viridis(5)

plot(1, type = "n",
     xlab = "", ylab = "log(Energyflux/ind.)", xaxt ="n",
     xlim = c(0.25, 0.95), ylim = c(1.8, 7 ))

data3 <- results_BBS[results_BBS$N<1000&results_BBS$N>=100,]
points(data3$GINI, log(data3$E_total/data3$N), col=col[5], pch=16, cex=0.4)

data4 <- results_BBS[results_BBS$N<10000&results_BBS$N>=1000,]
points(data4$GINI, log(data4$E_total/data4$N), col=col[4], pch=16, cex=0.4)

data2 <- results_BBS[results_BBS$N<100&results_BBS$N>=10,]
points(data2$GINI, log(data2$E_total/data2$N), col=col[3], pch=16, cex=0.4)

data1 <- results_BBS[results_BBS$N>10000,]
points(data1$GINI, log(data1$E_total/data1$N), col=col[2], pch=16, cex=0.4)

data5 <- results_BBS[results_BBS$N<10,]
points(data5$GINI, log(data5$E_total/data5$N), col=col[1], pch=16, cex=0.4)

legend("topleft",legend=c("N<10", "N<100", "N<1000", "N<10000", "N>10000"),
       col=c(col[5], col[4], col[3], col[2], col[1]), pch =16, bty="n" )
#plot2
#Boxplot with binned values of metabolic rate/individual
y <- log(results_BBS$E_total/results_BBS$N)
boxes <- list()
gini_borders <- seq(0.2, 0.9, 0.1)

for (i in gini_borders){
  run <- which(gini_borders==i)
  bottom_limit <- gini_borders[run+1]
  
  if (is.na(bottom_limit)) {
    temp.data <- results_BBS[results_BBS$GINI >= i & results_BBS$GINI < 1,]
  } else {
    temp.data <- results_BBS[results_BBS$GINI >= i & results_BBS$GINI < bottom_limit,]
  }
  boxes[[run]] <- temp.data$E_total/temp.data$N
}

#pdf(height = 6, width=10, file="GINI_metabolic.ind_boxplot.pdf")
boxplot(boxes, xaxt = "n", xlab= "" , ylab="", yaxt="n")
#dev.off()

#plot3

plot(1, type = "n",
     xlab = "Gini", ylab = "log(Energyflux/route)",
     xlim = c(0.25, 1), ylim = c(4.5, 15))

data3 <- results_BBS[results_BBS$N<1000&results_BBS$N>=100,]
points(data3$GINI, log(data3$E_total), col=col[5], pch=16, cex=0.4)

data4 <- results_BBS[results_BBS$N<10000&results_BBS$N>=1000,]
points(data4$GINI, log(data4$E_total), col=col[4], pch=16, cex=0.4)

data2 <- results_BBS[results_BBS$N<100&results_BBS$N>=10,]
points(data2$GINI, log(data2$E_total), col=col[3], pch=16, cex=0.4)

data1 <- results_BBS[results_BBS$N<10,]
points(data1$GINI, log(data1$E_total), col=col[2], pch=16, cex=0.4)

data5 <- results_BBS[results_BBS$N>10000,]
points(data5$GINI, log(data5$E_total), col=col[1], pch=16, cex=0.4)


#results_BBS[results_BBS$NPP>200,]

y <- log(results_BBS$E_total)
boxes <- list()
gini_borders <- seq(0.2, 0.9, 0.1)

for (i in gini_borders){
  run <- which(gini_borders==i)
  bottom_limit <- gini_borders[run+1]
  
  if (is.na(bottom_limit)) {
    temp.data <- results_BBS[results_BBS$GINI >= i & results_BBS$GINI < 1,]
  } else {
    temp.data <- results_BBS[results_BBS$GINI >= i & results_BBS$GINI < bottom_limit,]
  }
  boxes[[run]] <- temp.data$E_total
}

#pdf(height = 6, width=10, file="GINI_metabolic.ind_boxplot.pdf")
boxplot(boxes, names=c("<0.3", "<0.4", "<0.5", "<0.6", "<0.7", "<0.8", "<0.9", "<1"), xlab= "Gini", ylab="", yaxt="n")


dev.off()
