#####Plot Gini and Metabolic rate ####
#####Project: Ecology and Economics####
#####Lina Mhlbauer, modified and updated by Daria Lipsky #####
######Version: 16.06.2025#####

require(DescTools)
require(vegan)
require(aomisc)
library(dplyr)

setwd("C:/Users/aa82eluj/Desktop/BirdInequity/")

#load analysis data
# results_BBS <- read.csv("data_2024/Results_BBS_complete.csv")
routes70s_bool <- FALSE

results_BBS <- read.csv("data_2024/Results_BBS_complete_diet.csv")

routesNPP <- read.csv("results/routesNPP.csv")


if(routes70s_bool){
  #analysing subset of routes that were already surveyed in the 1970s to check if the effects we found are driven by newer routes.
  routes70s <- results_BBS %>% filter(Year<=1979) %>% dplyr::select(State, Route)
  results_BBS <- results_BBS %>% #filter only values with State and Route in routes70s
    filter(paste(State, Route) %in% paste(routes70s$State, routes70s$Route))
  
}

# Join NPP data using State and Route as keys
results_BBS_updated <- results_BBS %>%
  left_join(routesNPP %>% select(StateNum, Route, rvalue_1),
            by = c("State" = "StateNum", "Route" = "Route")) %>%
  rename(NPP = rvalue_1)

#par(mfrow=c(1,2))
#plot(results_BBS$GINI, results_BBS$NPP, xlab="Gini", ylab="NPP")
#plot(results_BBS$GINI, results_BBS$E_total/results_BBS$N, xlab="Gini", ylab="Energyflux/ind.")

if(routes70s_bool){
  pdf(width = 12, height = 10, file="figure4_productivity_routes70s.pdf")
}else{
  pdf(width = 12, height = 10, file="figure4_productivity_allroutes.pdf")
}
par(mfrow=c(2,2))
par(mar = c(5.1, 5, 4.1, 2.1))

col <- viridisLite::viridis(5)

plot(1, type = "n",
     xlab = "", ylab = "log(Energydemand/ind.)", xaxt ="n",
     xlim = c(0.25, 0.95), ylim = c(1.8, 7 ), cex.axis=1.2, cex.lab=1.2)

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
       col=c(col[5], col[4], col[3], col[2], col[1]), pch =16, bty="n", cex=1.2 )

# #add a regression line
# mod2 <- lm(log(E_total/N) ~ GINI, data = results_BBS)
# abline(mod2, col="black", lwd=2)
# R2 <- summary(mod2)
# legend("topright", legend = c(expression("adj."*R^2*"="),round(R2$adj.r.squared, digits=2)), bty="n", cex = 1.4)


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
  # boxes[[run]] <- temp.data$E_total/temp.data$N
  boxes[[run]] <- log(temp.data$E_total/temp.data$N)
  
}

#pdf(height = 6, width=10, file="GINI_metabolic.ind_boxplot.pdf")
boxplot(boxes, xaxt = "n", xlab= "" , ylab="", yaxt="n")
#dev.off()

#plot3

plot(1, type = "n",
     xlab = "Gini", ylab = "log(Energydemand/route)",
     xlim = c(0.25, 1), ylim = c(4.5, 15), cex.axis=1.2, cex.lab=1.2)

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

mod2 <- lm(log(E_total) ~ GINI, data = results_BBS)
abline(mod2, col="black", lwd=2)
R2 <- summary(mod2)
legend("topright", legend = c(expression("adj."*R^2*"="),round(R2$adj.r.squared, digits=2)), bty="n", cex = 1.4)


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
  # boxes[[run]] <- temp.data$E_total
  boxes[[run]] <- log(temp.data$E_total)
}

#pdf(height = 6, width=10, file="GINI_metabolic.ind_boxplot.pdf")
boxplot(boxes, names=c("<0.3", "<0.4", "<0.5", "<0.6", "<0.7", "<0.8", "<0.9", "<1"), xlab= "Gini", ylab="", yaxt="n", cex.axis=1.2, cex.lab=1.2)


dev.off()

