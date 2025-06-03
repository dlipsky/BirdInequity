#####Figure2_Time####
#####Project: Ecology and Economics####
#####Lina Mühlbauer#####

rm(list = ls())


require(DescTools)
require(vegan)
require(scales)
require(akima)

#load results_BBS
results_BBS <- read.csv("results_BBS_complete.csv")


#create data.frame for storing year and mean gini, mean N and mean S ...
MeanGini_year <- matrix(NA,nrow=length(unique(results_BBS$Year)), ncol=7)
colnames(MeanGini_year) <- c("Year", "Mean_Gini", "Mean_N","Mean_S", "Mean_E", "Mean_EN", "Nroutes")

#extract all years
years <- sort(unique(results_BBS$Year))

for (i in years){
  
  #calculate mean for Gini, N and S for each year
  mean_Gini <- mean(results_BBS[results_BBS$Year==i, "GINI"], na.rm=T)
  mean_N <- mean(results_BBS[results_BBS$Year==i, "N"], na.rm=T)
  mean_S <- mean(results_BBS[results_BBS$Year==i, "S"], na.rm=T)
  mean_E <- mean(results_BBS[results_BBS$Year==i, "E_total"], na.rm=T)
  mean_EN <- mean(results_BBS[results_BBS$Year==i, "E_total"]/results_BBS[results_BBS$Year==i, "N"], na.rm=T)
  n.routes <- length(results_BBS[results_BBS$Year==i, "GINI"])
  
  #check for run
  run <- which(years==i)
  #save year and means in matrix
  MeanGini_year[run, 1] <- i
  MeanGini_year[run, 2] <- mean_Gini
  MeanGini_year[run, 3] <- mean_N
  MeanGini_year[run, 4] <- mean_S
  MeanGini_year[run, 5] <- mean_E
  MeanGini_year[run, 6] <- mean_EN
  MeanGini_year[run, 7] <- n.routes
  
}

pdf(width = 12, height = 10, file="figure2.pdf")
col <- viridisLite::viridis(6)
par(mfrow=c(3,2))
plot(MeanGini_year[,1], MeanGini_year[,2], xlab = "", ylab="Mean(Gini)",
     xaxt="n", cex=1.2, col=col[1], pch = 16, cex.axis = 1.2, cex.lab=1.2)
mod1 <- lm(MeanGini_year[,2]~MeanGini_year[,1])
abline(mod1, col = col[1])
abline(v=1986, lty="dashed", col="black")
R1 <- summary(mod1)
legend("topright", legend = c(expression("adj."*R^2*"="),round(R1$adj.r.squared, digits=2)), bty="n")


plot(MeanGini_year[,1], MeanGini_year[,3], xlab = "", ylab="Mean(N)", xaxt = "n", cex=1.2, col=col[2], pch = 16, cex.axis = 1.2, cex.lab=1.2)
mod2 <- lm(MeanGini_year[,3]~MeanGini_year[,1])
abline(mod2, col = col[2])
abline(v=1986, lty="dashed", col="black")
R2 <- summary(mod2)
legend("topright", legend = c(expression("adj."*R^2*"="),round(R2$adj.r.squared, digits=2)), bty="n")



plot(MeanGini_year[,1], MeanGini_year[,4], xlab = "", ylab="Mean(S)", xaxt="n",cex=1.2, col=col[3], pch = 16, cex.axis = 1.2, cex.lab=1.2)
abline(v=1986, lty="dashed", col="black")
#create data.frame for storing year and slope
slope_year <- matrix(NA,nrow=length(unique(results_BBS$Year)), ncol=3)
colnames(slope_year) <- c("Year", "Slope_N_Gini", "Adj. R2")

for (i in years){
  
  #variables for relationship Gini vs. N per year
  x_N <- results_BBS[results_BBS$Year==i, "N"]
  y_Gini <- results_BBS[results_BBS$Year==i, "GINI"]
  
  mod1 <- lm(y_Gini~x_N) #linear model for variables
  slope <- coef(mod1)[2] #extract slope of model 
  summary <- summary(mod1)
  adj_r <- round(summary$adj.r.squared, digits=2)
  
  #check for run
  run <- which(years==i)
  #save year and slope in matrix
  slope_year[run, 1] <- i
  slope_year[run, 2] <- slope
  slope_year[run, 3] <- adj_r
}

#plot Gini slope/year vs. Year
plot(slope_year[,1], slope_year[,2], xlab="", ylab="Slope:Gini_N", cex=1.2, col=col[4], xaxt="n", pch = 16, cex.axis = 1.2, cex.lab=1.2)
mod4 <- lm(slope_year[,2]~ slope_year[,1])
abline(mod4, col = col[4])
abline(v=1986, lty="dashed", col="black")
R4 <- summary(mod4)
legend("topright", legend = c(expression("adj."*R^2*"="),round(R4$adj.r.squared, digits=2)), bty="n")

plot(MeanGini_year[,1], MeanGini_year[,5], xlab ="Year", ylab = "Mean(Energyflux/route)", col=col[5], pch = 16, cex=1.2, cex.axis = 1.2, cex.lab=1.2)
abline(v=1986, lty="dashed", col="black")
plot(MeanGini_year[,1], MeanGini_year[,6], xlab ="Year", ylab = "Mean(Energyflux/ind.)", col=col[6], pch = 16, cex=1.2, cex.axis = 1.2, cex.lab=1.2)
mod3 <- lm(MeanGini_year[,6]~MeanGini_year[,1])
abline(mod3, col = col[6])
abline(v=1986, lty="dashed", col="black")
R3 <- summary(mod3)
legend("bottomright", legend = c(expression("adj."*R^2*"="),round(R3$adj.r.squared, digits=2)), bty="n")


dev.off()