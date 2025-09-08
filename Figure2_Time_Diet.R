### Figure2_Time
### Project: Ecology and Economics
### Lina M?hlbauer with changes by Daria Lipsky

rm(list = ls())


require(DescTools)
require(vegan)
require(scales)
require(akima)
library(dplyr)
library(ggplot2)


setwd("C:/Users/aa82eluj/Desktop/BirdInequity/")

#load results_BBS
results_BBS <- read.csv("data_2024/Results_BBS_complete_diet.csv")

routes70s_bool <- TRUE

if(routes70s_bool){
  #analysing subset of routes that were already surveyed in the 1970s to check if the effects we found are driven by newer routes.
  routes70s <- results_BBS %>% filter(Year<=1979) %>% select(State, Route)
  results_BBS <- results_BBS %>% #filter only values with State and Route in routes70s
    filter(paste(State, Route) %in% paste(routes70s$State, routes70s$Route))
  
}

MeanGini_year <- results_BBS %>%
  group_by(Year) %>%
  summarise(
    Mean_Gini = mean(GINI, na.rm = TRUE),
    Mean_N = mean(N, na.rm = TRUE),
    Mean_S = mean(S, na.rm = TRUE),
    Mean_E = mean(E_total, na.rm = TRUE),
    Mean_EN = mean(E_total / N, na.rm = TRUE),
    Nroutes = n(),
    .groups = "drop"
  )

if(routes70s_bool){
  pdf(width = 12, height = 10, file="figure_2_routes70s.pdf")
}else{
  pdf(width = 12, height = 10, file="figure_2_allroutes.pdf")
}
col <- viridisLite::viridis(6)
par(mfrow=c(3,2))
plot(MeanGini_year$Year, MeanGini_year$Mean_Gini, xlab = "", ylab="Mean(Gini)",
     xaxt="n", cex=1.2, col=col[1], pch = 16, cex.axis = 1.2, cex.lab=1.2)
mod1 <- lm(MeanGini_year$Mean_Gini~MeanGini_year$Year)
abline(mod1, col = col[1])
abline(v=1986, lty="dashed", col="black")
R1 <- summary(mod1)
legend("topright", legend = c(expression("adj."*R^2*"="),round(R1$adj.r.squared, digits=2)), bty="n")


plot(MeanGini_year$Year, MeanGini_year$Mean_N, xlab = "", ylab="Mean(N)", xaxt = "n", cex=1.2, col=col[2], pch = 16, cex.axis = 1.2, cex.lab=1.2)
mod2 <- lm(MeanGini_year$Mean_N~MeanGini_year$Year)
abline(mod2, col = col[2])
abline(v=1986, lty="dashed", col="black")
R2 <- summary(mod2)
legend("topright", legend = c(expression("adj."*R^2*"="),round(R2$adj.r.squared, digits=2)), bty="n")



plot(MeanGini_year$Year, MeanGini_year$Mean_S, xlab = "", ylab="Mean(S)", xaxt="n",cex=1.2, col=col[3], pch = 16, cex.axis = 1.2, cex.lab=1.2)
abline(v=1986, lty="dashed", col="black")
#create data.frame for storing year and slope
slope_year <- matrix(NA,nrow=length(unique(results_BBS$Year)), ncol=3)
colnames(slope_year) <- c("Year", "Slope_N_Gini", "Adj. R2")

years <- sort(unique(results_BBS$Year))
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

plot(MeanGini_year$Year, MeanGini_year$Mean_E, xlab ="Year", ylab = "Mean(Energyflux/route)", col=col[5], pch = 16, cex=1.2, cex.axis = 1.2, cex.lab=1.2)
abline(v=1986, lty="dashed", col="black")

plot(MeanGini_year$Year, MeanGini_year$Mean_EN, xlab ="Year", ylab = "Mean(Energyflux/ind.)", col=col[6], pch = 16, cex=1.2, cex.axis = 1.2, cex.lab=1.2)
mod3 <- lm(MeanGini_year$Mean_EN~MeanGini_year$Year)
abline(mod3, col = col[6])
abline(v=1986, lty="dashed", col="black")
R3 <- summary(mod3)
legend("bottomright", legend = c(expression("adj."*R^2*"="),round(R3$adj.r.squared, digits=2)), bty="n")

dev.off()

