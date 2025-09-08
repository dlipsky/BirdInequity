#####Figure2_Time####
#####Project: Ecology and Economics####
#####Lina M?hlbauer#####

rm(list = ls())


require(DescTools)
require(vegan)
require(scales)
require(akima)

setwd("C:/Users/aa82eluj/Desktop/BirdInequity/")

routes70s_bool <- FALSE
results_BBS <- read.csv("data_2024/Results_BBS_complete_diet.csv")

if(routes70s_bool){
  #analysing subset of routes that were already surveyed in the 1970s to check if the effects we found are driven by newer routes.
  routes70s <- results_BBS %>% filter(Year<=1979) %>% dplyr::select(State, Route)
  results_BBS <- results_BBS %>% #filter only values with State and Route in routes70s
    filter(paste(State, Route) %in% paste(routes70s$State, routes70s$Route))
  
}

years <- sort(unique(results_BBS$Year))
col <- viridisLite::viridis(8)


MeanGini_year <- results_BBS %>%
  group_by(Year) %>%
  summarise(
    Mean_Gini = mean(GINI, na.rm = TRUE),
    Mean_N = mean(N, na.rm = TRUE),
    Mean_S = mean(S, na.rm = TRUE),
    # Mean_E = mean(E_total, na.rm = TRUE), #those where original bodymass calculations
    # Mean_EN = mean(E_total / N, na.rm = TRUE),
    Nroutes = n(),
    Mean_E_diet = mean(E_total_diet, na.rm = TRUE),
    Mean_EN_diet = mean(E_total_diet/N, na.rm = TRUE),
    Mean_E_Plant = mean(E_total_PlantSeed, na.rm = TRUE),
    Mean_E_Omni = mean(E_total_Omnivore, na.rm = TRUE),
    Mean_E_Inv = mean(E_total_Inv, na.rm = TRUE),
    Mean_E_Vert = mean(E_total_Vert, na.rm = TRUE),
    Mean_E_Fruit = mean(E_total_FruiNec, na.rm = TRUE),
    Mean_S_Plant = mean(S_PlantSeed, na.rm = TRUE),
    Mean_S_Omni = mean(S_Omnivore, na.rm = TRUE),
    Mean_S_Inv = mean(S_Inv, na.rm = TRUE),
    Mean_S_Vert = mean(S_Vert, na.rm = TRUE),
    Mean_S_Fruit = mean(S_FruiNec, na.rm = TRUE),
    Mean_N_Plant = mean(N_Plantseed, na.rm = TRUE),
    Mean_N_Omni = mean(N_Omnivore, na.rm = TRUE),
    Mean_N_Inv = mean(N_Inv, na.rm = TRUE),
    Mean_N_Vert = mean(N_Vert, na.rm = TRUE),
    Mean_N_Fruit = mean(N_FruiNec, na.rm = TRUE),
    Mean_trophiclevel = mean(Ave_trophiclevel, na.rm = TRUE),
    Mean_bodymass = mean(Ave_bodymass, na.rm = TRUE),
    .groups = "drop"
  )

if(routes70s_bool){
  pdf(width = 10, height = 10, file="figure_2_8plots_routes70.pdf")
}else{
  pdf(width = 10, height = 10, file="figure_2_8plots_allroutes.pdf")
}
par(mfrow=c(4,2))
par(mar = c(5.1, 5, 4.1, 2.1))
plot(MeanGini_year$Year, MeanGini_year$Mean_Gini, xlab = "", ylab="Mean(Gini)",
     xaxt="n", cex=1.2, col=col[1], pch = 16, cex.axis = 1.4, cex.lab=1.4)
mod1 <- lm(MeanGini_year$Mean_Gini~MeanGini_year$Year)
abline(mod1, col = col[1])
abline(v=1986, lty="dashed", col="black")
R1 <- summary(mod1)
legend("topright", legend = c(expression("adj."*R^2*"="),round(R1$adj.r.squared, digits=2)), bty="n", cex=1.4)


plot(MeanGini_year$Year, MeanGini_year$Mean_N, xlab = "", ylab="Mean(N)", xaxt = "n", cex=1.2, col=col[2], pch = 16, cex.axis = 1.4, cex.lab=1.4)
mod2 <- lm(MeanGini_year$Mean_N~MeanGini_year$Year)
abline(mod2, col = col[2])
abline(v=1986, lty="dashed", col="black")
R2 <- summary(mod2)
legend("topright", legend = c(expression("adj."*R^2*"="),round(R2$adj.r.squared, digits=2)), bty="n", cex = 1.4)



plot(MeanGini_year$Year, MeanGini_year$Mean_S, xlab = "", ylab="Mean(S)", xaxt="n",cex=1.2, col=col[3], pch = 16, cex.axis = 1.4, cex.lab=1.4)
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
plot(slope_year[,1], slope_year[,2], xlab="", ylab="Slope:Gini_N", cex=1.2, col=col[4], xaxt="n", pch = 16, cex.axis = 1.4, cex.lab=1.4)
mod4 <- lm(slope_year[,2]~ slope_year[,1])
abline(mod4, col = col[4])
abline(v=1986, lty="dashed", col="black")
R4 <- summary(mod4)
legend("topright", legend = c(expression("adj."*R^2*"="),round(R4$adj.r.squared, digits=2)), bty="n", cex = 1.4)

#add two plots: bodymass and trophic level
plot(MeanGini_year$Year, MeanGini_year$Mean_bodymass, xlab ="", ylab = "Mean(Bodymass)", col=col[5],xaxt="n", pch = 16, cex=1.2, cex.axis = 1.4, cex.lab=1.4)
abline(v=1986, lty="dashed", col="black")
mod3 <- lm(MeanGini_year$Mean_bodymass~MeanGini_year$Year)
abline(mod3, col = col[5])
R3 <- summary(mod3)
legend("bottomright", legend = c(expression("adj."*R^2*"="),round(R3$adj.r.squared, digits=2)), bty="n", cex=1.4)

plot(MeanGini_year$Year, MeanGini_year$Mean_trophiclevel, xlab ="", ylab = "Mean(Trophic level)", col=col[6], xaxt="n",pch = 16, cex=1.2, cex.axis = 1.4, cex.lab=1.4)
abline(v=1986, lty="dashed", col="black")

plot(MeanGini_year$Year, MeanGini_year$Mean_E_diet, xlab ="Year", ylab = "Mean(Energydemand/route)", col=col[7], pch = 16, cex=1.2, cex.axis = 1.4, cex.lab=1.4)
abline(v=1986, lty="dashed", col="black")
plot(MeanGini_year$Year, MeanGini_year$Mean_EN_diet, xlab ="Year", ylab = "Mean(Energydemand/ind.)", col=col[8], pch = 16, cex=1.2, cex.axis = 1.4, cex.lab=1.4)
mod3 <- lm(MeanGini_year$Mean_EN_diet~MeanGini_year$Year)
abline(mod3, col = col[8])
abline(v=1986, lty="dashed", col="black")
R3 <- summary(mod3)
legend("bottomright", legend = c(expression("adj."*R^2*"="),round(R3$adj.r.squared, digits=2)), bty="n", cex = 1.4)

dev.off()

