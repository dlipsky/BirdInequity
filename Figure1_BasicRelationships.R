### Figure1_GINI_S_N_3D
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

routes70s_bool <- FALSE
results_BBS <- read.csv("data_2024/results_BBS_2024.csv")

if(routes70s_bool){
  #analysing subset of routes that were already surveyed in the 1970s to check if the effects we found are driven by newer routes.
  routes70s <- results_BBS %>% filter(Year<=1979) %>% dplyr::select(State, Route)
  results_BBS <- results_BBS %>% #filter only values with State and Route in routes70s
    filter(paste(State, Route) %in% paste(routes70s$State, routes70s$Route))
  
}

#### 1 Plot basic relationships between S, N and GINI ####
if(routes70s_bool){
  pdf(width=6, height = 6, file="figure_1a_Ginivslog(N)_routes70s.pdf")
}else{
  pdf(width=6, height = 6, file="figure_1a_Ginivslog(N)_allroutes.pdf")
}
ggplot(results_BBS, aes(x=log(N), y=GINI)) +
  geom_point(size=0.4) +
  labs(x="log(N)", y="Gini") +
  theme_bw() +
  theme(legend.position = "bottomright", text = element_text(size=20)) +
  stat_smooth(method="lm", col="black") +
  annotate("text", x=0, y=max(results_BBS$GINI, na.rm=T), 
           # label=paste("adj.R^2 =", round(summary(lm(GINI ~ log(N), data=results_BBS))$adj.r.squared, digits=2)), #do it with bquote
           label=bquote("adj."*R^2*"="*.(round(summary(lm(GINI ~ log(N), data=results_BBS))$adj.r.squared, digits=2))),
           hjust=0, vjust=1, size=5)
dev.off()

if(routes70s_bool){
  pdf(width=6, height = 6, file="figure_1b_GinivsS_routes70s.pdf")
}else{
  pdf(width=6, height = 6, file="figure_1b_GinivsS_allroutes.pdf")
}
ggplot(results_BBS, aes(x=S, y=GINI)) +
  geom_point(size=0.4) +
  geom_point(data=results_BBS, aes(x=S, y=GINI)) +
  labs(x="S", y="Gini") +
  theme_bw() +
  theme(legend.position = "topright", text = element_text(size=20))
dev.off()


#### 2 Contour plot####
#set variables for contour.plot
x <- results_BBS$S
y <- results_BBS$N
z <- results_BBS$GINI
#remove missing value combinations, gives indices of finite values (not NA or infinite)
ps <- which(is.finite(x)&is.finite(y)&is.finite(z))

#interpolate data (Gridded Bivariate Interpolation)
#xo and yo create a grid of S and N values slightly wider than the observed data
xo <- seq(min(x[ps])-2, max(x[ps])+2, 1)
yo <- seq(min(y[ps])-2, max(y[ps])+2, length.out = 103)
#interp() performs a bivariate interpolation of the Gini values over this grid,
#smoothing the relationship between S, N and GINI
#duplicate="mean" handles duplicated (x, y) pairs by averaging the z values
interp_xyz <- interp(x[ps],y[ps],z[ps], xo=xo, yo=yo, duplicate="mean")

#plot filled.contour

if(routes70s_bool){
  pdf(width=10, height = 8, file="figure_1c_contourSNGini_routes70s.pdf")
}else{
  pdf(width=10, height = 8, file="figure_1c_contourSNGini_allroutes.pdf")
}
par(mar = c(5, 5,5,5))
filled.contour(interp_xyz, xlab="S", ylab="N", #change font size
               cex.lab = 1.5, plot.axes={
                 axis(1,cex.axis=1.2)
                 axis(2,cex.axis=1.2)
               },
               key.title=title(main = "Gini"), color.palette = viridis_pal()
               )
dev.off()

par(mar = c(5.1, 4.1, 4.1, 2.1))

#### 3 Heatmap of binned S and log(N) values ####
#plot heatmap

#add column with logged N values 
results_BBS$logN <- log(results_BBS$N)
#create matrix for binned values
min(results_BBS$S, na.rm =T)
max(results_BBS$S, na.rm = T)

min(results_BBS$logN, na.rm =T)
max(results_BBS$logN, na.rm = T)

#bins: 0:10, 10:20, 20:30, 30:40, 40:50 ...
lower.s <- seq(0,110, by=10)
upper.s <- seq(10,120,by=10)

lower.n <- seq(0,9, by=1)
upper.n <- seq(1,10, by=1)

heatmap.sn <- matrix(0,nrow=length(upper.s), ncol=(length(upper.n)))

for (i in 1:12){
  temp.data <- results_BBS[results_BBS$S<upper.s[i]&results_BBS$S>=lower.s[i],]
  gini.row <- rep(NA,10)
  
  if(is.finite(temp.data$GINI[1])){
    
  for (j in 1:10){
    n.data <- temp.data[temp.data$logN<upper.n[j]&temp.data$logN>=lower.n[j],]
    
    if(length(n.data$GINI)==0){
      gini.row[j] <- NA
    }else{
    gini.row[j] <- mean(n.data$GINI, na.rm =T)}
  }}else{gini.row <- rep(NA,10)}

  heatmap.sn[i,] <- gini.row
}

x <- heatmap.sn
colnames(x) <- upper.n
rownames(x) <- upper.s
require(scales)
require(lattice)
if(routes70s_bool){
  pdf(file="figure_1d_heatmap_routes70s.pdf")
}else{
  pdf(file="figure_1d_heatmap_allroutes.pdf")
}
levelplot(x, xlab="S", ylab="log(N)", col.regions=viridis_pal(), pretty=T)
dev.off()

