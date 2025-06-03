#####Figure1_GINI_S_N_3D####
#####Project: Ecology and Economics####
#####Lina Mühlbauer#####

rm(list = ls())

require(DescTools)
require(vegan)
require(scales)
require(akima)

#load results_BBS
results_BBS <- read.csv("results_BBS_complete.csv")

###plot###
pdf(width=12, height = 6, file="figure_1.pdf")
par(mfrow=c(1,2))

# quality
results_BBS_1 <- results_BBS[results_BBS$Quality==0,]
col <- viridisLite::cividis(2)
plot(results_BBS$S, results_BBS$GINI, xlab="S", ylab="Gini", cex=0.4)
points(results_BBS_1$S, results_BBS_1$GINI, col="red")

plot(log(results_BBS$N), results_BBS$GINI, xlab="log(N)", ylab="", cex=0.4)
points(log(results_BBS_1$N), results_BBS_1$GINI, col="red")
mod1 <- lm(results_BBS$GINI~log(results_BBS$N))
abline(mod1, col =col[1])
modsum1 <- summary(mod1)

legend("bottomright", legend = c(expression("adj."*R^2*"="),round(modsum1$adj.r.squared, digits=2)), bty="n")

dev.off()

#set variables for contour.plot
x <- results_BBS$S
y <- results_BBS$N
z <- results_BBS$GINI
#remove missing value combinations
ps <- which(is.finite(x)&is.finite(y)&is.finite(z))

#interpolate data (Gridded Bivariate Interpolation)
xo <- seq(min(x[ps])-2, max(x[ps])+2, 1)
yo <- seq(min(y[ps])-2, max(y[ps])+2, length.out = 103)
interp_xyz <- interp(x[ps],y[ps],z[ps], xo=xo, yo=yo, duplicate="mean")

#plot filled.contour
pdf(width=12, height = 8, file="figure_1b_basicrelationships.pdf")
filled.contour(interp_xyz, xlab="S", ylab="N", 
               key.title=title(main = "Gini"), color.palette = viridis_pal(),
               plot.axes ={axis(1); axis(2); 
                 points(jitter(x[ps], 1), y[ps], cex=0.4)})#adds points
dev.off()

#plot heatmap

#remove low quality counts
results_BBS <- results_BBS[results_BBS$Quality==1,]

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
pdf()
levelplot(x, xlab="S", ylab="log(N)", col.regions=viridis_pal(), pretty=T)
dev.off()

