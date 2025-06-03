#####S2_Energy per individual and route####
#####Project: Ecology and Economics####
#####Lina Mühlbauer#####

rm(list = ls())


require(DescTools)
require(vegan)
require(scales)
require(akima)

#load results_BBS
results_BBS <- read.csv("results_BBS_complete.csv")



pdf(width= 8, height =6, file="Eind_Eroute_relation.pdf")
plot(results_BBS$E_total, results_BBS$E_total/results_BBS$N,
     xlab="Energy Flux/Route", ylab="Energy Flux/Individual")
dev.off()

results <- results_BBS[results_BBS$Year==1966,]
plot(results$E_total, results$E_total/results$N,
     xlab="Energy Flux/Route", ylab="Energy Flux/Individual")
