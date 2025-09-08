### GINI - BBS
### Project: Ecology and Economics
### Lina M?hlbauer with changes from Daria Lipsky

setwd("C:/Users/aa82eluj/Desktop/BirdInequity/")

require(DescTools)
require(vegan)
require(RColorBrewer)

#### 1 Load all states and combine them ####

# #read in all the csv files in the folder "data_2024" and rbind them
# state_list <- list.files(path = "data_2024/States", pattern = "*.csv", full.names = TRUE)
# #initialize an empty list to store data frames
# data_list <- list()
# #loop through each file and read it into a data frame
# for (file in state_list) {
#   temp_data <- read.csv(file, stringsAsFactors = FALSE)
#   data_list[[length(data_list) + 1]] <- temp_data
# }
# 
# #combine all data frames into one
# States_combined_2024 <- do.call(rbind, data_list)
# write.csv(States_combined_2024, "data_2024/States_combined_2024.csv", row.names = FALSE)

#### 2 Calculate Number of species, individuals and Gini coefficient for each route ####

States_combined <- read.csv("data_2024/States_combined_2024.csv")#load route BCR?s
routes_all <- read.csv("data_2024/Routes.csv", sep=",")

#vector for Route data ID (unique ID for year/route)
routes <- unique(States_combined$RouteDataID)

elton <- read.csv("Elton1.0/BirdFuncDat.csv", header = TRUE, sep = ",") %>% 
  select(SpecID, PassNonPass, IOCOrder, BLFamilyLatin, BLFamilyEnglish, 
         BLFamSequID, Taxo, Scientific, English, Diet.Inv, Diet.Vend, 
         Diet.Vect, Diet.Vfish, Diet.Vunk, Diet.Scav, Diet.Fruit, 
         Diet.Nect, Diet.Seed, Diet.PlantO, Diet.5Cat, BodyMass.Value)

# species_List <- read.csv("data/Species_List.csv", sep=";")
species_List <- read.csv("data_2024/SpeciesList.csv", sep=",")
species_List$Genus_Species <- paste(species_List$Genus, species_List$Species)
species_List <- species_List %>%
  mutate(
    Genus_Species = case_when(
      Genus_Species == "Artemisiospiza belli" ~ "Amphispiza belli",
      Genus_Species == "Anser caerulescens (blue form)" ~ "Chen caerulescens",
      Genus_Species == "Antrostomus vociferus" ~ "Caprimulgus vociferus",
      TRUE ~ Genus_Species
    )
  )

merged <- species_List %>%
  inner_join(elton, by = c("Genus_Species" = "Scientific"))

#try with english names
merged_english <- species_List[!species_List$Genus_Species %in% elton$Scientific,] %>%
  inner_join(elton, by = c("English_Common_Name" = "English"))

#add to merged
merged <- merged %>%
  bind_rows(merged_english) %>%
  select(AOU, Diet.5Cat) %>% unique() 

rm(merged_english)

#merge with States_combined
States_combined <- States_combined %>%
  left_join(merged, by = c("AOU" = "AOU"))

#create matrix for results
results_BBS <- matrix(data=NA,nrow=length(routes), ncol=12) #nrow=number of routes
colnames(results_BBS) <- c("State", "Route", "Year", "S","N", "GINI", "BCR",
                           "GINI_Plant", "GINI_Omni", "GINI_Inv", "GINI_Vert",
                           "GINI_Fruit")

# routes <- routes %>% head()

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
      # results_BBS[run,7] <- BCR.ps$BCR #save habitat type
      results_BBS[run,8] <- Gini(as.integer(temp.data$SpeciesTotal[temp.data$Diet.5Cat=="PlantSeed"]), na.rm=TRUE)#calculate Gini Plant
      results_BBS[run,9] <- Gini(as.integer(temp.data$SpeciesTotal[temp.data$Diet.5Cat=="Omnivore"]), na.rm=TRUE)#calculate Gini Omni
      results_BBS[run,10] <- Gini(as.integer(temp.data$SpeciesTotal[temp.data$Diet.5Cat=="Invertebrate"]), na.rm=TRUE)#calculate Gini Inv
      results_BBS[run,11] <- Gini(as.integer(temp.data$SpeciesTotal[temp.data$Diet.5Cat=="Vertebrate"]), na.rm=TRUE)#calculate Gini Vert
      results_BBS[run,12] <- Gini(as.integer(temp.data$SpeciesTotal[temp.data$Diet.5Cat=="FruiNect"]), na.rm=TRUE)#calculate Gini Fruit
}
#save results
results_BBS_df <- as.data.frame(results_BBS)
colnames(results_BBS_df) <- c("State", "Route", "Year", "S","N", "GINI", "BCR")
write.csv(results_BBS_df, "data_2024/Results_BBS_2024.csv", row.names = FALSE)
