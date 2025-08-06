### Diet-specific metabolic rate using Elton 1.0 - BBS 
### Project: Ecology and Economics
### Daria Lipsky

# here, I’ll repeat the analysis using different groupings of forage-type birds, 
# applying specific energy equation coefficients. 

library(dplyr)
library(ggplot2)

setwd("C:/Users/aa82eluj/Desktop/BirdInequity/")

# elton <- read.table("Elton1.0/BirdFuncDat.txt", header = TRUE, sep = "\t")
elton <- read.csv("Elton1.0/BirdFuncDat.csv", header = TRUE, sep = ",")

species_List <- read.csv("data/Species_List.csv", sep=";")
species_List$Genus_Species <- paste(species_List$Genus, species_List$Species)


merged <- species_List %>%
  inner_join(elton, by = c("Genus_Species" = "Scientific"))

#get all species that are not in elton
species_not_in_elton <- species_List$Genus_Species[!species_List$Genus_Species %in% elton$Scientific]

aou_bodymass <- merged %>%
  select(AOU, Diet.5Cat, Diet.Inv, Diet.Vend, Diet.Vect, Diet.Vfish,
         Diet.Vunk, Diet.Scav, Diet.Fruit, Diet.Nect, Diet.Seed,
         Diet.PlantO, Bodymass = BodyMass.Value) %>% unique()

aou_bodymass <- aou_bodymass %>%
  mutate(AOU = as.numeric(AOU),
    Bodymass = as.numeric(Bodymass),
    Diet.Vert = Diet.Vend+ Diet.Vfish+ Diet.Vect+ Diet.Vunk,
    Ass_eff = 0.9*Diet.Vert + 0.8*Diet.Inv  + 0.7*Diet.Fruit + 0.7*Diet.Nect +
      0.7*Diet.Scav + 0.5*Diet.Seed + 0.3*Diet.PlantO,
    E = Bodymass^0.75,
    E_diet = Bodymass^0.75/Ass_eff
  )

#load result data BBS and overall BBS data
results_BBS_df <- read.csv("data_2024/results_BBS_2024.csv")
States_combined <- read.csv("data_2024/States_combined_2024.csv")#load route BCR?s

# Merge States_combined with aou_bodymass to add E
combined <- States_combined %>%
  mutate(AOU = as.integer(AOU), SpeciesTotal = as.integer(SpeciesTotal)) %>%
  left_join(aou_bodymass, by = "AOU")

# Calculate E_total per route
E_summary <- combined %>%
  group_by(RouteDataID) %>%
  summarise(
    E_total = sum(SpeciesTotal * E, na.rm = TRUE), 
    E_total_diet = sum(SpeciesTotal * E_diet, na.rm = TRUE),
    
    E_total_PlantSeed = sum(ifelse(Diet.5Cat == "PlantSeed", SpeciesTotal * E, 0), na.rm = TRUE),
    E_total_Omnivore   = sum(ifelse(Diet.5Cat == "Omnivore", SpeciesTotal * E, 0), na.rm = TRUE),
    E_total_Inv        = sum(ifelse(Diet.5Cat == "Invertebrate", SpeciesTotal * E, 0), na.rm = TRUE),
    E_total_Vert       = sum(ifelse(Diet.5Cat == "VertFishScav", SpeciesTotal * E, 0), na.rm = TRUE),
    E_total_FruiNec    = sum(ifelse(Diet.5Cat == "FruiNect", SpeciesTotal * E, 0), na.rm = TRUE),
    
    N_Plantseed = sum(ifelse(Diet.5Cat == "PlantSeed", SpeciesTotal, 0), na.rm = TRUE),
    N_Omnivore   = sum(ifelse(Diet.5Cat == "Omnivore", SpeciesTotal, 0), na.rm = TRUE),
    N_Inv        = sum(ifelse(Diet.5Cat == "Invertebrate", SpeciesTotal, 0), na.rm = TRUE),
    N_Vert       = sum(ifelse(Diet.5Cat == "VertFishScav", SpeciesTotal, 0), na.rm = TRUE),
    N_FruiNec    = sum(ifelse(Diet.5Cat == "FruiNect", SpeciesTotal, 0), na.rm = TRUE),
    
    S_PlantSeed = sum(ifelse(Diet.5Cat == "PlantSeed", SpeciesTotal, 0) > 0, na.rm = TRUE),
    S_Omnivore   = sum(ifelse(Diet.5Cat == "Omnivore", SpeciesTotal, 0) > 0, na.rm = TRUE),
    S_Inv        = sum(ifelse(Diet.5Cat == "Invertebrate", SpeciesTotal, 0) > 0, na.rm = TRUE),
    S_Vert       = sum(ifelse(Diet.5Cat == "VertFishScav", SpeciesTotal, 0) > 0, na.rm = TRUE),
    S_FruiNec    = sum(ifelse(Diet.5Cat == "FruiNect", SpeciesTotal, 0) > 0, na.rm = TRUE),
    
    .groups = "drop"
  )


results_BBS_df <- results_BBS_df %>%
  left_join(E_summary, by = "RouteDataID")

write.csv(results_BBS_df, "data_2024/Results_BBS_complete_diet.csv", row.names = FALSE)
