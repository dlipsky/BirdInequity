### Diet-specific metabolic rate using Elton 1.0 - BBS 
### Project: Ecology and Economics
### Daria Lipsky

# here, I’ll repeat the analysis using different groupings of forage-type birds, 
# applying specific energy equation coefficients. 

library(dplyr)
library(ggplot2)
library(stringr)

setwd("C:/Users/aa82eluj/Desktop/BirdInequity/")

#### 1 Load Elton 1.0 data ####
# elton <- read.table("Elton1.0/BirdFuncDat.txt", header = TRUE, sep = "\t")
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

#### 2 Try fixing missing species ####
#try with english names
merged_english <- species_List[!species_List$Genus_Species %in% elton$Scientific,] %>%
  inner_join(elton, by = c("English_Common_Name" = "English"))

#add to merged
merged <- merged %>%
  bind_rows(merged_english)

#not sure what to do with unidentified/hybrids
#i think it's better to drop them

# Function to average hybrids
# average_hybrid <- function(hybrid_name, elton_data) {
#   #split species names like Aythya marila / affinis into Aythya marila and Aythya affinis
# 
#   sp <- str_split(hybrid_name, " / ", simplify = TRUE)
#   sp[2] <- paste(word(sp[1], 1), sp[2], sep = " ")
#   
#   
#   trait_cols <- c("Diet.Inv", "Diet.Vend", "Diet.Vect", "Diet.Vfish", "Diet.Vunk", 
#                   "Diet.Scav", "Diet.Fruit", "Diet.Nect", "Diet.Seed", "Diet.PlantO", "BodyMass.Value")
#   
#   sub <- elton_data %>% filter(Scientific %in% sp)
#   
#   if (nrow(sub) == 0) return(NULL)
#   
#   averaged <- sub %>%
#     summarise(across(all_of(trait_cols), mean, na.rm = TRUE))
#   
#   averaged$Diet.5Cat <- sub$Diet.5Cat[1] # take first category
#   averaged$Genus_Species <- hybrid_name
#   
#   return(averaged)
# }
# 
# hybrids <- species_List %>% 
#   filter(str_detect(Genus_Species, "/"))
# hybrid_results <- bind_rows(lapply(hybrids$Genus_Species, average_hybrid, elton_data = elton))
# merged_hybrids <- hybrids %>% inner_join(hybrid_results, by = "Genus_Species")
# 
# merged <- merged %>%
#   bind_rows(merged_hybrids)

# species_not_in_merged <- species_List[!species_List$Genus_Species %in% merged$Genus_Species,]
rm(merged_english)

#### 3 Calculate trophic level ####
aou_bodymass <- merged %>%
  select(AOU, Diet.5Cat, Diet.Inv, Diet.Vend, Diet.Vect, Diet.Vfish,
         Diet.Vunk, Diet.Scav, Diet.Fruit, Diet.Nect, Diet.Seed,
         Diet.PlantO, Bodymass = BodyMass.Value) %>% unique() 

#Now get trophic level from diet for each species
# Trophic_level = Diet.Nect*0 + Diet.Fruit*0 + Diet.Seed*0 + Diet.Inv*1 + 
#   Diet.Vend*2 + Diet.Vect*2 + Diet.Vfish*2 + Diet.Vunk*2 + Diet.Scav*2 
aou_bodymass <- aou_bodymass %>%
  mutate(AOU = as.numeric(AOU),
         Bodymass = as.numeric(Bodymass),
         Trophic_level = (Diet.Nect*0 + Diet.Fruit*0 + Diet.Seed*0 + Diet.PlantO*0 + Diet.Inv*1 + 
           Diet.Vend*2 + Diet.Vect*2 + Diet.Vfish*2 + Diet.Vunk*2 + Diet.Scav*2)/100,
         E = Bodymass^0.75,
         E_diet_trophic = (1 / (0.1^Trophic_level)) * Bodymass^0.75
  )

#### 4 Load BBS data and calculate E_total per route ####
#load result data BBS and overall BBS data
results_BBS_df <- read.csv("data_2024/results_BBS_2024.csv")
States_combined <- read.csv("data_2024/States_combined_2024.csv")#load route BCR?s

# Merge States_combined with aou_bodymass to add E
combined <- States_combined %>%
  mutate(AOU = as.integer(AOU), SpeciesTotal = as.integer(SpeciesTotal)) %>%
  left_join(aou_bodymass, by = "AOU")

combined$AOU %>% unique() %>% length() # check how many AOU are in combined
#check which aou are not in aou_bodymass
# missing_aou <- setdiff(combined$AOU, aou_bodymass$AOU)

# Calculate E_total per route
E_summary <- combined %>%
  group_by(RouteDataID) %>%
  summarise(
    E_total = sum(SpeciesTotal * E, na.rm = TRUE), 
    E_total_diet = sum(SpeciesTotal * E_diet_trophic, na.rm = TRUE),
    N = sum(SpeciesTotal, na.rm = TRUE),
    S = sum(SpeciesTotal > 0, na.rm = TRUE),
    
    E_total_PlantSeed = sum(ifelse(Diet.5Cat == "PlantSeed", SpeciesTotal * E_diet_trophic, 0), na.rm = TRUE),
    E_total_Omnivore   = sum(ifelse(Diet.5Cat == "Omnivore", SpeciesTotal * E_diet_trophic, 0), na.rm = TRUE),
    E_total_Inv        = sum(ifelse(Diet.5Cat == "Invertebrate", SpeciesTotal * E_diet_trophic, 0), na.rm = TRUE),
    E_total_Vert       = sum(ifelse(Diet.5Cat == "VertFishScav", SpeciesTotal * E_diet_trophic, 0), na.rm = TRUE),
    E_total_FruiNec    = sum(ifelse(Diet.5Cat == "FruiNect", SpeciesTotal * E_diet_trophic, 0), na.rm = TRUE),
    
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
    
    Sum_bodymass_PlantSeed = sum(ifelse(Diet.5Cat == "PlantSeed", Bodymass*SpeciesTotal, NA), na.rm = TRUE),
    Sum_bodymass_Omnivore   = sum(ifelse(Diet.5Cat == "Omnivore", Bodymass*SpeciesTotal, NA), na.rm = TRUE),
    Sum_bodymass_Inv        = sum(ifelse(Diet.5Cat == "Invertebrate", Bodymass*SpeciesTotal, NA), na.rm = TRUE),
    Sum_bodymass_Vert       = sum(ifelse(Diet.5Cat == "VertFishScav", Bodymass*SpeciesTotal, NA), na.rm = TRUE),
    Sum_bodymass_FruiNec    = sum(ifelse(Diet.5Cat == "FruiNect", Bodymass*SpeciesTotal, NA), na.rm = TRUE),
    
    Sum_trophiclevel = sum(Trophic_level*SpeciesTotal, na.rm=TRUE),
    Sum_bodymass = sum(Bodymass*SpeciesTotal, na.rm=TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Ave_trophiclevel = Sum_trophiclevel / N,
    Ave_bodymass = Sum_bodymass / N,
    
    Ave_bodymass_PlantSeed = ifelse(N_Plantseed > 0, Sum_bodymass_PlantSeed / N_Plantseed, NA),
    Ave_bodymass_Omnivore   = ifelse(N_Omnivore > 0, Sum_bodymass_Omnivore / N_Omnivore, NA),
    Ave_bodymass_Inv        = ifelse(N_Inv > 0, Sum_bodymass_Inv / N_Inv, NA),
    Ave_bodymass_Vert       = ifelse(N_Vert > 0, Sum_bodymass_Vert / N_Vert, NA),
    Ave_bodymass_FruiNec    = ifelse(N_FruiNec > 0, Sum_bodymass_FruiNec / N_FruiNec, NA)
    
  ) %>%
  select(-Sum_trophiclevel, -Sum_bodymass, 
         -Sum_bodymass_PlantSeed, -Sum_bodymass_Omnivore, -Sum_bodymass_Inv, 
         -Sum_bodymass_Vert, -Sum_bodymass_FruiNec, -S, -N)

results_BBS_df <- results_BBS_df %>%
  left_join(E_summary, by = "RouteDataID")

write.csv(results_BBS_df, "data_2024/Results_BBS_complete_diet.csv", row.names = FALSE)

