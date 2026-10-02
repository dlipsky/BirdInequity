### ============================================================================
### Gini Coefficients for North American Breeding Bird Survey (BBS) Routes
### Authors : Lina Muehlbauer & Daria Lipsky
### Purpose : Calculate Gini coefficients and metabolic-rate summaries per
###           route, integrating EltonTraits functional-group information.
### ============================================================================


# ------------------------------------------------------------------------------
# 1. Setup
# ------------------------------------------------------------------------------

setwd("...")
library(dplyr)
library(stringr)
library(DescTools)

# ------------------------------------------------------------------------------
# 2. Load data
# ------------------------------------------------------------------------------

# Uncomment the block below to rebuild States_combined_2025.csv from raw files.
# state_files       <- list.files("data_2025/States", pattern = "*.csv", full.names = TRUE)
# States_combined   <- do.call(rbind, lapply(state_files, read.csv, stringsAsFactors = FALSE))
# write.csv(States_combined, "data_2025/States_combined_2025.csv", row.names = FALSE)

States_combined <- read.csv("data_2025/States_combined_2025.csv")
routes_all      <- read.csv("data_2025/Routes.csv")
species_list    <- read.csv("data_2025/SpeciesList.csv")
elton           <- read.csv("Elton1.0/BirdFuncDat.csv")


# ------------------------------------------------------------------------------
# 3. Clean species list
# ------------------------------------------------------------------------------

species_list <- species_list %>%
  mutate(
    Genus_Species = paste(Genus, Species),
    # Harmonise a small number of outdated scientific names
    Genus_Species = case_when(
      Genus_Species == "Artemisiospiza belli"              ~ "Amphispiza belli",
      Genus_Species == "Anser caerulescens (blue form)"   ~ "Chen caerulescens",
      Genus_Species == "Antrostomus vociferus"             ~ "Caprimulgus vociferus",
      TRUE                                                 ~ Genus_Species
    )
  ) %>%
  filter(
    !str_detect(Genus_Species,       "/|\\(| sp\\.|\\S+.*\\s\\S+.*\\s"),  # remove hybrids, groups, sp.
    !str_detect(English_Common_Name, fixed("unid.")),
    !str_detect(English_Common_Name, " x ")
  ) %>%
  distinct()


# ------------------------------------------------------------------------------
# 4. Match EltonTraits to species
# ------------------------------------------------------------------------------

# Primary join on scientific name
species_elton <- species_list %>%
  inner_join(elton, by = c("Genus_Species" = "Scientific")) %>%
  distinct()

# Secondary join on English name for species not matched above
species_elton_english <- species_list %>%
  filter(!Genus_Species %in% elton$Scientific) %>%
  inner_join(elton, by = c("English_Common_Name" = "English")) %>%
  distinct()

species_elton <- bind_rows(species_elton, species_elton_english) %>%
  distinct()

rm(species_elton_english)


# ------------------------------------------------------------------------------
# 5. Derive trophic level and metabolic-rate proxies
# ------------------------------------------------------------------------------
# Trophic level: weighted average across diet categories (0 = plant, 2 = vertebrate).
# Metabolic proxy E          = M^0.75 (Kleiber scaling; Brown et al. 2004).
# Diet-adjusted proxy E_diet = (1 / 0.1^TL) * M^0.75, reflecting trophic
# energy requirements relative to primary production, where 0.1 is the assumed
# trophic transfer efficiency (Lindeman 1942).

species_elton <- species_elton %>%
  select(
    AOU,
    Diet.5Cat,
    Diet.Inv, Diet.Vend, Diet.Vect, Diet.Vfish, Diet.Vunk, Diet.Scav,
    Diet.Fruit, Diet.Nect, Diet.Seed, Diet.PlantO,
    Bodymass = BodyMass.Value
  ) %>%
  distinct() %>%
  mutate(
    AOU      = as.numeric(AOU),
    Bodymass = as.numeric(Bodymass),
    Trophic_level = (
      Diet.Nect * 0 + Diet.Fruit * 0 + Diet.Seed * 0 + Diet.PlantO * 0 +
        Diet.Inv  * 1 +
        Diet.Vend * 2 + Diet.Vect  * 2 + Diet.Vfish * 2 +
        Diet.Vunk * 2 + Diet.Scav  * 2
    ) / 100,
    E              = Bodymass ^ 0.75,
    E_diet_trophic = (1 / (0.1 ^ Trophic_level)) * Bodymass ^ 0.75
  )

# ------------------------------------------------------------------------------
# 6. Merge traits into BBS dataset and save
# ------------------------------------------------------------------------------

States_combined <- States_combined %>%
  filter(AOU %in% species_list$AOU) %>%
  left_join(species_elton, by = "AOU")

write.csv(States_combined, "data_2025/States_combined_2025_Elton.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 7. Calculate route-level Gini coefficients
# ------------------------------------------------------------------------------

routes <- unique(States_combined$RouteDataID)

results <- matrix(
  NA,
  nrow = length(routes),
  ncol = 12,
  dimnames = list(NULL, c(
    "RouteDataID", "State", "Route", "Year", "S", "N", "GINI",
    "GINI_PlantSeed", "GINI_Omnivore", "GINI_Invertebrate",
    "GINI_VertFishScav", "GINI_FruiNect"
  ))
)
 
for (i in seq_along(routes)) {
 
  route_data <- States_combined[States_combined$RouteDataID == routes[i], ]
 
  state_id <- as.integer(unique(route_data$State))
  route_id <- as.integer(unique(route_data$Route))
  counts   <- as.integer(route_data$SpeciesTotal)
  diet     <- route_data$Diet.5Cat
 
  results[i, ] <- c(
    routes[i],
    state_id,
    route_id,
    unique(route_data$Year),
    length(counts),                                              # S: species richness
    sum(counts),                                                 # N: total individuals
    Gini(counts),                                                # overall Gini
    Gini(counts[diet == "PlantSeed"],    na.rm = TRUE),
    Gini(counts[diet == "Omnivore"],     na.rm = TRUE),
    Gini(counts[diet == "Invertebrate"], na.rm = TRUE),
    Gini(counts[diet == "VertFishScav"], na.rm = TRUE),
    Gini(counts[diet == "FruiNect"],     na.rm = TRUE)
  )
}
 
results_df <- as.data.frame(results)

# ------------------------------------------------------------------------------
# 8. Save route-level outputs
# ------------------------------------------------------------------------------

write.csv(
  results_df[, c("State", "Route", "Year", "S", "N", "GINI")],
  "data_2025/RouteMetrics_2025.csv",
  row.names = FALSE
)

write.csv(
  results_df,
  "data_2025/RouteMetrics_2025_Elton.csv",
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 9. Temporal coverage: first year with >= 2 000 unique routes
# ------------------------------------------------------------------------------

route_counts <- results_df %>%
  group_by(Year) %>%
  summarise(n_routes = n_distinct(paste(State, Route)), .groups = "drop")

first_year_2000_routes <- route_counts %>%
  filter(n_routes > 2000) %>%
  slice(1) %>%
  pull(Year)

print("First year with > 2,000 surveyed routes: ", first_year_2000_routes)

# ------------------------------------------------------------------------------
# 10. Metabolic-rate summaries per route
# ------------------------------------------------------------------------------
# Uncomment the two lines below to reload from disk if re-running from this point.
# results_df      <- read.csv("data_2025/RouteMetrics_2025_Elton.csv")
# States_combined <- read.csv("data_2025/States_combined_2025_Elton.csv")

diet_groups <- c("PlantSeed", "Omnivore", "Invertebrate", "VertFishScav", "FruiNect")

E_summary <- States_combined %>%
  group_by(RouteDataID) %>%
  summarise(
    # Community totals
    E_total      = sum(SpeciesTotal * E,              na.rm = TRUE),
    E_total_diet = sum(SpeciesTotal * E_diet_trophic, na.rm = TRUE),
    N            = sum(SpeciesTotal,                  na.rm = TRUE),
    S            = sum(SpeciesTotal > 0,              na.rm = TRUE),
 
    # Diet-group energy totals
    E_total_PlantSeed = sum(ifelse(Diet.5Cat == "PlantSeed",    SpeciesTotal * E_diet_trophic, 0), na.rm = TRUE),
    E_total_Omnivore  = sum(ifelse(Diet.5Cat == "Omnivore",     SpeciesTotal * E_diet_trophic, 0), na.rm = TRUE),
    E_total_Inv       = sum(ifelse(Diet.5Cat == "Invertebrate", SpeciesTotal * E_diet_trophic, 0), na.rm = TRUE),
    E_total_Vert      = sum(ifelse(Diet.5Cat == "VertFishScav", SpeciesTotal * E_diet_trophic, 0), na.rm = TRUE),
    E_total_FruiNect  = sum(ifelse(Diet.5Cat == "FruiNect",     SpeciesTotal * E_diet_trophic, 0), na.rm = TRUE),
 
    # Diet-group abundance totals
    N_PlantSeed = sum(ifelse(Diet.5Cat == "PlantSeed",    SpeciesTotal, 0), na.rm = TRUE),
    N_Omnivore  = sum(ifelse(Diet.5Cat == "Omnivore",     SpeciesTotal, 0), na.rm = TRUE),
    N_Inv       = sum(ifelse(Diet.5Cat == "Invertebrate", SpeciesTotal, 0), na.rm = TRUE),
    N_Vert      = sum(ifelse(Diet.5Cat == "VertFishScav", SpeciesTotal, 0), na.rm = TRUE),
    N_FruiNect  = sum(ifelse(Diet.5Cat == "FruiNect",     SpeciesTotal, 0), na.rm = TRUE),
 
    # Diet-group species richness
    S_PlantSeed = sum(ifelse(Diet.5Cat == "PlantSeed",    SpeciesTotal, 0) > 0, na.rm = TRUE),
    S_Omnivore  = sum(ifelse(Diet.5Cat == "Omnivore",     SpeciesTotal, 0) > 0, na.rm = TRUE),
    S_Inv       = sum(ifelse(Diet.5Cat == "Invertebrate", SpeciesTotal, 0) > 0, na.rm = TRUE),
    S_Vert      = sum(ifelse(Diet.5Cat == "VertFishScav", SpeciesTotal, 0) > 0, na.rm = TRUE),
    S_FruiNect  = sum(ifelse(Diet.5Cat == "FruiNect",     SpeciesTotal, 0) > 0, na.rm = TRUE),
 
    # Diet-group body-mass sums (for average calculation below)
    sum_bm_PlantSeed = sum(ifelse(Diet.5Cat == "PlantSeed",    Bodymass * SpeciesTotal, NA), na.rm = TRUE),
    sum_bm_Omnivore  = sum(ifelse(Diet.5Cat == "Omnivore",     Bodymass * SpeciesTotal, NA), na.rm = TRUE),
    sum_bm_Inv       = sum(ifelse(Diet.5Cat == "Invertebrate", Bodymass * SpeciesTotal, NA), na.rm = TRUE),
    sum_bm_Vert      = sum(ifelse(Diet.5Cat == "VertFishScav", Bodymass * SpeciesTotal, NA), na.rm = TRUE),
    sum_bm_FruiNect  = sum(ifelse(Diet.5Cat == "FruiNect",     Bodymass * SpeciesTotal, NA), na.rm = TRUE),
 
    # Community-level body-mass and trophic-level sums
    sum_trophic  = sum(Trophic_level * SpeciesTotal, na.rm = TRUE),
    sum_bodymass = sum(Bodymass      * SpeciesTotal, na.rm = TRUE),
 
    .groups = "drop"
  ) %>%
  mutate(
    # Community averages (abundance-weighted)
    Ave_trophic_level = sum_trophic  / N,
    Ave_bodymass      = sum_bodymass / N,
 
    # Diet-group average body masses (NA when group absent)
    Ave_bodymass_PlantSeed = ifelse(N_PlantSeed > 0, sum_bm_PlantSeed / N_PlantSeed, NA),
    Ave_bodymass_Omnivore  = ifelse(N_Omnivore  > 0, sum_bm_Omnivore  / N_Omnivore,  NA),
    Ave_bodymass_Inv       = ifelse(N_Inv       > 0, sum_bm_Inv       / N_Inv,       NA),
    Ave_bodymass_Vert      = ifelse(N_Vert      > 0, sum_bm_Vert      / N_Vert,      NA),
    Ave_bodymass_FruiNect  = ifelse(N_FruiNect  > 0, sum_bm_FruiNect  / N_FruiNect,  NA)
  ) %>%
  # Drop intermediate sums already captured in results_df
  select(-starts_with("sum_"), -S, -N)

# ------------------------------------------------------------------------------
# 11. Merge and save final dataset
# ------------------------------------------------------------------------------


States_combined <- States_combined %>%
  mutate(Route_unique = paste(CountryNum, StateNum, Route, RPID, sep = "_"))

# Deduplicate to one row per RouteDataID before joining, to avoid creating
# duplicate rows (States_combined has one row per species, not per route).
route_lookup <- States_combined %>%
  select(Route_unique, RouteDataID) %>%
  distinct(RouteDataID, .keep_all = TRUE)

results_df <- results_df %>%
  left_join(E_summary,    by = "RouteDataID") %>%
  left_join(route_lookup, by = "RouteDataID")

write.csv(results_df, "data_2025/RouteMetrics_2025_diet.csv", row.names = FALSE)

print("Rows: ", nrow(results_df), " | Columns: ", ncol(results_df))