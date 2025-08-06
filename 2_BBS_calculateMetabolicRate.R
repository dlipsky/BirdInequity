### GINI and Metabolic rate - BBS 
### Project: Ecology and Economics
### Lina M?hlbauer with changes from Daria Lipsky

require(DescTools)
require(vegan)
library(dplyr)
library(stringdist)  # for better fuzzy matching of species names

#### 1 Load bodymass data and prepare ####
#load species list and Bodymass data
bodymass <- read.delim("data/North American birds_Bodymass.txt")
species_List <- read.csv("data/Species_List.csv", sep=";")

# 1. Create Genus_Species name in both tables
species_List$Genus_Species <- paste(species_List$Genus, species_List$Species)

bodymass <- bodymass %>%
  mutate(Genus_Species = Species)  # Assuming 'Species' is already 'Genus species' in bodymass

# 2. First attempt: join by Genus_Species
merged <- species_List %>%
  left_join(bodymass %>% select(Genus_Species, Body.mass..g.), by = "Genus_Species")

# 3. For missing bodymass, try to fill in by common name
merged <- merged %>%
  left_join(
    bodymass %>% select(Common.name, Body.mass..g.) %>%
      rename(English_Common_Name = Common.name,
             Fallback_Bodymass = Body.mass..g.),
    by = "English_Common_Name"
  ) %>%
  mutate(Final_Bodymass = ifelse(is.na(Body.mass..g.), Fallback_Bodymass, Body.mass..g.))

# 4. Create final result: AOU + Bodymass
aou_bodymass <- merged %>%
  select(AOU, Bodymass = Final_Bodymass) %>% unique()

rm(merged)

#### 2 Missing species values: Automatic version ####

# normalize species names
clean_name <- function(x) {
  x <- tolower(x)
  x <- gsub("\\(.*\\)", "", x)     # remove anything in parentheses
  x <- gsub(" x |/", " ", x)       # replace hybrids symbols with space
  x <- gsub("[^a-z ]", "", x)      # remove special characters
  x <- trimws(x)
  return(x)
}

missing.species <- species_List$Genus_Species[which(is.na(aou_bodymass[,2]))]
length(missing.species)

clean_missing <- clean_name(missing.species)
clean_bodymass <- clean_name(bodymass$Species)

# initialize result
unknown_bodymass_2 <- data.frame(
  Genus_Species = missing.species,
  Matched_Species = NA,
  Bodymass = NA
)

# try automatic matching
for (i in seq_along(clean_missing)) {
  distances <- stringdist(clean_missing[i], clean_bodymass, method = "jw") # Jaro-Winkler
  best_match_idx <- which.min(distances)
  
  if (distances[best_match_idx] < 0.15) {  # adjust threshold if needed
    unknown_bodymass_2$Matched_Species[i] <- bodymass$Species[best_match_idx]
    unknown_bodymass_2$Bodymass[i] <- bodymass$Body.mass..g.[best_match_idx]
  }
}



#### 3 Missing species values: Lina's manual version ####

#check for missing values 
#bodymass data.frame contains real duplicates!
missing.species <- species_List$Genus_Species[which(is.na(aou_bodymass[,2]))]
length(missing.species)

#158 species where still body mass missing - check for hybrids?
check <- "/"
check2 <- " x "

a <- which(grepl(check2, missing.species)) 
b <- which(grepl(check, missing.species))
hybrids <- missing.species[sort(c(a, b))]

other <- missing.species[which(!(missing.species%in%hybrids))]

unknown_bodymass <- matrix(NA, nrow=length(missing.species), ncol =2)
unknown_bodymass[,1] <- missing.species

#add missing species bodymass manually
missing.species[1] #"Anser caerulescens"
b1 <-bodymass[bodymass$Species=="Chen caerulescens",]
unknown_bodymass[1,2] <- b1$Body.mass..g.

missing.species[2] #"Anser caerulescens (blue form)"
unknown_bodymass[2,2] <- b1$Body.mass..g.

missing.species[3] #"Anser rossii"
b3 <- bodymass[bodymass$Species=="Chen rossii",]
unknown_bodymass[3,2] <- b3$Body.mass..g.
#species_List[species_List$Genus_Species=="Anser rossii",]

missing.species[4] #"Branta bernicla nigricans"
b4 <-bodymass[bodymass$Common.name=="Brant",]
unknown_bodymass[4,2] <- b4$Body.mass..g.

missing.species[5]
#weight of canada goose
b5 <- bodymass[bodymass$Species=="Branta canadensis",]
unknown_bodymass[5,2] <- b5$Body.mass..g.

missing.species[6]
bodymass[bodymass$Common.name=="Egyptian Goose",]
#avibase:1-4 kg 
b6 <- 2500
unknown_bodymass[6,2] <- b6

missing.species[7]
species_List[species_List$Genus_Species==missing.species[7],]
#avibase: 2.7 to 6.8 kg  = 4750g
b7 <- 4750
unknown_bodymass[7,2] <- b7

missing.species[8]
b8 <- bodymass[bodymass$Species=="Anas platyrhynchos",] #same weight
unknown_bodymass[8,2] <- b8$Body.mass..g.

missing.species[9]
b9 <- bodymass[bodymass$Species=="Anas fulvigula",] #same weight
unknown_bodymass[9,2] <- b9$Body.mass..g.

missing.species[10]
#avibase: average male 889.6 g 	female 768.3 g 
b10 <- (889.6 + 768.3)/2
unknown_bodymass[10,2] <- b9$Body.mass..g.

missing.species[11]
b11a <- bodymass[bodymass$Species=="Aythya affinis",]
b11b <- bodymass[bodymass$Species=="Aythya marila",] #average
unknown_bodymass[11,2] <- (b11a$Body.mass..g. + b11b$Body.mass..g.)/2

missing.species[12]
unknown_bodymass[12,2] <- NA

missing.species[13]
b13a <- bodymass[bodymass$Species=="Bucephala clangula",]
b13b <- bodymass[bodymass$Species=="Bucephala islandica",]
unknown_bodymass[13,2] <- (b13a$Body.mass..g. + b13b$Body.mass..g.)/2

missing.species[14]
#450-800g
unknown_bodymass[14,2] <- 450 + ((800-450)/2)

missing.species[15]
#385-500
unknown_bodymass[15,2] <- 385 + ((500-385)/2)

missing.species[16]
#male 1.2 kg female 0.9
unknown_bodymass[16,2] <- (900+1200)/2

missing.species[17]
#2.75 to 6 kg
unknown_bodymass[17,2] <- 2750 + ((6000-2750)/2)

missing.species[18]
#990  1327 g
unknown_bodymass[18,2] <- 990 + ((1327-990)/2)

missing.species[19]
# formerly one species with Dendragapus obscurus
b19 <- bodymass[bodymass$Species=="Dendragapus obscurus",]
unknown_bodymass[19,2] <- b19$Body.mass..g.

missing.species[20]
# hybrid with Dendragapus obscurus
b20 <- bodymass[bodymass$Species=="Dendragapus obscurus",]
unknown_bodymass[20,2] <- b20$Body.mass..g.

missing.species[21]
#hybrid
b21a <- bodymass[bodymass$Species=="Aechmophorus occidentalis",]
b21b <- bodymass[bodymass$Species=="Aechmophorus clarkii",]
unknown_bodymass[21,2] <- (b21a$Body.mass..g.+b21b$Body.mass..g.)/2

missing.species[22]
#238-380 g
unknown_bodymass[22,2] <- 238 + ((380-238)/2)

missing.species[23]
#165-274g
unknown_bodymass[23,2] <- 165 + ((274-165)/2)

missing.species[24]
#125-240 g
unknown_bodymass[24,2] <- 125 + ((240-125)/2)

missing.species[25]
#47g
unknown_bodymass[25,2] <- 125 + ((240-125)/2)

missing.species[26]
b26a <- bodymass[bodymass$Species=="Coccyzus americanus",]
b26b <- bodymass[bodymass$Species=="Coccyzus erythropthalmus",]
unknown_bodymass[26,2] <- (b26a$Body.mass..g.+b26b$Body.mass..g.)/2

missing.species[27]
unknown_bodymass[27,2] <- NA

missing.species[28]
b28a <- bodymass[bodymass$Species=="Chordeiles minor",]
b28b <- bodymass[bodymass$Species=="Chordeiles acutipennis",]
unknown_bodymass[28,2] <- (b28a$Body.mass..g.+b28b$Body.mass..g.)/2

missing.species[29]
unknown_bodymass[29,2] <- (b28a$Body.mass..g.+b28b$Body.mass..g.)/2

missing.species[30]
#42-69
b30 <- bodymass[bodymass$Species=="Caprimulgus vociferus",]
unknown_bodymass[30,2] <- b30$Body.mass..g.

missing.species[31]
#same size as Caprimulgus vociferus
unknown_bodymass[31,2] <- b30$Body.mass..g.

missing.species[32]
b32a <- bodymass[bodymass$Species=="Archilochus colubris",]
b32b <- bodymass[bodymass$Species=="Archilochus alexandri",]
unknown_bodymass[32,2] <- (b32a$Body.mass..g.+b32b$Body.mass..g.)/2

missing.species[33]
b33a <- bodymass[bodymass$Species=="Selasphorus rufus",]
b33b <- bodymass[bodymass$Species=="Selasphorus sasin",]
unknown_bodymass[33,2] <- (b33a$Body.mass..g.+b33b$Body.mass..g.)/2

missing.species[34]
unknown_bodymass[34,2] <- NA

missing.species[35]
b35 <- bodymass[bodymass$Species=="Rallus longirostris",]
unknown_bodymass[35,2] <- b35$Body.mass..g.[1]

missing.species[36]
#elegans <- bigger than crepitans
b36 <- bodymass[bodymass$Species=="Rallus elegans",]
unknown_bodymass[36,2] <-(b35$Body.mass..g.[1]+b36$Body.mass..g.)/2

missing.species[37]
#725-870
unknown_bodymass[37,2] <- 725 + ((870-725)/2)

missing.species[38]
#distiguished from common moorhen since 2011
b38 <- bodymass[bodymass$Species=="Gallinula chloropus",]
unknown_bodymass[38,2] <- b38$Body.mass..g.

missing.species[39]
b39 <- bodymass[bodymass$Species=="Pluvialis dominica",]
unknown_bodymass[39,2] <- b39$Body.mass..g.

missing.species[40]
unknown_bodymass[40,2] <- 180 

missing.species[41]
#39-114g
unknown_bodymass[41,2] <- 39 + ((114-39)/2)

missing.species[42]
unknown_bodymass[42,2] <- NA

missing.species[43]
b43a <- bodymass[bodymass$Species=="Limnodromus griseus",]
b43b <- bodymass[bodymass$Species=="Limnodromus scolopaceus",]
unknown_bodymass[43,2] <- (b43a$Body.mass..g.+b43b$Body.mass..g.)/2

missing.species[44]
#former subspecies of Gallinago gallinago
b44 <- bodymass[bodymass$Species=="Gallinago gallinago",]
unknown_bodymass[44,2] <- b44$Body.mass..g.

missing.species[45]
#50g
unknown_bodymass[44,2] <- 50

missing.species[46]
b46a <- bodymass[bodymass$Species=="Tringa melanoleuca",]
b46b <- bodymass[bodymass$Species=="Tringa flavipes",]
unknown_bodymass[46,2] <- (b46a$Body.mass..g.+b46b$Body.mass..g.)/2

missing.species[47]
#140-270g
unknown_bodymass[47,2] <- 140 + ((270-140)/2)

missing.species[48]
b48a <- bodymass[bodymass$Species=="Uria aalge",]
b48b <- bodymass[bodymass$Species=="Uria lomvia",]
unknown_bodymass[48,2] <- (b48a$Body.mass..g.+b48b$Body.mass..g.)/2

missing.species[49]
#140-250 g
unknown_bodymass[49,2] <- 140 + ((250-140)/2)

missing.species[50]
#1-1.7kg
unknown_bodymass[50,2] <- 1000 + ((1700-1000)/2)

missing.species[51]
b51a <- bodymass[bodymass$Species=="Larus occidentalis",]
b51b <- bodymass[bodymass$Species=="Larus glaucescens",]
unknown_bodymass[51,2] <- (b51a$Body.mass..g.+b51b$Body.mass..g.)/2

missing.species[52]
unknown_bodymass[52,2] <-NA

missing.species[53]
unknown_bodymass[53,2] <-NA

missing.species[54]
b54 <- bodymass[bodymass$Species=="Gavia pacifica",]
unknown_bodymass[54,2] <- b54$Body.mass..g.

missing.species[55]
unknown_bodymass[54,2] <-NA

missing.species[56]
unknown_bodymass[56,2] <-NA

missing.species[57]
unknown_bodymass[57,2] <- 1500

missing.species[58]
b58a <- bodymass[bodymass$Species=="Phalacrocorax brasilianus",]
b58b <- bodymass[bodymass$Species=="Phalacrocorax auritus",]
unknown_bodymass[58,2] <- (b58a$Body.mass..g.+b58b$Body.mass..g.)/2

missing.species[59]
b59a <- bodymass[bodymass$Species=="Phalacrocorax carbo",]
b59b <- bodymass[bodymass$Species=="Phalacrocorax auritus",]
unknown_bodymass[59,2] <- (b59a$Body.mass..g.+b59b$Body.mass..g.)/2

missing.species[60]
unknown_bodymass[60,2] <-NA

missing.species[61]
b61 <- bodymass[bodymass$Species=="Ardea herodias",]
unknown_bodymass[61,2] <- b61$Body.mass..g.

missing.species[62]
#350-550
unknown_bodymass[62,2] <- 350 + ((550-350)/2)

missing.species[63]
b63 <- bodymass[bodymass$Species=="Nyctanassa violacea",]
unknown_bodymass[63,2] <- b63$Body.mass..g.

missing.species[64]

missing.species[65]
b65a <- bodymass[bodymass$Species=="Plegadis falcinellus",]
b65b <- bodymass[bodymass$Species=="Plegadis chihi",]
unknown_bodymass[65,2] <- (b65a$Body.mass..g.+b65b$Body.mass..g.)/2

missing.species[66]
b66 <- bodymass[bodymass$Species=="Coragyps atratus",]
unknown_bodymass[66,2] <- b66$Body.mass..g.

missing.species[67]

missing.species[68]
b68 <- bodymass[bodymass$Species=="Buteo jamaicensis",]
unknown_bodymass[68,2] <- b68$Body.mass..g.

missing.species[69]
missing.species[70]
missing.species[71]
b71 <- bodymass[bodymass$Species=="Picoides tridactylus",]
unknown_bodymass[71,2] <- b71$Body.mass..g.

missing.species[72] 
b72 <- bodymass[bodymass$Species=="Colaptes auratus",]
unknown_bodymass[72,2] <- b72$Body.mass..g.

missing.species[73]
b73 <- bodymass[bodymass$Species=="Colaptes auratus",]
unknown_bodymass[73,2] <- b73$Body.mass..g.

missing.species[74]
b74 <- bodymass[bodymass$Species=="Colaptes auratus",]
unknown_bodymass[74,2] <- b74$Body.mass..g.

missing.species[75]
missing.species[76]
#208-305 g
unknown_bodymass[76,2] <- 208 + ((305-208)/2)

missing.species[77]
#100g
unknown_bodymass[77,2] <- 100

missing.species[78]
#140 g
unknown_bodymass[78,2] <- 140

missing.species[79]
missing.species[80]

missing.species[81]
#270g
unknown_bodymass[81,2] <- 270

missing.species[82]
missing.species[83]
unknown_bodymass[83,2] <- 35

missing.species[84]
b84a <- bodymass[bodymass$Species=="Tyrannus melancholicus",]
b84b <- bodymass[bodymass$Species=="Tyrannus couchii",]
unknown_bodymass[84,2] <- (b84a$Body.mass..g.+b84b$Body.mass..g.)/2

missing.species[85]
b85a <- bodymass[bodymass$Species=="Tyrannus vociferans",]
b85b <- bodymass[bodymass$Species=="Tyrannus verticalis",]
unknown_bodymass[85,2] <- (b85a$Body.mass..g.+b85b$Body.mass..g.)/2

missing.species[86]
unknown_bodymass[86,2] <- 30

missing.species[87]
b87a <- bodymass[bodymass$Species=="Empidonax alnorum",]
b87b <- bodymass[bodymass$Species=="Empidonax traillii",]
unknown_bodymass[87,2] <- (b87a$Body.mass..g.+b87b$Body.mass..g.[1])/2

missing.species[88]
b88a <- bodymass[bodymass$Species=="Empidonax hammondii",]
b88b <- bodymass[bodymass$Species=="Empidonax oberholseri",]
unknown_bodymass[88,2] <- (b88a$Body.mass..g.+b88b$Body.mass..g.)/2

missing.species[89]
b89b <- bodymass[bodymass$Species=="Empidonax difficilis",]
unknown_bodymass[89,2] <- b89b$Body.mass..g.

missing.species[90]
unknown_bodymass[90,2] <- b89b$Body.mass..g.

missing.species[91]
missing.species[92]
#species complex Vireo solitarius
b92 <- bodymass[bodymass$Species=="Vireo solitarius",]
unknown_bodymass[92,2] <- b92$Body.mass..g.

missing.species[93]
unknown_bodymass[93,2] <- b92$Body.mass..g.

missing.species[94]
unknown_bodymass[94,2] <- b92$Body.mass..g.

missing.species[95]
unknown_bodymass[95,2] <- b92$Body.mass..g.

missing.species[96]
unknown_bodymass[96,2] <- 120

missing.species[97]
b97 <- bodymass[bodymass$Species=="Aphelocoma californica",]
unknown_bodymass[97,2] <- b97$Body.mass..g.

missing.species[98]
unknown_bodymass[98,2] <- b97$Body.mass..g.

missing.species[99]
b99 <- bodymass[bodymass$Species=="Aphelocoma ultramarina",]
unknown_bodymass[99,2] <- b99$Body.mass..g.

missing.species[100]
b100a <- bodymass[bodymass$Species=="Corvus brachyrhynchos",]
b100b <- bodymass[bodymass$Species=="Corvus ossifragus",]
unknown_bodymass[100,2] <- (b100a$Body.mass..g.+b100b$Body.mass..g.)/2

missing.species[101]
b101a <- bodymass[bodymass$Species=="Corvus brachyrhynchos",]
b101b <- bodymass[bodymass$Species=="Corvus caurinus",]
unknown_bodymass[101,2] <- (b101a$Body.mass..g.+b101b$Body.mass..g.)/2

missing.species[102]
b102a <- bodymass[bodymass$Species=="Corvus corax",]
b102b <- bodymass[bodymass$Species=="Corvus cryptoleucus",]
unknown_bodymass[102,2] <- (b102a$Body.mass..g.+b102b$Body.mass..g.)/2

missing.species[103]
b103a <- bodymass[bodymass$Species=="Petrochelidon pyrrhonota",]
b103b <- bodymass[bodymass$Species=="Petrochelidon fulva",]
unknown_bodymass[103,2] <- (b103a$Body.mass..g.+b103b$Body.mass..g.)/2

missing.species[104]
b104a <- bodymass[bodymass$Species=="Poecile carolinensis",]
b104b <- bodymass[bodymass$Species=="Poecile atricapilla",]
unknown_bodymass[104,2] <- (b104a$Body.mass..g.+b104b$Body.mass..g.)/2

missing.species[105]
b105a <- bodymass[bodymass$Species=="Poecile gambeli",]
b105b <- bodymass[bodymass$Species=="Poecile atricapilla",]
unknown_bodymass[105,2] <- (b105a$Body.mass..g.+b105b$Body.mass..g.)/2

missing.species[106]
b106a <- bodymass[bodymass$Species=="Poecile hudsonica",]
b106b <- bodymass[bodymass$Species=="Poecile atricapilla",]
unknown_bodymass[106,2] <- (b106a$Body.mass..g.+b106b$Body.mass..g.)/2

missing.species[107]
missing.species[108]
b108 <- bodymass[bodymass$Species=="Baeolophus inornatus",]
unknown_bodymass[108,2] <- b108$Body.mass..g.

missing.species[109]
unknown_bodymass[109,2] <- b108$Body.mass..g.

missing.species[110]
b110 <- bodymass[bodymass$Species=="Baeolophus bicolor",]
unknown_bodymass[110,2] <- b110$Body.mass..g.

missing.species[111]
unknown_bodymass[111,2] <- b110$Body.mass..g.

missing.species[112]
b112 <- bodymass[bodymass$Species=="Troglodytes troglodytes",]
unknown_bodymass[112,2] <- b112$Body.mass..g.

missing.species[113]
b113 <- bodymass[bodymass$Species=="Troglodytes troglodytes",]
unknown_bodymass[113,2] <- b113$Body.mass..g.

missing.species[114]

missing.species[115]
b115a <- bodymass[bodymass$Species=="Polioptila melanura",]
b115b <- bodymass[bodymass$Species=="Polioptila caerulea",]
unknown_bodymass[115,2] <- (b115a$Body.mass..g.[1]+b115b$Body.mass..g.)/2

missing.species[116]
unknown_bodymass[116,2] <- 28

missing.species[117]
#57 - 85 g
unknown_bodymass[117,2] <- 57+((85-57)/2)

missing.species[118]
missing.species[119]
b119a <- bodymass[bodymass$Species=="Bombycilla cedrorum",]
b119b <- bodymass[bodymass$Species=="Bombycilla garrulus",]
unknown_bodymass[119,2] <- (b119a$Body.mass..g.[1]+b119b$Body.mass..g.)/2

missing.species[120]
unknown_bodymass[120,2] <- 17

missing.species[121]
missing.species[122]
unknown_bodymass[122,2] <- 14

missing.species[123]
unknown_bodymass[123,2] <- 24

missing.species[124]
b124 <- bodymass[bodymass$Species=="Motacilla flava",]
unknown_bodymass[124,2] <- b124$Body.mass..g.

missing.species[125]
b125 <- bodymass[bodymass$Species=="Leucosticte tephrocotis",]
unknown_bodymass[125,2] <- b125$Body.mass..g.

missing.species[126]
b126a <- bodymass[bodymass$Species=="Carpodacus purpureus",]
b126b <- bodymass[bodymass$Species=="Carpodacus cassinii",]
b126c <- bodymass[bodymass$Species=="Carpodacus mexicanus",]
unknown_bodymass[126,2] <- (b126a$Body.mass..g.+b126b$Body.mass..g.+b126b$Body.mass..g.)/3

missing.species[127]
b127a <- bodymass[bodymass$Species=="Carduelis flammea",]
b127b <- bodymass[bodymass$Species=="Carduelis hornemanni",]
unknown_bodymass[127,2] <- (b127a$Body.mass..g.+b127b$Body.mass..g.)/2

missing.species[128]
b128 <- bodymass[bodymass$Species=="Loxia curvirostra",]
unknown_bodymass[128,2] <- b128$Body.mass..g.

missing.species[129]
b129a <- bodymass[bodymass$Species=="Loxia leucoptera",]
b129b <- bodymass[bodymass$Species=="Loxia curvirostra",]
unknown_bodymass[127,2] <- (b129a$Body.mass..g.+b129b$Body.mass..g.)/2

missing.species[130]
unknown_bodymass[130,2] <- 16.5

missing.species[131]
b131 <- bodymass[bodymass$Species=="Junco hyemalis",]
unknown_bodymass[131,2] <- b131$Body.mass..g.

missing.species[132]
unknown_bodymass[132,2] <- b131$Body.mass..g.

missing.species[133]
unknown_bodymass[133,2] <- b131$Body.mass..g.

missing.species[134]
unknown_bodymass[134,2] <- b131$Body.mass..g.

missing.species[135]
unknown_bodymass[135,2] <- b131$Body.mass..g.

missing.species[136]
b136 <- bodymass[bodymass$Species=="Amphispiza belli",]
unknown_bodymass[136,2] <- b136$Body.mass..g.[1]

missing.species[137]
unknown_bodymass[137,2] <- b136$Body.mass..g.[1]

missing.species[138]
unknown_bodymass[138,2] <- b136$Body.mass..g.[1]

missing.species[139]
unknown_bodymass[139,2] <- 14

missing.species[140]
b140 <- bodymass[bodymass$Species=="Ammodramus caudacutus",]
unknown_bodymass[140,2] <- b140$Body.mass..g.[1]

missing.species[141]
unknown_bodymass[141,2] <- b140$Body.mass..g.[1]

missing.species[142]
b142a <- bodymass[bodymass$Species=="Pipilo maculatus",]
b142b <- bodymass[bodymass$Species=="Pipilo erythrophtalmus",]
unknown_bodymass[142,2] <- (b142a$Body.mass..g.+b142b$Body.mass..g.)/2

missing.species[143]
b143a <- bodymass[bodymass$Species=="Sturnella magna",]
b143b <- bodymass[bodymass$Species=="Sturnella neglecta",]
unknown_bodymass[143,2] <- (b143a$Body.mass..g.+b143b$Body.mass..g.)/2

missing.species[144]
b144a <- bodymass[bodymass$Species=="Icterus bullocki",]
b144b <- bodymass[bodymass$Species=="Icterus galbula",]
unknown_bodymass[144,2] <- (b144a$Body.mass..g.+b144b$Body.mass..g.)/2

missing.species[145]
unknown_bodymass[145,2] <- (b144a$Body.mass..g.+b144b$Body.mass..g.)/2

missing.species[146]
unknown_bodymass[146,2] <- 60

missing.species[147]
b147a <- bodymass[bodymass$Species=="Molothrus aeneus",]
b147b <- bodymass[bodymass$Species=="Molothrus ater",]
unknown_bodymass[147,2] <- (b147a$Body.mass..g.+b147b$Body.mass..g.)/2

missing.species[148]
b148a <- bodymass[bodymass$Species=="Quiscalus major",]
b148b <- bodymass[bodymass$Species=="Quiscalus mexicanus",]
unknown_bodymass[148,2] <- (b148a$Body.mass..g.+b148b$Body.mass..g.)/2

missing.species[149]
b149a <- bodymass[bodymass$Species=="Vermivora chrysoptera",]
b149b <- bodymass[bodymass$Species=="Vermivora pinus",]
unknown_bodymass[149,2] <- (b149a$Body.mass..g.+b149b$Body.mass..g.)/2

missing.species[150]
unknown_bodymass[150,2] <- (b149a$Body.mass..g.+b149b$Body.mass..g.)/2

missing.species[151]
b151 <- bodymass[bodymass$Species=="Dendroica cerulea",]
unknown_bodymass[150,2] <- b151$Body.mass..g.

missing.species[152]
b152 <- bodymass[bodymass$Species=="Dendroica coronata",]
unknown_bodymass[152,2] <- b152$Body.mass..g.

missing.species[153]
unknown_bodymass[153,2] <- b152$Body.mass..g.

missing.species[154]
unknown_bodymass[154,2] <- b152$Body.mass..g.

missing.species[155]
b155a <- bodymass[bodymass$Species=="Dendroica townsendi",]
b155b <- bodymass[bodymass$Species=="Dendroica occidentalis",]
unknown_bodymass[155,2] <- (b155a$Body.mass..g.+b155b$Body.mass..g.)/2

missing.species[156]
unknown_bodymass[156,2] <- (b155a$Body.mass..g.+b155b$Body.mass..g.)/2

missing.species[157]
b157a <- bodymass[bodymass$Species=="Cardinalis cardinalis",]
b157b <- bodymass[bodymass$Species=="Cardinalis sinuatus",]
unknown_bodymass[157,2] <- (b157a$Body.mass..g.+b157b$Body.mass..g.)/2

missing.species[158]
b158a <- bodymass[bodymass$Species=="Passerina amoena",]
b158b <- bodymass[bodymass$Species=="Passerina cyanea",]
unknown_bodymass[158,2] <- (b158a$Body.mass..g.+b158b$Body.mass..g.)/2


#### 4 Merge missing bodymass and calculate total energy per route ####
# Rename columns for clarity
colnames(unknown_bodymass) <- c("Genus_Species", "Bodymass")
unknown_bodymass <- as.data.frame(unknown_bodymass)

#add unknown bodymass to aou_bodymass dataframe
# Add AOU codes by joining with species_List
unknown_bodymass <- unknown_bodymass %>%
  left_join(species_List %>% select(Genus_Species, AOU), by = "Genus_Species")

# Replace missing bodymass values in aou_bodymass using left_join
aou_bodymass <- aou_bodymass %>%
  mutate(AOU = as.numeric(AOU)) %>%  # Ensure AOU is numeric if needed
  left_join(unknown_bodymass %>% select(AOU, Bodymass_unknown = Bodymass), by = "AOU") %>%
  mutate(
    Bodymass = ifelse(is.na(Bodymass), Bodymass_unknown, Bodymass),
    Bodymass = as.numeric(Bodymass),
    E = Bodymass^0.75
  ) %>%
  select(-Bodymass_unknown)

#load result data BBS and overall BBS data
results_BBS_df <- read.csv("data_2024/Results_BBS_2024.csv")
States_combined <- read.csv("data_2024/States_combined_2024.csv")#load route BCR?s

# Merge States_combined with aou_bodymass to add E
combined <- States_combined %>%
  mutate(AOU = as.integer(AOU), SpeciesTotal = as.integer(SpeciesTotal)) %>%
  left_join(aou_bodymass, by = "AOU")

# Calculate E_total per route
E_summary <- combined %>%
  group_by(RouteDataID) %>%
  summarise(E_total = sum(SpeciesTotal * E, na.rm = TRUE), .groups = "drop")

results_BBS_df <- results_BBS_df %>%
  left_join(E_summary, by = "RouteDataID")

write.csv(results_BBS_df, "data_2024/Results_BBS_complete.csv", row.names = FALSE)


