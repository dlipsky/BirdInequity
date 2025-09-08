### Figure4_Time_Diet
### Project: Ecology and Economics
### Daria Lipsky

rm(list = ls())

require(DescTools)
require(vegan)
require(scales)
require(akima)
library(dplyr)
library(ggplot2)
library(segmented)
library(broom)
library(tidyr)


# helper function to get slope + p-value
get_slope <- function(df, response) {
  mod <- lm(df[[response]] ~ df$Year)
  out <- tidy(mod)
  out <- out[out$term == "df$Year", c("estimate", "p.value")]
  names(out) <- c("slope", "p.value")
  return(out)
}

setwd("C:/Users/aa82eluj/Desktop/BirdInequity/")

#load results_BBS
results_BBS <- read.csv("data_2024/Results_BBS_complete_diet.csv")

routes70s_bool <- FALSE

if(routes70s_bool){
  #analysing subset of routes that were already surveyed in the 1970s to check if the effects we found are driven by newer routes.
  routes70s <- results_BBS %>% filter(Year<=1979) %>% dplyr::select(State, Route)
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
    Mean_bodymass_Plant = mean(Ave_bodymass_PlantSeed, na.rm = TRUE),
    Mean_bodymass_Omni = mean(Ave_bodymass_Omnivore, na.rm = TRUE),
    Mean_bodymass_Inv = mean(Ave_bodymass_Inv, na.rm = TRUE),
    Mean_bodymass_Vert = mean(Ave_bodymass_Vert, na.rm = TRUE),
    Mean_bodymass_Fruit = mean(Ave_bodymass_FruiNec, na.rm = TRUE),
    .groups = "drop"
  )

#### 1 Plot Diet Categories ####

##### 1.1 E: energy demand #####
results <- data.frame(
  Diet = c("PlantSeed", "Omnivore", "Invertebrate", "Vertebrate", "FruiNect"),
  rbind(
    get_slope(MeanGini_year, "Mean_E_Plant"),
    get_slope(MeanGini_year, "Mean_E_Omni"),
    get_slope(MeanGini_year, "Mean_E_Inv"),
    get_slope(MeanGini_year, "Mean_E_Vert"),
    get_slope(MeanGini_year, "Mean_E_Fruit")
  )
)

results$cv <- c(
  sd(MeanGini_year$Mean_E_Plant, na.rm = TRUE) / mean(MeanGini_year$Mean_E_Plant, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_E_Omni, na.rm = TRUE) / mean(MeanGini_year$Mean_E_Omni, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_E_Inv, na.rm = TRUE) / mean(MeanGini_year$Mean_E_Inv, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_E_Vert, na.rm = TRUE) / mean(MeanGini_year$Mean_E_Vert, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_E_Fruit, na.rm = TRUE) / mean(MeanGini_year$Mean_E_Fruit, na.rm = TRUE) * 100
)

print(results)

if(routes70s_bool){
  pdf(width = 8, height = 5, file="figure_2_diet_energy_routes70.pdf")
}else{
  pdf(width = 8, height = 5, file="figure_2_diet_energy_allroutes.pdf")
}
ggplot(MeanGini_year, aes(x = Year)) +
 geom_point(aes(y = Mean_E_Plant, color = "PlantSeed"), size = 2) +
 geom_smooth(aes(y = Mean_E_Plant, color = "PlantSeed"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_E_Omni, color = "Omnivore"), size = 2) +
  geom_smooth(aes(y = Mean_E_Omni, color = "Omnivore"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_E_Inv, color = "Invertebrate"), size = 2) +
  geom_smooth(aes(y = Mean_E_Inv, color = "Invertebrate"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_E_Vert, color = "Vertebrate"), size = 2) +
  geom_smooth(aes(y = Mean_E_Vert, color = "Vertebrate"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_E_Fruit, color = "FruiNect"), size = 2) +
  geom_smooth(aes(y = Mean_E_Fruit, color = "FruiNect"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  scale_color_manual(
    values = c(
      "Omnivore" = "#d95f02",
      "PlantSeed" = "#1b9e77",
      "Vertebrate" = "#e7298a",
      "Invertebrate" = "#7570b3",
      "FruiNect" = "orange"
    ),
    breaks = c("Omnivore", "PlantSeed", "Vertebrate", "Invertebrate", "FruiNect")  # legend order
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2020, by = 10),  
    limits = c(1966, 2023)             
  ) +
  labs(y = "Mean Energy Demand",
       x = "Year",
       color = "Diet Category",
       title = "Mean Energy Demand by Diet Category Over Time") +
  theme_bw() +
  theme(legend.position = "right", 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

dev.off()

##### 1.2 S: number of species ####
results <- data.frame(
  Diet = c("PlantSeed", "Omnivore", "Invertebrate", "Vertebrate", "FruiNect"),
  rbind(
    get_slope(MeanGini_year, "Mean_S_Plant"),
    get_slope(MeanGini_year, "Mean_S_Omni"),
    get_slope(MeanGini_year, "Mean_S_Inv"),
    get_slope(MeanGini_year, "Mean_S_Vert"),
    get_slope(MeanGini_year, "Mean_S_Fruit")
  )
)
# add CVs
results$CV <- c(
  sd(MeanGini_year$Mean_S_Plant, na.rm = TRUE) / mean(MeanGini_year$Mean_S_Plant, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_S_Omni, na.rm = TRUE) / mean(MeanGini_year$Mean_S_Omni, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_S_Inv, na.rm = TRUE) / mean(MeanGini_year$Mean_S_Inv, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_S_Vert, na.rm = TRUE) / mean(MeanGini_year$Mean_S_Vert, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_S_Fruit, na.rm = TRUE) / mean(MeanGini_year$Mean_S_Fruit, na.rm = TRUE) * 100
)

print(results)

if(routes70s_bool){
  pdf(width = 8, height = 5, file="figure_2_diet_S_routes70.pdf")
}else{
  pdf(width = 8, height = 5, file="figure_2_diet_S_allroutes.pdf")
}
ggplot(MeanGini_year, aes(x = Year)) +
  geom_point(aes(y = Mean_S_Plant, color = "PlantSeed"), size = 2) +
  geom_smooth(aes(y = Mean_S_Plant, color = "PlantSeed"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  # geom_line(aes(y = Mean_S_Plant, color = "PlantSeed"), linewidth = 1) +

  # geom_point(aes(y = Mean_S_Omni, color = "Omnivore"), size = 2) +
  # geom_smooth(aes(y = Mean_S_Omni, color = "Omnivore"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  # 
  # geom_point(aes(y = Mean_S_Inv, color = "Invertebrate"), size = 2) +
  # geom_smooth(aes(y = Mean_S_Inv, color = "Invertebrate"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  # 
  geom_point(aes(y = Mean_S_Vert, color = "Vertebrate"), size = 2) +
  geom_smooth(aes(y = Mean_S_Vert, color = "Vertebrate"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_S_Fruit, color = "FruiNect"), size = 2) +
  geom_smooth(aes(y = Mean_S_Fruit, color = "FruiNect"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  scale_color_manual(
    values = c(
      "Omnivore" = "#d95f02",
      "PlantSeed" = "#1b9e77",
      "Vertebrate" = "#e7298a",
      "Invertebrate" = "#7570b3",
      "FruiNect" = "orange"
    ),
    breaks = c("Omnivore", "PlantSeed", "Vertebrate", "Invertebrate", "FruiNect")  # legend order
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2020, by = 10),  
    limits = c(1966, 2023)             
  ) +
  labs(y = "Mean Species Richness",
       x = "Year",
       color = "Diet Category",
       title = "Mean Species Richness by Diet Category Over Time") +
  theme_bw() +
  theme(legend.position = "right", 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
dev.off()

##### 1.3 N: number of individuals #####

results <- data.frame(
  Diet = c("PlantSeed", "Omnivore", "Invertebrate", "Vertebrate", "FruiNect"),
  rbind(
    get_slope(MeanGini_year, "Mean_N_Plant"),
    get_slope(MeanGini_year, "Mean_N_Omni"),
    get_slope(MeanGini_year, "Mean_N_Inv"),
    get_slope(MeanGini_year, "Mean_N_Vert"),
    get_slope(MeanGini_year, "Mean_N_Fruit")
  )
)
# add CVs
results$CV <- c(
  sd(MeanGini_year$Mean_N_Plant, na.rm = TRUE) / mean(MeanGini_year$Mean_N_Plant, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_N_Omni, na.rm = TRUE) / mean(MeanGini_year$Mean_N_Omni, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_N_Inv, na.rm = TRUE) / mean(MeanGini_year$Mean_N_Inv, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_N_Vert, na.rm = TRUE) / mean(MeanGini_year$Mean_N_Vert, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_N_Fruit, na.rm = TRUE) / mean(MeanGini_year$Mean_N_Fruit, na.rm = TRUE) * 100
)

print(results)

if(routes70s_bool){
  pdf(width = 8, height = 5, file="figure_2_diet_N_routes70.pdf")
}else{
  pdf(width = 8, height = 5, file="figure_2_diet_N_allroutes.pdf")
}
ggplot(MeanGini_year, aes(x = Year)) +
  geom_point(aes(y = Mean_N_Plant, color = "PlantSeed"), size = 2) +
  geom_smooth(aes(y = Mean_N_Plant, color = "PlantSeed"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_N_Omni, color = "Omnivore"), size = 2) +
  geom_smooth(aes(y = Mean_N_Omni, color = "Omnivore"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_N_Inv, color = "Invertebrate"), size = 2) +
  geom_smooth(aes(y = Mean_N_Inv, color = "Invertebrate"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_N_Vert, color = "Vertebrate"), size = 2) +
  geom_smooth(aes(y = Mean_N_Vert, color = "Vertebrate"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_N_Fruit, color = "FruiNect"), size = 2) +
  geom_smooth(aes(y = Mean_N_Fruit, color = "FruiNect"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  labs(y = "Mean Number of Individuals",
       x = "Year",
       color = "Diet Category",
       title = "Long-term Trends in Abundance by Diet Category") +
  scale_color_manual(
    values = c(
      "Omnivore"     = "#d95f02",
      "PlantSeed"    = "#1b9e77",
      "Vertebrate"   = "#e7298a",
      "Invertebrate" = "#7570b3",
      "FruiNect"     = "orange"
    ),
    breaks = c("Omnivore", "PlantSeed", "Vertebrate", "Invertebrate", "FruiNect")
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2020, by = 10),  
    limits = c(1966, 2023)             
  ) +
  theme_bw() +
  theme(legend.position = "right", 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
dev.off()


##### 1.4 Bodymass #####
results <- data.frame(
  Diet = c("PlantSeed", "Omnivore", "Invertebrate", "Vertebrate", "FruiNect"),
  rbind(
    get_slope(MeanGini_year, "Mean_bodymass_Plant"),
    get_slope(MeanGini_year, "Mean_bodymass_Omni"),
    get_slope(MeanGini_year, "Mean_bodymass_Inv"),
    get_slope(MeanGini_year, "Mean_bodymass_Vert"),
    get_slope(MeanGini_year, "Mean_bodymass_Fruit")
  )
)

results$cv <- c(
  sd(MeanGini_year$Mean_bodymass_Plant, na.rm = TRUE) / mean(MeanGini_year$Mean_bodymass_Plant, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_bodymass_Omni, na.rm = TRUE) / mean(MeanGini_year$Mean_bodymass_Omni, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_bodymass_Inv, na.rm = TRUE) / mean(MeanGini_year$Mean_bodymass_Inv, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_bodymass_Vert, na.rm = TRUE) / mean(MeanGini_year$Mean_bodymass_Vert, na.rm = TRUE) * 100,
  sd(MeanGini_year$Mean_bodymass_Fruit, na.rm = TRUE) / mean(MeanGini_year$Mean_bodymass_Fruit, na.rm = TRUE) * 100
)

print(results)

if(routes70s_bool){
  pdf(width = 8, height = 5, file="figure_2_diet_bodymass_routes70.pdf")
}else{
  pdf(width = 8, height = 5, file="figure_2_diet_bodymass_allroutes.pdf")
}
ggplot(MeanGini_year, aes(x = Year)) +
  geom_point(aes(y = Mean_bodymass_Plant, color = "PlantSeed"), size = 2) +
  geom_smooth(aes(y = Mean_bodymass_Plant, color = "PlantSeed"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_bodymass_Omni, color = "Omnivore"), size = 2) +
  geom_smooth(aes(y = Mean_bodymass_Omni, color = "Omnivore"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_bodymass_Inv, color = "Invertebrate"), size = 2) +
  geom_smooth(aes(y = Mean_bodymass_Inv, color = "Invertebrate"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_bodymass_Vert, color = "Vertebrate"), size = 2) +
  geom_smooth(aes(y = Mean_bodymass_Vert, color = "Vertebrate"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  geom_point(aes(y = Mean_bodymass_Fruit, color = "FruiNect"), size = 2) +
  geom_smooth(aes(y = Mean_bodymass_Fruit, color = "FruiNect"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  
  scale_color_manual(
    values = c(
      "Omnivore" = "#d95f02",
      "PlantSeed" = "#1b9e77",
      "Vertebrate" = "#e7298a",
      "Invertebrate" = "#7570b3",
      "FruiNect" = "orange"
    ),
    breaks = c("Omnivore", "PlantSeed", "Vertebrate", "Invertebrate", "FruiNect")  # legend order
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2020, by = 10),  
    limits = c(1966, 2023)             
  ) +
  labs(y = "Mean Bodymass (g)",
       x = "Year",
       color = "Diet Category",
       title = "Mean Bodymass by Diet Category Over Time") +
  theme_bw() +
  theme(legend.position = "right", 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
dev.off()

##### 1.5 Inequality #####


#### 2 Plot Diet-Adjusted Energy Demand per Individual and per Route ####
# plotting both original and diet-adjusted energy demand per individual
# version 1: two y-axes
if(routes70s_bool){
  pdf(width = 6, height = 4, file="figure_2_diet_energy_indiv_routes70s.pdf")
}else{
  pdf(width = 6, height = 4, file="figure_2_diet_energy_indiv_allroutes.pdf")
}
scale_factor <- max(MeanGini_year$Mean_EN, na.rm=TRUE) / max(MeanGini_year$Mean_EN_diet, na.rm=TRUE)
ggplot(MeanGini_year, aes(x = Year)) +
  geom_line(aes(y = Mean_EN, color = "Original"), linewidth = 1) +
  geom_point(aes(y = Mean_EN, color = "Original"), size = 2) +
  
  geom_line(aes(y = Mean_EN_diet * scale_factor, color = "Diet adj"), linewidth = 1) +
  geom_point(aes(y = Mean_EN_diet * scale_factor, color = "Diet adj"), size = 2) +
  
  scale_y_continuous(
    name = "Original Mean Energy Demand per Individual",
    sec.axis = sec_axis(~ . / scale_factor, name = "Diet-adjusted Energy Demand per Route")
  ) +
  scale_color_manual(
    values =  c("Diet adj" = "#2A788EFF", "Original" = "#7AD151FF")
  ) +
  scale_x_continuous(breaks = seq(1970, 2020, by = 10)) +
  labs(x = "Year", color = "Calculation") +
  theme_bw() +
  theme(legend.position = "right")
dev.off()

# plotting both original and diet-adjusted energy demand per individual
# version 2: rescaled y-axes
if(routes70s_bool){
  pdf(width = 6, height = 4, file="figure_2_diet_energy_indiv_scaled_routes70s.pdf")
}else{
  pdf(width = 6, height = 4, file="figure_2_diet_energy_indiv_scaled_allroutes.pdf")
}
MeanGini_year$EN_scaled <- scales::rescale(MeanGini_year$Mean_EN)
MeanGini_year$EN_diet_scaled <- scales::rescale(MeanGini_year$Mean_EN_diet)
ggplot(MeanGini_year, aes(x = Year)) +
  geom_line(aes(y = EN_scaled, color = "Original"), linewidth = 1) +
  geom_point(aes(y = EN_scaled, color = "Original"), size = 2) +
  geom_line(aes(y = EN_diet_scaled, color = "Diet adj"), linewidth = 1) +
  geom_point(aes(y = EN_diet_scaled, color = "Diet adj"), size = 2) +
  scale_color_manual(values = c("Diet adj" = "#2A788EFF", "Original" = "#7AD151FF")) +
  labs(y = "Rescaled Energy Demand per Individual",
       x = "Year",
       color = "Calculation") +
  scale_x_continuous(breaks = seq(1970, 2020, by = 10)) +
  theme_bw() +
  theme(legend.position = "right")
dev.off()

# plotting both original and diet-adjusted energy demand per route
# version 2: rescaled y-axes
if(routes70s_bool){
  pdf(width = 6, height = 4, file="figure_2_diet_energy_route_scaled_quadreg_routes70s.pdf")
}else{
  pdf(width = 6, height = 4, file="figure_2_diet_energy_route_scaled_quadreg_allroutes.pdf")
}
MeanGini_year$E_scaled <- scales::rescale(MeanGini_year$Mean_E)
MeanGini_year$E_diet_scaled <- scales::rescale(MeanGini_year$Mean_E_diet)
ggplot(MeanGini_year, aes(x = Year)) +
  # geom_line(aes(y = E_scaled, color = "Original"), linewidth = 1) +
  geom_point(aes(y = E_scaled, color = "Original"), size = 2) +
  geom_smooth(aes(y = E_scaled, color = "Original"), method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  # geom_line(aes(y = E_diet_scaled, color = "Diet adj"), linewidth = 1) +
  geom_point(aes(y = E_diet_scaled, color = "Diet adj"), size = 2) +
  geom_smooth(aes(y = E_diet_scaled, color = "Diet adj"), method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  scale_color_manual(values = c("Diet adj" = "#2A788EFF", "Original" = "#7AD151FF")) +
  labs(y = "Rescaled Energy Demand per Route",
       x = "Year",
       color = "Calculation") +
  scale_x_continuous(breaks = seq(1970, 2020, by = 10)) +
  theme_bw() +
  theme(legend.position = "right")
dev.off()

ggplot(MeanGini_year, aes(x = Year, y = Mean_E)) +
  # geom_line(color = col[5], size = 1) +
  geom_point(size = 2) +
  labs(y = "Mean Energy Demand per Route",
       x = "Year",
       title = "Mean Energy Demand per Route Over Time") +
  scale_x_continuous(breaks = seq(1970, 2020, by = 10)) +
  theme_bw() +
  # geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linetype = "dotted")
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "darkgreen")
  # geom_smooth(method = "loess", se = TRUE, color = "darkgreen", span = 0.75)


#### Finally: Breakpoint Analysis ####
#regression with breakpoint analysis
lm_e <- lm(Mean_E ~ Year, data = MeanGini_year)
lm_e_seg <- segmented(lm_e, seg.Z = ~ Year)
summary(lm_e_seg)
lm_e_seg$psi[2]

lm_e_seg_fitted <- fitted(lm_e_seg)
lm_e_seg_model <- data.frame(Year = MeanGini_year$Year, Fitted = lm_e_seg_fitted)

if(routes70s_bool){
  pdf(width = 8, height = 6, file="figure_2_energy_demand_breakpoint_route70s.pdf")
}else{
  pdf(width = 8, height = 6, file="figure_2_energy_demand_breakpoint_allroutes.pdf")
}
ggplot(MeanGini_year, aes(x = Year, y = Mean_E)) +
  geom_point(color = "#7AD151FF", size = 2) +
  # geom_line(data = lm_e_seg_model, aes(x = Year, y = Fitted), color = "#22A884FF", size = 1) +
  labs(y = "Mean Energy Demand per Route",
       x = "Year",
       title = "Mean Energy Demand per Route Over Time with Breakpoint Analysis") +
  scale_x_continuous(breaks = seq(1970, 2020, by = 10), limits = c(1970, 2020)) +
  theme_bw() +
  geom_vline(xintercept = lm_e_seg$psi[1], linetype = "dashed", color = "black") 
dev.off()

#### 3 Plot Trophic Level ####
if(routes70s_bool){
  pdf(width = 6, height = 4, file="figure_2_trophic_level_elton_routes70s.pdf")
}else{
  pdf(width = 6, height = 4, file="figure_2_trophic_level_elton_allroutes.pdf")
}
ggplot(MeanGini_year, aes(x = Year)) +
  geom_line(aes(y = Mean_trophiclevel, color = "Trophic Level"), linewidth = 1) +
  geom_point(aes(y = Mean_trophiclevel, color = "Trophic Level"), size = 2) +
  
  scale_color_manual(
    values = c("Trophic Level" = "#7570b3")
  ) +
  
  labs(y = "Mean Trophic Level",
       x = "Year",
       color = "Calculation") +
  
  scale_x_continuous(breaks = seq(1970, 2023, by = 10)) +
  
  theme_bw() +
  theme(legend.position = "", 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
dev.off()


#### 4 Plot Bodymass ####
if(routes70s_bool){
  pdf(width = 6, height = 4, file="figure_2_bodymass_elton_routes70s.pdf")
}else{
  pdf(width = 6, height = 4, file="figure_2_bodymass_elton_allroutes.pdf")
}
ggplot(MeanGini_year, aes(x = Year)) +
  geom_line(aes(y = Mean_bodymass, color = "Body Mass"), linewidth = 1) +
  geom_point(aes(y = Mean_bodymass, color = "Body Mass"), size = 2) +
  
  scale_color_manual(
    values = c("Body Mass" = "#d95f02")
  ) +
  
  labs(y = "Mean Body Mass (g)",
       x = "Year",
       color = "Calculation") +
  
  scale_x_continuous(breaks = seq(1970, 2023, by = 10)) +
  
  theme_bw() +
  theme(legend.position = "", 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
dev.off()
