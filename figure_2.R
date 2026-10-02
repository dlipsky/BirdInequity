### ============================================================================
### Figure 2 - Relationships among Gini, S, N, and Energy Demand
### Authors: Lina K. Mühlbauer & Daria Lipsky
### ============================================================================


# ------------------------------------------------------------------------------
# 1. Setup
# ------------------------------------------------------------------------------
rm(list = ls())

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(ggExtra)
library(patchwork)   # combine panels into one figure
library(viridis)     # perceptually uniform colour scale

setwd("...")  # Set working directory to the location of this script

# ------------------------------------------------------------------------------
# 2. Parameters
# ------------------------------------------------------------------------------
routes_40yrs        <- FALSE   # TRUE — keep only routes surveyed > 40 times

# ------------------------------------------------------------------------------
# 3. Load data
# ------------------------------------------------------------------------------
results <- read.csv("data_2025/RouteMetrics_2025_diet.csv")

if (routes_40yrs) {
  results <- results %>%
    group_by(Route_unique) %>%
    filter(n() > 40) %>%
    ungroup()}

# Build output filename suffix encoding all flags
suffix <- paste0(ifelse(routes_40yrs,"routes40plus", "allroutes"))

# ------------------------------------------------------------------------------
# 4. Publication theme
# ------------------------------------------------------------------------------
pub_theme <- theme_classic(base_size = 12) +
  theme(
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 10, colour = "black"),
    legend.title     = element_text(size = 10),
    legend.text      = element_text(size = 9),
    legend.key.size  = unit(0.4, "cm"),
    plot.tag         = element_text(size = 13, face = "bold"),
    strip.background = element_blank())

theme_set(pub_theme)

# =============================================================================
# 5. Statistics — print to console
# =============================================================================
cat("\n============================================================\n")
cat("STATISTICAL SUMMARIES - Figure 2\n")
cat("============================================================\n\n")

# Panel A: Gini ~ log(N)
mod_gini_n <- lm(GINI ~ log(N), data = results)
s1 <- summary(mod_gini_n)
cat("--- Panel A: Gini ~ log(N) ---\n")
print(s1)

# Panel B: Gini ~ S
mod_gini_s <- lm(GINI ~ S, data = results)
s2 <- summary(mod_gini_s)
cat("--- Panel B: Gini ~ S ---\n")
print(s2)

# Panel D: log(E_total_diet) ~ Gini
mod_e_gini <- lm(log(E_total_diet) ~ GINI, data = results)
s4 <- summary(mod_e_gini)
cat("--- Panel D: log(Energy demand) ~ Gini ---\n")
print(s4)

# Pearson correlations
cr1 <- cor.test(results$GINI, log(results$N), method = "pearson")
cat("--- Pearson correlation: Gini vs log(N) ---\n")
print(cr1)

cr2 <- cor.test(results$GINI, results$S, method = "pearson")
cat("--- Pearson correlation: Gini vs S ---\n")
print(cr2)

cr3 <- cor.test(results$GINI, log(results$E_total_diet), method = "pearson")
cat("--- Pearson correlation: Gini vs log(E_total_diet) ---\n")
print(cr3)

# =============================================================================
# 6. Build panels
# =============================================================================
hex_palette <- colorRampPalette(c("#c5e3e3", "#5ba8a0", "#0d4f4a"))(256)

# --- Panel A: Gini vs log(N), hexagonal binning + regression -----------------
pA <- ggplot(results, aes(x = log(N), y = GINI)) +
  stat_binhex(bins = 60) +
  stat_smooth(method = "lm", colour = "black", linewidth = 0.8, se = TRUE,
              fill = "#ff993366") +
  scale_fill_gradientn(
    colours  = hex_palette,
    trans    = "log10",
    name     = "Number of\nroutes",
    labels   = label_comma()
  ) +
  labs(x = "log(Number of individuals)", y = "Gini coefficient") +
  theme(legend.position = "right")
pA

# --- Panel B: Gini vs S, hexagonal binning -----------------------------------
pB <- ggplot(results, aes(x = S, y = GINI)) +
  stat_binhex(bins = 60) +
  scale_fill_gradientn(
    colours  = hex_palette,
    trans    = "log10",
    name     = "Number of\nroutes",
    labels   = label_comma()
  ) +
  labs(x = "Species richness", y = "Gini coefficient") +
  theme(legend.position = "right")
pB

# --- Panel C: Heatmap — mean Gini across S × log(N) bins --------------------
s_breaks <- seq(0, 120, by = 10)
n_breaks <- seq(0,  10, by =  1)

s_labels <- paste0(head(s_breaks, -1), "\u2013", tail(s_breaks, -1))
n_labels <- paste0(head(n_breaks, -1), "\u2013", tail(n_breaks, -1))

heatmap_df <- results %>%
  mutate(
    logN  = log(N),
    S_bin = cut(S,    breaks = s_breaks, right = FALSE, include.lowest = TRUE,
                labels = s_labels),
    N_bin = cut(logN, breaks = n_breaks, right = FALSE, include.lowest = TRUE,
                labels = n_labels)
  ) %>%
  filter(!is.na(S_bin), !is.na(N_bin)) %>%
  group_by(S_bin, N_bin) %>%
  summarise(mean_gini = mean(GINI, na.rm = TRUE), n = n(), .groups = "drop")

pC <- ggplot(heatmap_df, aes(x = S_bin, y = N_bin, fill = mean_gini)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_viridis(
    option   = "viridis", name = "Mean\nGini",
    limits   = c(0, 1), breaks = seq(0, 1, 0.2),
    na.value = "transparent"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Species richness", y = "log(Number of individuals)") +
  theme(
    legend.position  = "right",
    panel.border     = element_rect(colour = "grey50", fill = NA, linewidth = 0.5),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )
pC

# --- Panel D: Boxplot — log(Energy demand) by Gini bin ----------------------
gini_breaks <- seq(0.2, 1.0, by = 0.1)
gini_labels <- paste0(head(gini_breaks, -1), "\u2013", tail(gini_breaks, -1))

results_d <- results %>%
  mutate(
    GINI_bin = cut(GINI, breaks = gini_breaks, right = FALSE,
                   include.lowest = TRUE, labels = gini_labels)
  ) %>%
  filter(!is.na(GINI_bin), !is.na(E_total_diet), E_total_diet > 0)

pD <- ggplot(results_d, aes(x = GINI_bin, y = log(E_total_diet))) +
  geom_boxplot(
    fill = "grey90", colour = "grey30",
    outlier.size = 0.4, outlier.alpha = 0.3,
    linewidth = 0.4
  ) +
  labs(x = "Gini coefficient", y = "log(Energy demand)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
pD

# =============================================================================
# 7. Combine and save
# =============================================================================
fig2 <- (pB + pA) / (pC + pD) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 13, face = "bold"))

ggsave(
  filename = paste0("figures/figure_2_combined_", suffix, ".pdf"),
  plot     = fig2,
  width    = 10,
  height   = 8,
  device   = cairo_pdf
)

ggsave(
  filename = paste0("figures/figure_2_combined_", suffix, ".png"),
  plot     = fig2,
  width    = 10,
  height   = 8,
  dpi      = 300
)
