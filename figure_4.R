### ============================================================================
### Figure 4 — Zero-sum dynamics: log(N) ~ log(individual energy demand)
### Authors: Lina K. Mühlbauer & Daria Lipsky
### ============================================================================


# --- Setup -------------------------------------------------------------------
rm(list = ls())

library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)

setwd("...")

# --- Parameters --------------------------------------------------------------
split_year        <- 1994    # year used to split pre/post periods
exclude_early_years <- FALSE # set TRUE to drop 1966 & 1967 from all analyses
RECENT_START      <- 1966    # start year for analyses (set to 1994 for recent window only)

suffix <- "fullseries"       # used in output filenames


# --- Load & filter data ------------------------------------------------------
results <- read.csv("data_2025/RouteMetrics_2025_diet.csv")

if (exclude_early_years) {
  results <- results %>% filter(!Year %in% c(1966, 1967))
  suffix  <- "no6667"
}


# --- Yearly means ------------------------------------------------------------
MeanGini_year <- results %>%
  filter(Year >= RECENT_START) %>%
  group_by(Year) %>%
  summarise(
    Mean_Gini       = mean(GINI,              na.rm = TRUE),
    Mean_N          = mean(N,                 na.rm = TRUE),
    Mean_S          = mean(S,                 na.rm = TRUE),
    Mean_E_diet     = mean(E_total_diet,       na.rm = TRUE),
    Mean_EN_diet    = mean(E_total_diet / N,   na.rm = TRUE),
    Mean_trophiclev = mean(Ave_trophic_level,  na.rm = TRUE),
    Mean_bodymass   = mean(Ave_bodymass,       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Period = ifelse(Year < split_year,
                         paste0("< ", split_year),
                         paste0("\u2265 ", split_year)))


# =============================================================================
# Statistics — printed to console
# =============================================================================

lm_stats <- function(formula, data, label) {
  m  <- lm(formula, data = data)
  sm <- summary(m)
  cf <- coef(sm)
  pred_name <- rownames(cf)[2]
  cat(sprintf(
    "\n--- %s ---\n  slope = %.4f  SE = %.4f  t = %.3f  p = %.4g  adj.R2 = %.4f\n",
    label,
    cf[pred_name, "Estimate"],
    cf[pred_name, "Std. Error"],
    cf[pred_name, "t value"],
    cf[pred_name, "Pr(>|t|)"],
    sm$adj.r.squared
  ))
  invisible(m)
}

cat("\n=================================================================\n")
cat("STATISTICAL SUMMARIES \u2014 Figure 4\n")
cat("=================================================================\n")

mod_all     <- lm_stats(log(Mean_N) ~ log(Mean_EN_diet), MeanGini_year,
                        "log(N) ~ log(E/ind)  \u2014  all years")
mod_pre     <- lm(log(Mean_N) ~ log(Mean_EN_diet),
                  data = MeanGini_year, subset = Year < split_year)
mod_post    <- lm(log(Mean_N) ~ log(Mean_EN_diet),
                  data = MeanGini_year, subset = Year >= split_year)
lm_stats(log(Mean_N) ~ log(Mean_EN_diet),
         filter(MeanGini_year, Year < split_year),
         paste0("log(N) ~ log(E/ind)  \u2014  pre-", split_year))
lm_stats(log(Mean_N) ~ log(Mean_EN_diet),
         filter(MeanGini_year, Year >= split_year),
         paste0("log(N) ~ log(E/ind)  \u2014  post-", split_year))
mod_bm_n    <- lm_stats(log(Mean_N)       ~ log(Mean_bodymass), MeanGini_year,
                        "log(N) ~ log(body mass)")
mod_bm_en   <- lm_stats(log(Mean_EN_diet) ~ log(Mean_bodymass), MeanGini_year,
                        "log(E/ind) ~ log(body mass)")
mod_bm_etot <- lm_stats(log(Mean_E_diet)  ~ log(Mean_bodymass), MeanGini_year,
                        "log(E_total) ~ log(body mass)")
mod_raw     <- lm_stats(Mean_N ~ Mean_EN_diet, MeanGini_year,
                        "N ~ E/ind  (raw, unlogged)")

# Interaction model: test whether pre- and post-split slopes differ significantly
mod_interact <- lm(
  log(Mean_N) ~ log(Mean_EN_diet) * (Year >= split_year),
  data = MeanGini_year
)
cat(sprintf("\n--- Interaction test: does slope differ pre/post %d? ---\n", split_year))
print(summary(mod_interact))

cat("\n")

# Helper: extract slope and adj.R² for figure annotations
ann <- function(mod, digits_slope = 3, digits_r2 = 3) {
  sm   <- summary(mod)
  cf   <- coef(sm)
  pval <- cf[2, "Pr(>|t|)"]
  p_str <- ifelse(pval < 0.001, "p<0.001", paste0("p=", round(pval, 3)))
  list(
    slope  = round(cf[2, "Estimate"], digits_slope),
    adj_r2 = round(sm$adj.r.squared, digits_r2),
    p_str  = p_str
  )
}


# =============================================================================
# Publication theme
# =============================================================================

pub_theme <- theme_classic(base_size = 12) +
  theme(
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 10, colour = "black"),
    plot.tag         = element_text(size = 13, face = "bold"),
    strip.background = element_blank()
  )
theme_set(pub_theme)

# Colours
pt_fill  <- "#6BAED6"
pt_col   <- "#08306B"
line_col <- "#2171B5"
se_fill  <- "#BDD7E7"
col_pre  <- "#555555"
col_post <- "#C2506E"
col_all  <- "#2171B5"
fill_all <- "#BDD7E7"
col_ref  <- "#2166ac"

# Axis ranges (shared across panels)
x_rng <- range(log(MeanGini_year$Mean_EN_diet), na.rm = TRUE)
y_rng <- range(log(MeanGini_year$Mean_N),        na.rm = TRUE)

period_cols <- setNames(c(col_pre, col_post),
                        c(paste0("< ", split_year),
                          paste0("\u2265 ", split_year)))



# =============================================================================
# FIGURE 4 (main manuscript) — combined: overall + pre/post fits
# =============================================================================

a_all  <- ann(mod_all)
a_pre  <- ann(mod_pre)
a_post <- ann(mod_post)

fig4_combined <- ggplot(MeanGini_year,
                        aes(x = log(Mean_EN_diet), y = log(Mean_N))) +
  # Overall regression line (solid blue)
  geom_smooth(method = "lm", formula = y ~ x,
              se = TRUE, colour = col_all, fill = fill_all,
              linewidth = 1.0, alpha = 0.20) +
  # Pre-split regression (dashed grey)
  geom_smooth(data = filter(MeanGini_year, Year < split_year),
              method = "lm", formula = y ~ x,
              se = TRUE, colour = col_pre, fill = col_pre,
              linewidth = 0.85, alpha = 0.15,
              linetype = "dashed") +
  # Post-split regression (dashed rose)
  geom_smooth(data = filter(MeanGini_year, Year >= split_year),
              method = "lm", formula = y ~ x,
              se = TRUE, colour = col_post, fill = col_post,
              linewidth = 0.85, alpha = 0.15,
              linetype = "dashed") +
  geom_point(aes(colour = Period, fill = Period),
             shape = 21, size = 2.6,
             stroke = 0.5, alpha = 0.85) +
  scale_colour_manual(values = period_cols, guide = "none") +
  scale_fill_manual(  values = period_cols, guide = "none") +
  labs(x = "log(Individual energy demand, J / route / ind)",
       y = "log(Number of individuals, N)")

ggsave(file.path("figures", paste0("figure_4_", suffix, ".pdf")),
       fig4_combined, width = 7, height = 5.5, device = cairo_pdf)
ggsave(file.path("figures", paste0("figure_4_", suffix, ".png")),
       fig4_combined, width = 7, height = 5.5, dpi = 300)


