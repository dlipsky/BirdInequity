### ============================================================================
### Figure 3 - Temporal Trends by Dietary Guild
### Authors: Lina K. Muehlbauer & Daria Lipsky
### ============================================================================

# =============================================================================
# 1.  SETUP
# =============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)
library(lme4)
library(MuMIn)

setwd("...")

# -- KEY SWITCHES -------------------------------------------------------------
REFIT_GLMM    <- FALSE
LOG_TRANSFORM <- TRUE
LOG_Y_AXIS    <- TRUE

if (LOG_Y_AXIS && !LOG_TRANSFORM)
  warning("LOG_Y_AXIS = TRUE has no effect when LOG_TRANSFORM = FALSE.")

# -- TIME WINDOW --------------------------------------------------------------
GLMM_START <- 1966
GLMM_END   <- 2023

# =============================================================================
# 2.  COLOURS, LABELS, THEME
# =============================================================================

diet_cols <- c(
  "PlantSeed"    = "#4CAF81",
  "Omnivore"     = "#E07B3A",
  "Invertebrate" = "#5B9EC9",
  "VertFishScav" = "#C2506E",
  "FruiNect"     = "#9B6BB5")

diet_labels <- c(
  "PlantSeed"    = "Plant/Seed",
  "Omnivore"     = "Omnivore",
  "Invertebrate" = "Invertebrate",
  "VertFishScav" = "Vert./Fish/Scav.",
  "FruiNect"     = "Fruit/Nectar")
diet_groups <- names(diet_cols)

pub_theme <- theme_classic(base_size = 14) +
  theme(
    axis.title       = element_text(size = 13),
    axis.text        = element_text(size = 11, colour = "black"),
    axis.text.x      = element_text(size = 10),
    plot.tag         = element_text(size = 15, face = "bold"),
    strip.background = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(size = 11),
    legend.text      = element_text(size = 10),
    legend.key.size  = unit(0.6, "cm")
  )
theme_set(pub_theme)


# =============================================================================
# 3.  LOAD DATA
# =============================================================================

results <- read.csv("data_2025/RouteMetrics_2025_diet.csv")

weather <- read.csv("data_2025/Weather.csv") %>%
  mutate(Route_unique = paste(CountryNum, StateNum, Route, RPID, sep = "_")) %>%
  rename(Observer = ObsN)


# =============================================================================
# 4.  BUILD MODELLING DATASET
# =============================================================================

grand_mean_year <- mean(
  results$Year[results$Year >= GLMM_START & results$Year <= GLMM_END],
  na.rm = TRUE
)

data <- results %>%
  mutate(Route_unique = as.character(Route_unique)) %>%
  left_join(
    weather %>% select(Route_unique, Year, Observer),
    by = c("Route_unique", "Year")
  ) %>%
  mutate(
    Year_c       = Year - grand_mean_year,
    Observer     = factor(Observer),
    Route_unique = factor(Route_unique)
  )


# =============================================================================
# 5.  RESHAPE TO LONG FORMAT
# =============================================================================

data_long <- data %>%
  select(
    RouteDataID, Route_unique, Year, Year_c, Observer,
    S_PlantSeed, S_Omnivore, S_Inv, S_Vert, S_FruiNect,
    N_PlantSeed, N_Omnivore, N_Inv, N_Vert, N_FruiNect,
    Ave_bodymass_PlantSeed, Ave_bodymass_Omnivore,
    Ave_bodymass_Inv, Ave_bodymass_Vert, Ave_bodymass_FruiNect,
    GINI_PlantSeed, GINI_Omnivore, GINI_Invertebrate,
    GINI_VertFishScav, GINI_FruiNect,
    E_total_PlantSeed, E_total_Omnivore, E_total_Inv,
    E_total_Vert, E_total_FruiNect
  ) %>%
  rename(
    S_Invertebrate            = S_Inv,
    S_VertFishScav            = S_Vert,
    N_Invertebrate            = N_Inv,
    N_VertFishScav            = N_Vert,
    Ave_bodymass_Invertebrate = Ave_bodymass_Inv,
    Ave_bodymass_VertFishScav = Ave_bodymass_Vert,
    E_total_Invertebrate      = E_total_Inv,
    E_total_VertFishScav      = E_total_Vert
  ) %>%
  pivot_longer(
    cols = matches(
      "^(S|N|Ave_bodymass|GINI|E_total)_(PlantSeed|Omnivore|Invertebrate|VertFishScav|FruiNect)$"
    ),
    names_to      = c(".value", "DietGroup"),
    names_pattern = "^(.+)_(PlantSeed|Omnivore|Invertebrate|VertFishScav|FruiNect)$"
  ) %>%
  rename(
    S_diet    = S,
    N_diet    = N,
    BM_diet   = Ave_bodymass,
    Gini_diet = GINI,
    E_diet    = E_total
  ) %>%
  mutate(DietGroup = factor(DietGroup, levels = diet_groups))


# =============================================================================
# 6.  PANEL SPECS
# =============================================================================

panel_specs <- list(
  list(key = "S_diet",    use_log = FALSE, tag = "A",
       ylab_raw = "Species richness",
       ylab_log = "Species richness"),
  list(key = "N_diet",    use_log = TRUE,  tag = "B",
       ylab_raw = "Number of individuals",
       ylab_log = "Number of individuals"),
  list(key = "Gini_diet", use_log = FALSE, tag = "C",
       ylab_raw = "Gini coefficient",
       ylab_log = "Gini coefficient"),
  list(key = "BM_diet",   use_log = TRUE,  tag = "D",
       ylab_raw = "Mean body mass (g)",
       ylab_log = "Mean body mass (g)"),
  list(key = "E_diet",    use_log = TRUE,  tag = "E",
       ylab_raw = "Energy demand (J)",
       ylab_log = "Energy demand (J)")
)


# =============================================================================
# 7.  FITTING FUNCTIONS
# =============================================================================

bobyqa_ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# -- Shared prediction + stats builder ----------------------------------------
build_fit_obj_diet <- function(m, fit_data, link) {

  newdata <- data.frame(
    Year   = seq(GLMM_START, GLMM_END),
    Year_c = seq(GLMM_START, GLMM_END) - grand_mean_year
  )
  mm     <- model.matrix(~ Year_c, data = newdata)
  eta    <- as.vector(mm %*% fixef(m))
  se_eta <- sqrt(diag(mm %*% tcrossprod(vcov(m), mm)))

  if (link == "log") {
    newdata$pred     <- exp(eta)
    newdata$lower_ci <- exp(eta - 1.96 * se_eta)
    newdata$upper_ci <- exp(eta + 1.96 * se_eta)
  } else {
    newdata$pred     <- eta
    newdata$lower_ci <- eta - 1.96 * se_eta
    newdata$upper_ci <- eta + 1.96 * se_eta
  }

  coefs <- summary(m)$coefficients
  tcol  <- ifelse("t value" %in% colnames(coefs), "t value", "z value")

  r2 <- tryCatch(
    MuMIn::r.squaredGLMM(m),
    error   = function(e) matrix(c(NA_real_, NA_real_), nrow = 1,
                                 dimnames = list(NULL, c("R2m", "R2c"))),
    warning = function(w) suppressWarnings(MuMIn::r.squaredGLMM(m))
  )

  list(
    line         = newdata,
    slope        = coefs["Year_c", "Estimate"],
    se           = coefs["Year_c", "Std. Error"],
    t            = coefs["Year_c", tcol],
    pval         = 2 * pnorm(abs(coefs["Year_c", tcol]), lower.tail = FALSE),
    AIC          = AIC(m),
    intercept    = fixef(m)["(Intercept)"],
    intercept_se = coefs["(Intercept)", "Std. Error"],
    link         = link,
    R2m          = r2[1, "R2m"],
    R2c          = r2[1, "R2c"]
  )
}

# -- Raw (identity) fitter ----------------------------------------------------
fit_diet_identity <- function(yvar, diet) {
  message("  identity | ", yvar, " | ", diet)
  d <- data_long %>%
    filter(DietGroup == diet, Year >= GLMM_START, Year <= GLMM_END,
           !is.na(.data[[yvar]]))
  fml <- as.formula(
    paste0(yvar, " ~ Year_c + (1 + Year_c | Route_unique) + (1 | Observer)")
  )
  m <- tryCatch(
    lmer(fml, data = d, REML = FALSE, control = bobyqa_ctrl),
    error   = function(e) { message("    FAILED: ", e$message); NULL },
    warning = function(w) suppressWarnings(
      lmer(fml, data = d, REML = FALSE, control = bobyqa_ctrl)
    )
  )
  if (is.null(m)) return(NULL)
  build_fit_obj_diet(m, d, link = "identity")
}

# -- Log-transform fitter -----------------------------------------------------
fit_diet_log <- function(yvar, diet) {
  message("  log-transform | ", yvar, " | ", diet)
  log_yvar <- paste0("log_", yvar)
  d <- data_long %>%
    filter(DietGroup == diet, Year >= GLMM_START, Year <= GLMM_END,
           !is.na(.data[[yvar]]), .data[[yvar]] > 0) %>%
    mutate(!!log_yvar := log(.data[[yvar]]))
  fml <- as.formula(
    paste0(log_yvar, " ~ Year_c + (1 + Year_c | Route_unique) + (1 | Observer)")
  )
  m <- tryCatch(
    lmer(fml, data = d, REML = FALSE, control = bobyqa_ctrl),
    error   = function(e) { message("    FAILED: ", e$message); NULL },
    warning = function(w) suppressWarnings(
      lmer(fml, data = d, REML = FALSE, control = bobyqa_ctrl)
    )
  )
  if (is.null(m)) return(NULL)
  build_fit_obj_diet(m, d, link = "log")
}

# -- Z-scored fitter (for standardised slope in stats table) ------------------
fit_diet_zscore <- function(yvar, diet, use_log) {
  message("  z-score | ", yvar, " | ", diet)
  z_yvar <- paste0(yvar, "_z")
  d <- data_long %>%
    filter(DietGroup == diet, Year >= GLMM_START, Year <= GLMM_END,
           !is.na(.data[[yvar]]))
  if (use_log && LOG_TRANSFORM) {
    d    <- d %>% filter(.data[[yvar]] > 0)
    vals <- log(d[[yvar]])
  } else {
    vals <- d[[yvar]]
  }
  mn <- mean(vals, na.rm = TRUE); s <- sd(vals, na.rm = TRUE)
  if (is.na(s) || s == 0) return(NULL)
  d[[z_yvar]] <- (vals - mn) / s
  fml <- as.formula(
    paste0(z_yvar, " ~ Year_c + (1 + Year_c | Route_unique) + (1 | Observer)")
  )
  m <- tryCatch(
    lmer(fml, data = d, REML = FALSE, control = bobyqa_ctrl),
    error   = function(e) { message("    FAILED: ", e$message); NULL },
    warning = function(w) suppressWarnings(
      lmer(fml, data = d, REML = FALSE, control = bobyqa_ctrl)
    )
  )
  if (is.null(m)) return(NULL)
  coefs <- summary(m)$coefficients
  tcol  <- ifelse("t value" %in% colnames(coefs), "t value", "z value")
  list(
    slope = coefs["Year_c", "Estimate"],
    se    = coefs["Year_c", "Std. Error"],
    t     = coefs["Year_c", tcol],
    pval  = 2 * pnorm(abs(coefs["Year_c", tcol]), lower.tail = FALSE)
  )
}


# =============================================================================
# 8.  FIT ALL MODELS (with cache)
# =============================================================================

link_tag     <- if (LOG_TRANSFORM) "_logtransform" else "_idlink"
cache_raw    <- paste0("glmm_fig3_cache_from", GLMM_START, "_raw",    link_tag, ".rds")
cache_zscore <- paste0("glmm_fig3_cache_from", GLMM_START, "_zscore", link_tag, ".rds")

if (!REFIT_GLMM && file.exists(cache_raw) && file.exists(cache_zscore)) {

  message("Loading cached Fig 3 GLMMs: ", cache_raw)
  glmm_fits_raw    <- readRDS(cache_raw)
  glmm_fits_zscore <- readRDS(cache_zscore)

} else {

  message("Fitting Fig 3 GLMMs (25 guild × variable combinations)...")
  glmm_fits_raw    <- list()
  glmm_fits_zscore <- list()

  for (spec in panel_specs) {
    for (dd in diet_groups) {
      key <- paste0(spec$key, "::", dd)

      glmm_fits_raw[[key]] <- if (LOG_TRANSFORM && spec$use_log) {
        fit_diet_log(spec$key, dd)
      } else {
        fit_diet_identity(spec$key, dd)
      }

      glmm_fits_zscore[[key]] <- fit_diet_zscore(spec$key, dd, spec$use_log)
    }
  }

  saveRDS(glmm_fits_raw,    cache_raw)
  saveRDS(glmm_fits_zscore, cache_zscore)
  message("Saved: ", cache_raw, " | ", cache_zscore)
}


# =============================================================================
# 9.  YEARLY MEANS (display only — points on figure)
# =============================================================================
# Means computed on the original scale; ggplot's log10 transform handles
# log-axis panels correctly.

yearly_means <- data_long %>%
  filter(Year >= GLMM_START, Year <= GLMM_END) %>%
  group_by(Year, DietGroup) %>%
  summarise(
    Mean_S_diet    = mean(S_diet,    na.rm = TRUE),
    Mean_N_diet    = mean(N_diet,    na.rm = TRUE),
    Mean_Gini_diet = mean(Gini_diet, na.rm = TRUE),
    Mean_BM_diet   = mean(BM_diet,   na.rm = TRUE),
    Mean_E_diet    = mean(E_diet,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(DietGroup = factor(DietGroup, levels = diet_groups))

# Map panel key to column in yearly_means
means_col <- c(
  S_diet    = "Mean_S_diet",
  N_diet    = "Mean_N_diet",
  Gini_diet = "Mean_Gini_diet",
  BM_diet   = "Mean_BM_diet",
  E_diet    = "Mean_E_diet"
)

# Assemble prediction lines with significance flag.
# sig = "sig" when GLMM fixed-effect p < 0.05, "ns" otherwise.
# Used in make_panel() to style lines (solid/dashed) and ribbons (alpha).
pred_lines <- do.call(rbind, lapply(names(glmm_fits_raw), function(key) {
  fit <- glmm_fits_raw[[key]]
  if (is.null(fit)) return(NULL)
  parts        <- strsplit(key, "::")[[1]]
  ln           <- fit$line
  ln$Variable  <- parts[1]
  ln$DietGroup <- parts[2]
  ln$sig       <- ifelse(!is.null(fit$pval) && fit$pval < 0.05, "sig", "ns")
  ln
})) %>%
  mutate(DietGroup = factor(DietGroup, levels = diet_groups))


# =============================================================================
# 10.  PANEL BUILDER
# =============================================================================

make_panel <- function(spec) {

  key        <- spec$key
  is_log     <- LOG_TRANSFORM && spec$use_log
  use_log_ax <- LOG_Y_AXIS && is_log
  mc         <- means_col[key]
  ylab_use   <- if (use_log_ax) spec$ylab_log else spec$ylab_raw

  # Points: yearly means (same style regardless of significance)
  pts <- yearly_means %>%
    rename(y_val = !!sym(mc)) %>%
    filter(!is.na(y_val))
  if (use_log_ax) pts <- pts %>% filter(y_val > 0)

  # GLMM prediction lines
  lns <- pred_lines %>% filter(Variable == key)

  p <- ggplot() +

    geom_point(
      data  = pts,
      aes(x = Year, y = y_val, colour = DietGroup),
      size  = 1.4, alpha = 0.45,
      show.legend = TRUE
    ) +

    # Ribbon: more transparent for non-significant trends
    geom_ribbon(
      data  = lns,
      aes(x = Year, ymin = lower_ci, ymax = upper_ci,
          fill = DietGroup, alpha = sig),
      show.legend = FALSE
    ) +

    # Line: solid for significant, dashed for non-significant
    geom_line(
      data      = lns,
      aes(x = Year, y = pred, colour = DietGroup, linetype = sig),
      linewidth = 0.95,
      show.legend = FALSE
    ) +

    scale_colour_manual(values = diet_cols, labels = diet_labels,
                        name = "Dietary guild") +
    scale_fill_manual(  values = diet_cols, labels = diet_labels,
                        name = "Dietary guild") +
    scale_linetype_manual(
      values = c("sig" = "solid", "ns" = "dashed"),
      guide  = "none"
    ) +
    scale_alpha_manual(
      values = c("sig" = 0.13, "ns" = 0.04),
      guide  = "none"
    ) +

    labs(y = ylab_use, x = NULL, tag = spec$tag) +
    guides(colour = guide_legend(
      override.aes = list(size = 3, alpha = 1, linewidth = 1)
    ))

  if (use_log_ax) {
    p <- p + scale_y_log10(labels = label_comma())
  }

  p
}


# =============================================================================
# 11.  BUILD AND SAVE FIGURE
# =============================================================================

# Create output directory if it does not exist
dir.create("figures", showWarnings = FALSE)

plots <- lapply(panel_specs, make_panel)

# Add "Year" x-label to bottom row only (panels D and E)
plots[[4]] <- plots[[4]] + labs(x = "Year")
plots[[5]] <- plots[[5]] + labs(x = "Year")

log_y_tag   <- if (LOG_Y_AXIS && LOG_TRANSFORM) "_logy" else ""
bool_suffix <- paste0(link_tag, log_y_tag)

# guides = "collect" pulls all identical colour legends into one shared legend
fig3 <- wrap_plots(plots, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

base_name <- file.path("figures", paste0("figure_3", bool_suffix))

ggsave(paste0(base_name, ".pdf"), fig3,
       width = 11, height = 13, device = cairo_pdf)

ggsave(paste0(base_name, ".png"), fig3,
       width = 11, height = 13, dpi = 300)

# =============================================================================
# 12.  STATS TABLE  (Table S2)
# =============================================================================

fmt_pval_csv <- function(p) formatC(p, format = "e", digits = 2)

stats_rows <- lapply(names(glmm_fits_raw), function(key) {

  fr  <- glmm_fits_raw[[key]]
  fz  <- glmm_fits_zscore[[key]]
  if (is.null(fr)) return(NULL)

  parts    <- strsplit(key, "::")[[1]]
  var_key  <- parts[1]
  diet_key <- parts[2]
  is_log   <- !is.null(fr$link) && fr$link == "log"

  pct_per_yr <- if (is_log) round((exp(fr$slope) - 1) * 100, 3) else NA_real_

  data.frame(
    Variable          = var_key,
    DietGroup         = diet_key,
    Start_Year        = GLMM_START,
    End_Year          = GLMM_END,
    Scale             = if (is_log) "log(y)" else "raw",
    Intercept         = round(fr$intercept,      4),
    Intercept_SE      = round(fr$intercept_se,   4),
    Intercept_year    = round(grand_mean_year,    1),
    Slope             = round(fr$slope,           6),
    Slope_SE          = round(fr$se,              6),
    Pct_change_per_yr = pct_per_yr,
    t_value           = round(fr$t,               3),
    p_value           = fmt_pval_csv(fr$pval),
    AIC               = round(fr$AIC,             1),
    R2_marginal       = round(fr$R2m,             4),
    R2_conditional    = round(fr$R2c,             4),
    Beta_SD_per_yr    = if (!is.null(fz)) round(fz$slope, 6) else NA_real_,
    Beta_SE           = if (!is.null(fz)) round(fz$se,    6) else NA_real_,
    p_Beta            = if (!is.null(fz)) fmt_pval_csv(fz$pval) else NA_character_,
    stringsAsFactors  = FALSE
  )
})

glmm_stats_fig3 <- do.call(rbind, stats_rows)

glmm_stats_fig3$Variable <- dplyr::recode(
  glmm_stats_fig3$Variable,
  S_diet    = "Species richness",
  N_diet    = "Individuals (N)",
  Gini_diet = "Gini coefficient",
  BM_diet   = "Mean body mass (g)",
  E_diet    = "Energy demand (J)"
)
glmm_stats_fig3$DietGroup <- dplyr::recode(glmm_stats_fig3$DietGroup,
                                            !!!diet_labels)

var_order <- c("Species richness", "Individuals (N)", "Gini coefficient",
               "Mean body mass (g)", "Energy demand (J)")

glmm_stats_fig3 <- glmm_stats_fig3 %>%
  mutate(Variable = factor(Variable, levels = var_order)) %>%
  arrange(Variable, DietGroup) %>%
  mutate(Variable = as.character(Variable))

out_csv <- paste0("figure_3_GLMM_stats", bool_suffix, ".csv")
write.csv(glmm_stats_fig3, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
print(glmm_stats_fig3, row.names = FALSE)
