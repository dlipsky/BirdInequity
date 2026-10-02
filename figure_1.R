### ============================================================================
### Figure 1 - Temporal Trends in BBS Route-Level Metrics
### Authors: Lina K. Mühlbauer & Daria Lipsky
### ============================================================================


# =============================================================================
# 1.  SETUP
# =============================================================================

rm(list = ls())

library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(lme4)
library(MuMIn)
library(ragg)

setwd("/path/to/BirdInequity")

# -- KEY SWITCHES -------------------------------------------------------------
REFIT_GLMM     <- FALSE
LOG_TRANSFORM  <- TRUE
SHOW_PCT_SLOPE <- TRUE
LOG_YAXIS      <- TRUE

# -- TIME WINDOWS -------------------------------------------------------------
FULL_START   <- 1966
RECENT_START <- 1994
RECENT_END   <- 2023

# =============================================================================
# 2.  LOAD DATA
# =============================================================================

results <- read.csv("data_2025/RouteMetrics_2025_diet.csv")

weather <- read.csv("data_2025/Weather.csv") %>%
  mutate(Route_unique = paste(CountryNum, StateNum, Route, RPID, sep = "_")) %>%
  rename(Observer = ObsN)


# =============================================================================
# 3.  BUILD MODELLING DATASET
# =============================================================================

data <- results %>%
  mutate(Route_unique = as.character(Route_unique)) %>%
  left_join(
    weather %>% select(Route_unique, Year, Observer),
    by = c("Route_unique", "Year")
  ) %>%
  mutate(
    Observer     = factor(Observer),
    Route_unique = factor(Route_unique)
  )

# =============================================================================
# 4.  PANEL SPECS
# =============================================================================

panel_specs <- list(
  list(key = "S",          raw_yvar = "S",                 use_log = FALSE,
       ylab_raw = "Species richness"),
  list(key = "N",          raw_yvar = "N",                 use_log = TRUE,
       ylab_raw = "Number of individuals"),
  list(key = "Gini",       raw_yvar = "GINI",              use_log = FALSE,
       ylab_raw = "Gini coefficient"),
  list(key = "bodymass",   raw_yvar = "Ave_bodymass",      use_log = TRUE,
       ylab_raw = "Mean body mass (g)"),
  list(key = "trophiclev", raw_yvar = "Ave_trophic_level", use_log = FALSE,
       ylab_raw = "Mean trophic level"),
  list(key = "E_diet",     raw_yvar = "E_total_diet",      use_log = TRUE,
       ylab_raw = "Energy demand (J)"))

# =============================================================================
# 5.  COLOURS
# =============================================================================

panel_cols <- c(
  S          = "#4E9AC0",
  N          = "#5ba8a0",
  Gini       = "#0d4f4a",
  bodymass   = "#9B6BB5",
  trophiclev = "#C2506E",
  E_diet     = "#E07B3A")

# =============================================================================
# 6.  FITTING FUNCTIONS
# =============================================================================

bobyqa_ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# -- Identity-link lmer -------------------------------------------------------
fit_lmer <- function(yvar, fit_data) {
  message("    lmer (identity): ", yvar)
  window_mean_year <- mean(fit_data$Year, na.rm = TRUE)
  fit_data <- fit_data %>%
    filter(!is.na(.data[[yvar]])) %>%
    mutate(Year_c = Year - window_mean_year)
  fml <- as.formula(
    paste0(yvar, " ~ Year_c + (1 + Year_c | Route_unique) + (1 | Observer)")
  )
  m <- tryCatch(
    lmer(fml, data = fit_data, REML = FALSE, control = bobyqa_ctrl),
    error   = function(e) { message("    FAILED: ", e$message); NULL },
    warning = function(w) suppressWarnings(
      lmer(fml, data = fit_data, REML = FALSE, control = bobyqa_ctrl)
    )
  )
  if (is.null(m)) return(NULL)
  build_fit_obj(m, fit_data, window_mean_year, link = "identity")
}

# -- Log-transform lmer -------------------------------------------------------
fit_lmer_logtrans <- function(yvar, fit_data) {
  message("    lmer (log-transform): ", yvar)
  log_yvar <- paste0("log_", yvar)
  fit_data <- fit_data %>%
    filter(!is.na(.data[[yvar]]), .data[[yvar]] > 0) %>%
    mutate(!!log_yvar := log(.data[[yvar]]))
  window_mean_year <- mean(fit_data$Year, na.rm = TRUE)
  fit_data <- fit_data %>% mutate(Year_c = Year - window_mean_year)
  fml <- as.formula(
    paste0(log_yvar, " ~ Year_c + (1 + Year_c | Route_unique) + (1 | Observer)")
  )
  m <- tryCatch(
    lmer(fml, data = fit_data, REML = FALSE, control = bobyqa_ctrl),
    error   = function(e) { message("    FAILED: ", e$message); NULL },
    warning = function(w) suppressWarnings(
      lmer(fml, data = fit_data, REML = FALSE, control = bobyqa_ctrl)
    )
  )
  if (is.null(m)) return(NULL)
  build_fit_obj(m, fit_data, window_mean_year, link = "log")
}

# -- Shared: extract predictions, stats, and BLUP annual means ----------------
build_fit_obj <- function(m, fit_data, window_mean_year, link) {

  # Fixed-effect predictions (the plotted line)
  newdata <- data.frame(
    Year   = seq(min(fit_data$Year), max(fit_data$Year)),
    Year_c = seq(min(fit_data$Year), max(fit_data$Year)) - window_mean_year
  )
  mm     <- model.matrix(~ Year_c, data = newdata)
  eta    <- as.vector(mm %*% fixef(m))
  se_eta <- sqrt(diag(mm %*% tcrossprod(vcov(m), mm)))

  if (link == "log") {
    newdata$pred_glmm <- exp(eta)
    newdata$lower_ci  <- exp(eta - 1.96 * se_eta)
    newdata$upper_ci  <- exp(eta + 1.96 * se_eta)
  } else {
    newdata$pred_glmm <- eta
    newdata$lower_ci  <- eta - 1.96 * se_eta
    newdata$upper_ci  <- eta + 1.96 * se_eta
  }

  # BLUP-fitted annual means (used for R² computation)
  blup_annual <- data.frame(
    Year   = fit_data$Year,
    fitted = fitted(m)
  ) %>%
    group_by(Year) %>%
    summarise(mean_fitted = mean(fitted, na.rm = TRUE), .groups = "drop")

  # Model coefficients and goodness of fit
  coefs <- summary(m)$coefficients
  tcol  <- ifelse("t value" %in% colnames(coefs), "t value", "z value")

  r2 <- tryCatch(
    MuMIn::r.squaredGLMM(m),
    error   = function(e) matrix(c(NA_real_, NA_real_), nrow = 1,
                                 dimnames = list(NULL, c("R2m", "R2c"))),
    warning = function(w) suppressWarnings(MuMIn::r.squaredGLMM(m)))

  list(
    line              = newdata,
    blup_annual_means = blup_annual,
    slope             = coefs["Year_c",      "Estimate"],
    se                = coefs["Year_c",      "Std. Error"],
    t                 = coefs["Year_c",      tcol],
    pval              = 2 * pnorm(abs(coefs["Year_c", tcol]), lower.tail = FALSE),
    AIC               = AIC(m),
    intercept         = fixef(m)["(Intercept)"],
    intercept_se      = coefs["(Intercept)", "Std. Error"],
    intercept_year    = window_mean_year,
    link              = link,
    R2m               = r2[1, "R2m"],
    R2c               = r2[1, "R2c"])
}


# =============================================================================
# 6b.  R² FROM BLUP-FITTED ANNUAL MEANS
# =============================================================================

compute_r2_annual <- function(spec, fit, plot_data = NULL) {
  if (is.null(fit))                   return(NA_real_)
  if (is.null(fit$blup_annual_means)) {
    warning("fit$blup_annual_means is missing — cache needs regeneration ",
            "(run once with REFIT_GLMM = TRUE).")
    return(NA_real_)
  }
  summary(lm(mean_fitted ~ Year, data = fit$blup_annual_means))$r.squared
}


# =============================================================================
# 7.  FIGURE CONFIGURATIONS & CACHE
# =============================================================================
figure_configs <- list(
  list(label    = "full",
       year_min = FULL_START,   year_max = RECENT_END,
       width    = 10,           height   = 11),
  list(label    = "post1994",
       year_min = RECENT_START, year_max = RECENT_END,
       width    = 10,           height   = 11))

link_tag      <- if (LOG_TRANSFORM) "_logtransform" else "_idlink"
all_glmm_fits <- list()
all_glmm_beta <- list()

for (cfg in figure_configs) {

  cache_raw    <- paste0("glmm_cache_from", cfg$year_min, "_raw",    link_tag, ".rds")
  cache_zscore <- paste0("glmm_cache_from", cfg$year_min, "_zscore", link_tag, ".rds")

  if (!REFIT_GLMM && file.exists(cache_raw) && file.exists(cache_zscore)) {
    message("Loading cached GLMMs [", cfg$label, "]: ", cache_raw)
    all_glmm_fits[[cfg$label]] <- readRDS(cache_raw)
    all_glmm_beta[[cfg$label]] <- readRDS(cache_zscore)

  } else {
    message("Fitting GLMMs for window: ", cfg$year_min, "\u2013", cfg$year_max)
    fit_data <- data %>% filter(Year >= cfg$year_min, Year <= cfg$year_max)

    # Raw-scale (or log-transformed) models
    fits_raw <- lapply(panel_specs, function(spec) {
      if (LOG_TRANSFORM && spec$use_log) {
        fit_lmer_logtrans(spec$raw_yvar, fit_data)
      } else {
        fit_lmer(spec$raw_yvar, fit_data)
      }
    })
    names(fits_raw) <- sapply(panel_specs, `[[`, "key")

    # Z-scored models (for standardised slopes in stats table)
    fit_data_z <- fit_data
    for (spec in panel_specs) {
      rv <- spec$raw_yvar
      if (LOG_TRANSFORM && spec$use_log) {
        lv <- log(fit_data[[rv]])
        mn <- mean(lv, na.rm = TRUE); s <- sd(lv, na.rm = TRUE)
        fit_data_z[[paste0(rv, "_z")]] <- (lv - mn) / s
      } else {
        mn <- mean(fit_data[[rv]], na.rm = TRUE)
        s  <- sd(fit_data[[rv]],  na.rm = TRUE)
        fit_data_z[[paste0(rv, "_z")]] <- (fit_data[[rv]] - mn) / s
      }
    }

    fits_z <- lapply(panel_specs, function(spec) {
      fit_lmer(paste0(spec$raw_yvar, "_z"), fit_data_z)
    })
    names(fits_z) <- sapply(panel_specs, `[[`, "key")

    saveRDS(fits_raw, cache_raw)
    saveRDS(fits_z,   cache_zscore)
    message("  Saved: ", cache_raw, " | ", cache_zscore)
    all_glmm_fits[[cfg$label]] <- fits_raw
    all_glmm_beta[[cfg$label]] <- fits_z
  }
}


# =============================================================================
# 8.  PLOT THEME
# =============================================================================

pub_theme <- theme_classic(base_size = 12) +
  theme(
    axis.title       = element_text(size = 15),
    axis.text        = element_text(size = 13, colour = "black"),
    axis.text.x      = element_text(size = 12),
    plot.tag         = element_text(size = 13, face = "bold"),
    strip.background = element_blank()  )
theme_set(pub_theme)


# =============================================================================
# 9.  PANEL-BUILDING FUNCTIONS
# =============================================================================

fmt_pval <- function(p) formatC(p, format = "e", digits = 2)

# Annotation: R² + significance marker
# Significance uses the GLMM fixed-effect p-value for the Year term.
make_label <- function(r2_annual, pval) {
  sig <- if (is.na(pval) || pval >= 0.05) "(ns)"
         else if (pval < 0.001)            "***"
         else if (pval < 0.01)             "**"
         else                              "*"
  r2_str <- if (!is.na(r2_annual)) formatC(r2_annual, format = "f", digits = 2)
             else "NA"
  paste0("italic(R)^2~'= ", r2_str, " ", sig, "'")
}

make_panel <- function(spec, fits_raw, fits_beta, plot_data, year_range) {

  key          <- spec$key
  col          <- panel_cols[key]
  fit          <- fits_raw[[key]]
  rv           <- spec$raw_yvar
  is_log_panel <- LOG_TRANSFORM && spec$use_log
  use_log_axis <- is_log_panel && LOG_YAXIS

  pd_nona <- plot_data %>% filter(!is.na(.data[[rv]]), .data[[rv]] > 0)

  # Y limits: envelope of per-year IQRs + 5% padding
  iqr_range <- pd_nona %>%
    group_by(Year) %>%
    summarise(
      q25 = quantile(.data[[rv]], 0.25, na.rm = TRUE),
      q75 = quantile(.data[[rv]], 0.75, na.rm = TRUE),
      .groups = "drop")
  lo <- min(iqr_range$q25, na.rm = TRUE)
  hi <- max(iqr_range$q75, na.rm = TRUE)

  if (use_log_axis) {
    ylims <- c(lo / 1.05, hi * 1.05)
  } else {
    pad   <- 0.05 * (hi - lo)
    ylims <- c(lo - pad, hi + pad)
  }

  # Base boxplots (whiskers suppressed: coef = 0)
  p <- ggplot(pd_nona, aes(x = Year, y = .data[[rv]], group = Year)) +
    geom_boxplot(
      colour        = col,
      fill          = col,
      alpha         = 0.20,
      linewidth     = 0.25,
      coef          = 0,
      outlier.shape = NA,
      width         = 0.55
    ) +
    labs(y = spec$ylab_raw, x = NULL)

  # Y-axis scale
  if (use_log_axis) {
    p <- p +
      coord_trans(y = "log10", ylim = ylims) +
      scale_y_continuous(
        labels = label_comma(accuracy = 1),
        breaks = scales::log_breaks(n = 6)(ylims))
  } else {
    p <- p + coord_cartesian(ylim = ylims)
  }

  # GLMM trend line, CI ribbon, and R² annotation
  if (!is.null(fit)) {

    line_data <- fit$line %>% filter(Year >= year_range[1], Year <= year_range[2])

    ann_x <- year_range[2] - 2
    ann_y <- if (use_log_axis) ylims[2] * (ylims[1] / ylims[2])^0.01
             else ylims[2] - 0.01 * diff(ylims)

    r2_annual <- compute_r2_annual(spec, fit)
    lbl       <- make_label(r2_annual = r2_annual, pval = fit$pval)

    p <- p +
      geom_ribbon(
        data = line_data,
        aes(x = Year, ymin = lower_ci, ymax = upper_ci),
        fill = col, alpha = 0.35, inherit.aes = FALSE
      ) +
      geom_line(
        data = line_data,
        aes(x = Year, y = pred_glmm),
        colour = col, linewidth = 0.9, inherit.aes = FALSE
      ) +
      annotate("text",
               x = ann_x, y = ann_y,
               label = lbl, parse = TRUE,
               hjust = 1, vjust = 1,
               size  = 4.0, colour = "grey10", lineheight = 1.1)
  }

  p
}

# =============================================================================
# 10.  BUILD AND SAVE FIGURES
# =============================================================================

bool_suffix <- paste0(
  link_tag,
  if (SHOW_PCT_SLOPE) "_pct"  else "_logslope",
  if (LOG_YAXIS)      "_logy" else "_liny")

for (cfg in figure_configs) {

  message("\nBuilding figure: ", cfg$label)

  fits_raw   <- all_glmm_fits[[cfg$label]]
  fits_beta  <- all_glmm_beta[[cfg$label]]
  plot_data  <- data %>% filter(Year >= cfg$year_min, Year <= cfg$year_max)
  year_range <- c(cfg$year_min, cfg$year_max)

  plots <- lapply(seq_along(panel_specs), function(i) {
    p <- make_panel(panel_specs[[i]], fits_raw, fits_beta, plot_data, year_range)
    if (i %in% c(5, 6)) p <- p + labs(x = "Year")
    p
  })

  fig <- wrap_plots(plots, ncol = 2) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 13, face = "bold"))

  base_name <- file.path("figures", paste0("figure_1_", cfg$label, bool_suffix))

  ggsave(paste0(base_name, ".pdf"), fig,
         width = cfg$width, height = cfg$height, device = cairo_pdf)

  ggsave(paste0(base_name, ".png"), fig,
         width = cfg$width, height = cfg$height, dpi = 300,
         device = ragg::agg_png)

  message("Saved: ", base_name)
}


# =============================================================================
# 11.  STATS TABLE  (Table S1)
# =============================================================================

for (cfg in figure_configs) {

  fits_raw  <- all_glmm_fits[[cfg$label]]
  fits_beta <- all_glmm_beta[[cfg$label]]

  rows <- lapply(names(fits_raw), function(key) {
    fr   <- fits_raw[[key]]
    fz   <- fits_beta[[key]]
    spec <- panel_specs[[which(sapply(panel_specs, `[[`, "key") == key)]]
    if (is.null(fr)) return(NULL)

    is_log    <- !is.null(fr$link) && fr$link == "log"
    pct       <- if (is_log) round((exp(fr$slope) - 1) * 100, 4) else NA_real_
    r2_annual <- compute_r2_annual(spec, fr)

    data.frame(
      Variable          = key,
      Window_start      = cfg$year_min,
      Window_end        = cfg$year_max,
      Scale             = if (is_log) "log(y)" else "raw",
      Intercept         = round(fr$intercept,      4),
      Intercept_SE      = round(fr$intercept_se,   4),
      Intercept_year    = round(fr$intercept_year, 1),
      Slope_log_or_raw  = round(fr$slope,           6),
      Slope_SE          = round(fr$se,              6),
      Pct_change_per_yr = pct,
      t_value           = round(fr$t,               3),
      p_value           = fmt_pval(fr$pval),
      AIC               = round(fr$AIC,             1),
      R2_marginal       = round(fr$R2m,             4),
      R2_conditional    = round(fr$R2c,             4),
      R2_blup_annual    = round(r2_annual,          4),
      Beta_SD_per_yr    = if (!is.null(fz)) round(fz$slope, 6) else NA_real_,
      Beta_SE           = if (!is.null(fz)) round(fz$se,    6) else NA_real_,
      p_Beta            = if (!is.null(fz)) fmt_pval(fz$pval) else NA_character_,
      stringsAsFactors  = FALSE
    )
  })

  stats_tbl <- do.call(rbind, rows)
  stats_tbl$Variable <- dplyr::recode(
    stats_tbl$Variable,
    S          = "Species richness",
    N          = "Individuals (N)",
    Gini       = "Gini coefficient",
    bodymass   = "Body mass",
    trophiclev = "Trophic level",
    E_diet     = "Energy demand"
  )

  out_file <- paste0("figure1_GLMM_stats_", cfg$label, bool_suffix, ".csv")
  write.csv(stats_tbl, out_file, row.names = FALSE, fileEncoding = "UTF-8")
  message("Stats saved: ", out_file)
  print(stats_tbl, row.names = FALSE)
}


# =============================================================================
# 12.  ABUNDANCE LOSS CALCULATION  (for manuscript)
# =============================================================================

fit_N      <- all_glmm_fits[["full"]][["N"]]
year_start <- min(fit_N$line$Year)
year_end   <- max(fit_N$line$Year)

N_start <- fit_N$line$pred_glmm[fit_N$line$Year == year_start]
N_end   <- fit_N$line$pred_glmm[fit_N$line$Year == year_end]

cat(sprintf("\nAbundance loss (model-predicted, fixed-effect line):\n"))
cat(sprintf("  %d:  %.0f birds per route\n", year_start, N_start))
cat(sprintf("  %d:  %.0f birds per route\n", year_end,   N_end))
cat(sprintf("  Loss:  %.0f birds per route (%.1f%% over %d years)\n",
            N_start - N_end,
            (N_end - N_start) / N_start * 100,
            year_end - year_start))
cat(sprintf("  Annual rate:  %.2f%% per year\n",
            (exp(fit_N$slope) - 1) * 100))
