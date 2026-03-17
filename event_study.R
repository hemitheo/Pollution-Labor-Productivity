# ==============================================================================
# Event Study: Coal/Diesel Power Plant Openings and Labor Productivity
# ==============================================================================
# This script estimates event-study regressions around the commissioning dates
# of coal/diesel power plants in Chilean municipalities. Two outcomes are
# examined: (1) log PM2.5 pollution (first stage) and (2) log output per
# worker (reduced form).
# ==============================================================================

library(readstata13)
library(tidyverse)
library(fixest)

# ------------------------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------------------------

# Adjust this path as needed
dat <- read.dta13("../Cleaned_AUG2022.dta", fromEncoding = "macroman")
dat$city <- as.character(dat$city)

# Fix encoding issues in city names (same corrections as MergeClean_AUG2022.R)
dat$city <- str_replace(dat$city, "Combarbal<e1><e1>", "Combarbal\u00e1")
dat$city <- str_replace(dat$city, "Combarbal\u00b7",   "Combarbal\u00e1")
dat$city <- str_replace(dat$city, "Conc<f3>n",         "Conc\u00f3n")
dat$city <- str_replace(dat$city, "Conc\u00dbn",       "Conc\u00f3n")
dat$city <- str_replace(dat$city, "Copiap<f3>",        "Copiap\u00f3")
dat$city <- str_replace(dat$city, "Copiap\u00db",      "Copiap\u00f3")
dat$city <- str_replace(dat$city, "Puchuncav<ed>",     "Puchuncav\u00ed")
dat$city <- str_replace(dat$city, "Puchuncav\u00cc",   "Puchuncav\u00ed")

dat$year <- as.numeric(as.character(dat$year))

# ------------------------------------------------------------------------------
# 2. Define event (commissioning) years for the 16 plant municipalities
# ------------------------------------------------------------------------------

event_years <- tribble(
  ~city,               ~event_year,
  "Antofagasta",       2011,
  "Cabrero",           2007,
  "Combarbal\u00e1",   2010,
  "Conc\u00f3n",       2009,
  "Copiap\u00f3",      2007,
  "Coquimbo",          2010,
  "Diego de Almagro",  2011,
  "Huasco",            2006,
  "Llaillay",          2009,
  "Los Vilos",         2010,
  "Mejillones",        2008,
  "Puchuncav\u00ed",   2006,
  "Puerto Montt",      2013,
  "Quintero",          2006,
  "Tocopilla",         2006,
  "Vallenar",          2011
)

# Merge event years onto data
dat <- left_join(dat, event_years, by = "city")

# Treatment indicator: 1 if municipality ever has a power plant
dat$treated <- as.integer(!is.na(dat$event_year))

# Event time: years relative to plant commissioning
# For never-treated units, event_time is NA (they serve as controls)
dat$event_time <- ifelse(dat$treated == 1, dat$year - dat$event_year, NA_real_)

# ------------------------------------------------------------------------------
# 3. Create binned event-time variable
# ------------------------------------------------------------------------------
# We keep leads t-3 and earlier (binned at -3) through lags t+3 and later
# (binned at +3). The reference (omitted) period is t-1.

dat$event_time_binned <- case_when(
  is.na(dat$event_time)  ~ 0,          # controls: assign 0 (interaction with treated=0 makes it irrelevant)
  dat$event_time <= -4    ~ -4,        # bin distant leads
  dat$event_time >= 4     ~ 4,         # bin distant lags
  TRUE                    ~ dat$event_time
)

# For fixest's sunab() we need a cohort variable for treated units
# and a large value (e.g., 10000) for never-treated units
dat$cohort <- ifelse(dat$treated == 1, dat$event_year, 10000)

# ------------------------------------------------------------------------------
# 4. Event study regressions
# ------------------------------------------------------------------------------

# --- 4a. Sun & Abraham (2021) estimator via sunab() --------------------------
# This is robust to heterogeneous treatment effects across cohorts.
# Reference period is automatically set. ATT is identified under parallel
# trends conditional on fixed effects.

cat("\n====== First Stage: log PM2.5 (sunab) ======\n")
es_pm_sunab <- feols(
  lpm ~ sunab(cohort, year) | year + prov + prov[year] + sector4[year],
  data    = dat,
  cluster = ~city
)
summary(es_pm_sunab)

cat("\n====== Reduced Form: log productivity (sunab) ======\n")
es_yl_sunab <- feols(
  yl ~ sunab(cohort, year) | year + prov + prov[year] + sector4[year],
  data    = dat,
  cluster = ~city
)
summary(es_yl_sunab)

# --- 4b. Manual event-time dummies using i() ---------------------------------
# For treated observations, create event-time indicators interacted with
# treatment; for controls, these are zero by construction.
# We use i(event_time_binned, treated, ref = -1) to omit t-1.

cat("\n====== First Stage: log PM2.5 (manual) ======\n")
es_pm_manual <- feols(
  lpm ~ i(event_time_binned, treated, ref = -1) | year + prov + prov[year] + sector4[year],
  data    = dat,
  cluster = ~city
)
summary(es_pm_manual)

cat("\n====== Reduced Form: log productivity (manual) ======\n")
es_yl_manual <- feols(
  yl ~ i(event_time_binned, treated, ref = -1) | year + prov + prov[year] + sector4[year],
  data    = dat,
  cluster = ~city
)
summary(es_yl_manual)

# ------------------------------------------------------------------------------
# 5. Event-study plots
# ------------------------------------------------------------------------------

# Helper function to extract coefficients and CIs from a fixest model
extract_es_coefs <- function(model, label = "") {
  ct <- as.data.frame(coeftable(model))
  ct$coef_name <- rownames(ct)

  # Extract event time from coefficient names
  # Pattern: the number (possibly negative) in the coefficient name
  ct$event_time <- as.numeric(str_extract(ct$coef_name, "-?[0-9]+"))

  ct <- ct %>%
    filter(!is.na(event_time)) %>%
    transmute(
      event_time = event_time,
      estimate   = Estimate,
      se         = `Std. Error`,
      ci_lower   = estimate - 1.96 * se,
      ci_upper   = estimate + 1.96 * se,
      label      = label
    )

  # Add the omitted reference period (t = -1) with zero effect
  ref_row <- tibble(
    event_time = -1, estimate = 0, se = 0,
    ci_lower = 0, ci_upper = 0, label = label
  )
  ct <- bind_rows(ct, ref_row) %>% arrange(event_time)

  return(ct)
}

# --- Plot function ------------------------------------------------------------

plot_event_study <- function(coef_df, title, ylab, filename) {

  p <- ggplot(coef_df, aes(x = event_time, y = estimate)) +
    # Confidence interval
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.15, linewidth = 0.6, color = "grey40"
    ) +
    # Point estimates
    geom_point(shape = 16, size = 2.5, color = "steelblue") +
    # Reference lines
    geom_hline(yintercept = 0, color = "firebrick", linetype = "solid") +
    geom_vline(xintercept = -0.5, color = "grey30", linetype = "dashed") +
    # Labels
    scale_x_continuous(breaks = unique(coef_df$event_time)) +
    labs(
      title = title,
      x     = "Years relative to power plant commissioning",
      y     = ylab
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13)
    )

  # Save
  ggsave(filename, plot = p, width = 8, height = 5, device = "pdf")
  cat("Saved:", filename, "\n")

  return(p)
}

# --- Extract coefficients and plot --------------------------------------------

# Use the manual event-time specification for cleaner coefficient labels
coefs_pm <- extract_es_coefs(es_pm_manual, label = "PM2.5")
coefs_yl <- extract_es_coefs(es_yl_manual, label = "Productivity")

# Create output directory path (relative to Code/)
chart_dir <- "../Pollution_Labor_Productivity/Charts"
if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE)

# First stage: pollution
p_pm <- plot_event_study(
  coefs_pm,
  title    = "First Stage: Effect of Power Plant Opening on log PM2.5",
  ylab     = "Coefficient (95% CI)",
  filename = file.path(chart_dir, "event_study_pollution.pdf")
)

# Reduced form: productivity
p_yl <- plot_event_study(
  coefs_yl,
  title    = "Reduced Form: Effect of Power Plant Opening on log Productivity",
  ylab     = "Coefficient (95% CI)",
  filename = file.path(chart_dir, "event_study_productivity.pdf")
)

# --- Combined plot (both outcomes) --------------------------------------------

coefs_both <- bind_rows(coefs_pm, coefs_yl)

p_combined <- ggplot(coefs_both, aes(x = event_time, y = estimate)) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.15, linewidth = 0.6, color = "grey40"
  ) +
  geom_point(shape = 16, size = 2.5, color = "steelblue") +
  geom_hline(yintercept = 0, color = "firebrick") +
  geom_vline(xintercept = -0.5, color = "grey30", linetype = "dashed") +
  facet_wrap(~label, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = unique(coefs_both$event_time)) +
  labs(
    title = "Event Study: Power Plant Commissioning",
    x     = "Years relative to power plant commissioning",
    y     = "Coefficient (95% CI)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 13),
    strip.text       = element_text(face = "bold")
  )

ggsave(
  file.path(chart_dir, "event_study.pdf"),
  plot = p_combined, width = 8, height = 8, device = "pdf"
)
cat("Saved combined plot:", file.path(chart_dir, "event_study.pdf"), "\n")

# --- Also produce sunab() versions -------------------------------------------

coefs_pm_sa <- extract_es_coefs(es_pm_sunab, label = "PM2.5 (Sun & Abraham)")
coefs_yl_sa <- extract_es_coefs(es_yl_sunab, label = "Productivity (Sun & Abraham)")

p_pm_sa <- plot_event_study(
  coefs_pm_sa,
  title    = "First Stage (Sun & Abraham): Power Plant Opening on log PM2.5",
  ylab     = "Coefficient (95% CI)",
  filename = file.path(chart_dir, "event_study_pollution_sunab.pdf")
)

p_yl_sa <- plot_event_study(
  coefs_yl_sa,
  title    = "Reduced Form (Sun & Abraham): Power Plant Opening on log Productivity",
  ylab     = "Coefficient (95% CI)",
  filename = file.path(chart_dir, "event_study_productivity_sunab.pdf")
)

# ------------------------------------------------------------------------------
# 6. Summary tables
# ------------------------------------------------------------------------------

cat("\n\n====== Event Study Summary ======\n")
cat("Number of treated municipalities:", nrow(event_years), "\n")
cat("Cohort (commissioning year) distribution:\n")
print(table(event_years$event_year))
cat("\nTotal observations:", nrow(dat), "\n")
cat("Treated observations:", sum(dat$treated), "\n")
cat("Control observations:", sum(1 - dat$treated), "\n")

# Display regression tables side by side
cat("\n====== Manual Event-Time Dummies ======\n")
etable(es_pm_manual, es_yl_manual,
       headers = c("log PM2.5", "log Productivity"),
       fitstat = ~ n + r2 + wr2)

cat("\n====== Sun & Abraham (2021) ======\n")
etable(es_pm_sunab, es_yl_sunab,
       headers = c("log PM2.5", "log Productivity"),
       fitstat = ~ n + r2 + wr2)

cat("\nEvent study script complete.\n")
