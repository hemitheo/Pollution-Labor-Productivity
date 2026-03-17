# ==============================================================================
# Mining-Region Heterogeneity Analysis
# ==============================================================================
# Referee 2 asked whether mining-heavy regions disproportionately drive results.
# We re-run the main IV specification:
#   (a) Full sample (baseline)
#   (b) Excluding Antofagasta and Atacama (mining regions)
#   (c) Mining regions only
#   (d) With mining-region interaction
# ==============================================================================

library(readstata13)
library(tidyverse)
library(fixest)

# Load data
dat <- read.dta13("../Cleaned_AUG2022.dta", fromEncoding = "macroman")
dat$city <- as.character(dat$city)
dat$region <- as.character(dat$region)
dat$year <- as.numeric(as.character(dat$year))

# Fix encoding
dat$city <- str_replace(dat$city, "Combarbal<e1><e1>", "Combarbal\u00e1")
dat$city <- str_replace(dat$city, "Combarbal\u00b7",   "Combarbal\u00e1")
dat$city <- str_replace(dat$city, "Conc<f3>n",         "Conc\u00f3n")
dat$city <- str_replace(dat$city, "Conc\u00dbn",       "Conc\u00f3n")
dat$city <- str_replace(dat$city, "Copiap<f3>",        "Copiap\u00f3")
dat$city <- str_replace(dat$city, "Copiap\u00db",      "Copiap\u00f3")
dat$city <- str_replace(dat$city, "Puchuncav<ed>",     "Puchuncav\u00ed")
dat$city <- str_replace(dat$city, "Puchuncav\u00cc",   "Puchuncav\u00ed")

# Identify mining regions
dat$mining_region <- as.integer(grepl("Antofagasta|Atacama", dat$region))

cat("====== Sample Composition ======\n")
cat("Mining regions (Antofagasta + Atacama):", sum(dat$mining_region), "\n")
cat("Non-mining regions:", sum(1 - dat$mining_region), "\n")
cat("\nTreated municipalities by mining status:\n")
print(table(Mining = dat$mining_region, Treated = dat$dummy))

# ------------------------------------------------------------------------------
# (a) Full sample — baseline IV
# ------------------------------------------------------------------------------
cat("\n====== (a) Full Sample (Baseline) ======\n")
iv_full <- feols(yl ~ 1 | year + prov + prov[year] + sector4[year] | lpm ~ dummy, data = dat)
cat("Coef:", round(coef(iv_full), 4), "\n")
cat("SE:", round(sqrt(vcov(iv_full)[1,1]), 4), "\n")
fstat_full <- fitstat(iv_full, "ivf")$ivf$stat
cat("First-stage F:", round(fstat_full, 1), "\n")
cat("N:", nobs(iv_full), "\n")

# ------------------------------------------------------------------------------
# (b) Excluding mining regions
# ------------------------------------------------------------------------------
cat("\n====== (b) Excluding Mining Regions ======\n")
dat_nonmining <- dat[dat$mining_region == 0, ]
iv_nonmining <- feols(yl ~ 1 | year + prov + prov[year] + sector4[year] | lpm ~ dummy,
                       data = dat_nonmining)
cat("Coef:", round(coef(iv_nonmining), 4), "\n")
cat("SE:", round(sqrt(vcov(iv_nonmining)[1,1]), 4), "\n")
fstat_nm <- fitstat(iv_nonmining, "ivf")$ivf$stat
cat("First-stage F:", round(fstat_nm, 1), "\n")
cat("N:", nobs(iv_nonmining), "\n")

# ------------------------------------------------------------------------------
# (c) Mining regions only
# ------------------------------------------------------------------------------
cat("\n====== (c) Mining Regions Only ======\n")
dat_mining <- dat[dat$mining_region == 1, ]
tryCatch({
  iv_mining <- feols(yl ~ 1 | year + prov + prov[year] + sector4[year] | lpm ~ dummy,
                     data = dat_mining)
  cat("Coef:", round(coef(iv_mining), 4), "\n")
  cat("SE:", round(sqrt(vcov(iv_mining)[1,1]), 4), "\n")
  fstat_m <- fitstat(iv_mining, "ivf")$ivf$stat
  cat("First-stage F:", round(fstat_m, 1), "\n")
  cat("N:", nobs(iv_mining), "\n")
}, error = function(e) {
  cat("Error (likely too few clusters):", e$message, "\n")
  # Try simpler FE
  cat("\nRetrying with simpler FE (year + prov + sector4[year]):\n")
  iv_mining2 <- feols(yl ~ 1 | year + prov + sector4[year] | lpm ~ dummy,
                      data = dat_mining)
  cat("Coef:", round(coef(iv_mining2), 4), "\n")
  cat("SE:", round(sqrt(vcov(iv_mining2)[1,1]), 4), "\n")
  fstat_m2 <- fitstat(iv_mining2, "ivf")$ivf$stat
  cat("First-stage F:", round(fstat_m2, 1), "\n")
  cat("N:", nobs(iv_mining2), "\n")
})

# ------------------------------------------------------------------------------
# (d) Interaction: mining region × pollution
# ------------------------------------------------------------------------------
cat("\n====== (d) Mining Interaction ======\n")
# IV with mining interaction: instrument pollution and pollution×mining
# with dummy and dummy×mining
iv_interact <- feols(yl ~ 1 | year + prov + prov[year] + sector4[year] |
                       lpm + lpm:mining_region ~ dummy + dummy:mining_region,
                     data = dat)
cat("Main effect (lpm):\n")
summary_interact <- summary(iv_interact)
print(coeftable(iv_interact))
fstat_int <- fitstat(iv_interact, "ivf")$ivf$stat
cat("First-stage F:", round(fstat_int, 1), "\n")

# ------------------------------------------------------------------------------
# Summary table for LaTeX
# ------------------------------------------------------------------------------
cat("\n\n====== LaTeX Summary ======\n")
cat("Ready to insert into main.tex appendix.\n")
cat("Full sample: coef =", round(coef(iv_full), 3),
    ", SE =", round(sqrt(vcov(iv_full)[1,1]), 3),
    ", F =", round(fstat_full, 1),
    ", N =", nobs(iv_full), "\n")
cat("Non-mining: coef =", round(coef(iv_nonmining), 3),
    ", SE =", round(sqrt(vcov(iv_nonmining)[1,1]), 3),
    ", F =", round(fstat_nm, 1),
    ", N =", nobs(iv_nonmining), "\n")

cat("\nMining heterogeneity script complete.\n")
