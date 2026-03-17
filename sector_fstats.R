# ==============================================================================
# Sector-Specific First-Stage F-Statistics
# ==============================================================================
# Extracts first-stage F-stats from sector-specific IV regressions
# to fill the [F-STAT] placeholder in main.tex
# ==============================================================================

library(readstata13)
library(tidyverse)
library(fixest)

# Load data
dat <- read.dta13("../Cleaned_AUG2022.dta", fromEncoding = "macroman")
dat$city <- as.character(dat$city)

# Fix encoding
dat$city <- str_replace(dat$city, "Combarbal<e1><e1>", "Combarbal\u00e1")
dat$city <- str_replace(dat$city, "Combarbal\u00b7",   "Combarbal\u00e1")
dat$city <- str_replace(dat$city, "Conc<f3>n",         "Conc\u00f3n")
dat$city <- str_replace(dat$city, "Conc\u00dbn",       "Conc\u00f3n")
dat$city <- str_replace(dat$city, "Copiap<f3>",        "Copiap\u00f3")
dat$city <- str_replace(dat$city, "Copiap\u00db",      "Copiap\u00f3")
dat$city <- str_replace(dat$city, "Puchuncav<ed>",     "Puchuncav\u00ed")
dat$city <- str_replace(dat$city, "Puchuncav\u00cc",   "Puchuncav\u00ed")

dat$year <- as.numeric(as.character(dat$year))

# Rename sectors to match MergeClean_AUG2022.R
dat$sector <- as.character(dat$sector)
dat$sector[dat$sector == "A - Agricultura, ganader\u00eda, silvicultura y pesca"] <- "Agriculture"
dat$sector[grepl("^A - Agricultura", dat$sector)] <- "Agriculture"
dat$sector[dat$sector == "C - Industria manufacturera"] <- "Manufacturing"
dat$sector[grepl("^C - Industria", dat$sector)] <- "Manufacturing"
dat$sector[dat$sector == "F - Construcci\u00f3n"] <- "Construction"
dat$sector[grepl("^F - Construcci", dat$sector)] <- "Construction"
dat$sector[dat$sector == "H - Transporte y almacenamiento"] <- "Transport"
dat$sector[grepl("^H - Transporte", dat$sector)] <- "Transport"
dat$sector[grepl("^G - Comercio", dat$sector)] <- "Wholesale"

sectors <- c("Agriculture", "Manufacturing", "Construction", "Transport", "Wholesale")

cat("\n====== Sector-Specific First-Stage F-Statistics ======\n\n")

results <- data.frame(Sector = character(), F_stat = numeric(), N = integer(),
                      stringsAsFactors = FALSE)

for (s in sectors) {
  dat_s <- dat[dat$sector == s, ]
  n_obs <- nrow(dat_s)

  if (n_obs < 50) {
    cat(sprintf("%-15s: Too few observations (%d)\n", s, n_obs))
    next
  }

  tryCatch({
    iv_model <- feols(yl ~ 1 | year + sector4[year] + prov | lpm ~ dummy, data = dat_s)
    fstat <- fitstat(iv_model, "ivf")
    f_val <- fstat$ivf$stat

    results <- rbind(results, data.frame(Sector = s, F_stat = round(f_val, 1),
                                         N = n_obs, stringsAsFactors = FALSE))
    cat(sprintf("%-15s: F = %6.1f  (N = %d)\n", s, f_val, n_obs))
  }, error = function(e) {
    cat(sprintf("%-15s: Error - %s\n", s, e$message))
  })
}

# Also run the overall first-stage F-stat for reference
cat("\n--- Overall (all sectors) ---\n")
tryCatch({
  iv_all <- feols(yl ~ 1 | year + prov + prov[year] + sector4[year] | lpm ~ dummy, data = dat)
  fstat_all <- fitstat(iv_all, "ivf")
  cat(sprintf("Overall F-stat: %.1f  (N = %d)\n", fstat_all$ivf$stat, nrow(dat)))
}, error = function(e) {
  cat(sprintf("Overall: Error - %s\n", e$message))
})

cat("\n--- LaTeX-ready string ---\n")
latex_str <- paste(
  sapply(1:nrow(results), function(i) {
    sprintf("%s: %.1f", results$Sector[i], results$F_stat[i])
  }),
  collapse = ", "
)
cat(latex_str, "\n")

cat("\nSector F-stat script complete.\n")
