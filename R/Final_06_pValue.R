# Re-calculation of insulin and glucagon ELISA, based on the individial calibration curve of each ELISA plate
# As suggested by Jakob

# Load the packages from package-loading.R
source(here::here("R/package-loading.R"))

# Open data sets
Ins_Conc <- read.csv("~/assay_calculation/data/Final_Ins_Conc.csv", row.names = 1, stringsAsFactors = FALSE)
Gcg_Conc <- read.csv("~/assay_calculation/data/Final_Gcg_Conc.csv", row.names = 1, stringsAsFactors = FALSE)
# AUC calculation ---------------------------------------------------------


# ANOVA on AUC ------------------------------------------------------------
# One-way ANOVA test
# Tukey multiple pairwise-comparisons

# Insulin stimulation index -----------------------------------------------


# ANOVA on stimulation index ----------------------------------------------













Cal_Ins_v2 %>%
  split(.$ELISA_Date) %>%
  map(~ plot(data = ., Insulin, OD450))