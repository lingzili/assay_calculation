# Open the script package-loading.R and load all the packages


# Tidy up calibrator file -------------------------------------------------

# Load data set calibrator
Gcg_Cal <- read_excel("data/Gcg_Calibrator.xlsx")
View(Gcg_Cal)

# Calculate average of calibrators
Gcg_Cal$Avg <- apply(Gcg_Cal[, 3:10], 1, mean)

# Save wrangled file
write.csv(Gcg_Cal, "data/Gcg_Calibrator.csv")

# Tidy up sample file -----------------------------------------------------

# Load data set samples
Gcg_Conc <- read_excel("data/Gcg_OD450.xlsx")
View(Gcg_Conc)

# Reshape wide format to long format in sample file
Gcg_Conc <- Gcg_Conc %>%
  gather(Minute, OD450, 5:43) %>%
  arrange(ID)

View(Gcg_Conc)

str(Gcg_Conc)

# Convert ID from numeric to character
Gcg_Conc$ID <- as.character(Gcg_Conc$ID)

# Convert Minute from character to numeric
Gcg_Conc$Minute <- as.numeric(Gcg_Conc$Minute)

# Add glucose value to data set
Gcg_Conc_v2 <- Gcg_Conc %>%
  mutate(Glucose = if_else(Minute < 5, "Glc_1mM",
    if_else(Minute < 18, "Glc_6mM",
      if_else(Minute < 33, "Glc_20mM", "KCl")
    )
  ))

View(Gcg_Conc_v2)

# Save wrangled file
write.csv(Gcg_Conc_v2, "data/Gcg_OD450.csv")
