# Open the script package-loading.R and load all the packages


# Tidy up calibrator file -------------------------------------------------

# Load data set calibrator
Ins_Cal <- read_excel("data/Ins_Calibrator.xlsx")
View(Ins_Cal)

# Calculate average of calibrators
Ins_Cal$Avg <- apply(Ins_Cal[, 3:10], 1, mean)

# Save wrangled file
write.csv(Ins_Cal, "data/Ins_Calibrator.csv")

# Tidy up sample file -----------------------------------------------------

# Load data set samples
Ins_Conc <- read_excel("data/Ins_OD450.xlsx")
View(Ins_Conc)

# Reshape wide format to long format in sample file
Ins_Conc <- Ins_Conc %>%
  gather(Minute, OD450, 5:43) %>%
  arrange(ID)

View(Ins_Conc)

str(Ins_Conc)

# Convert ID from numeric to character
Ins_Conc$ID <- as.character(Ins_Conc$ID)

# Convert Minute from character to numeric
Ins_Conc$Minute <- as.numeric(Ins_Conc$Minute)

# Add glucose value to data set
Ins_Conc <- Ins_Conc %>%
  mutate(Glucose = if_else(Minute < 5, "Glc_1mM",
    if_else(Minute < 11, "Glc_6mM_1st",
      if_else(Minute < 18, "Glc_6mM_2nd",
        if_else(Minute < 24, "Glc_20mM_1st",
          if_else(Minute < 33, "Glc_20mM_2nd", "KCl")
        )
      )
    )
  ))

# Add dilution factor to data set
Ins_Conc <- Ins_Conc %>%
  mutate(Dilution = if_else(Minute >= 32, 20, 5))

# Correct dilution factor for ID 1485, 1480
# Create a new data frame with ID 1485 and 1480, with new dilution factors
Ins_1485_1480 <- Ins_Conc %>%
  filter(ID == 1485 | ID == 1480) %>%
  mutate(Dilution = (if_else(Dilution == 5, 10, 25)))

View(Ins_1485_1480)

# Remove rows with ID 1485 and 1480 from old data set
Ins_Remove <- Ins_Conc[ !(Ins_Conc$ID %in% c(1485, 1480)), ]

unique(Ins_Remove$ID)

# Combine Ins_Remove and Ins_1485_1480 together
Ins_Conc_v2 <- rbind(Ins_Remove, Ins_1485_1480)

View(Ins_Conc_v2)

# Save wrangled file
write.csv(Ins_Conc_v2, "data/Ins_OD450.csv")
