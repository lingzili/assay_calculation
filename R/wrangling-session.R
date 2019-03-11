# Open the script package-loading.R and load all the packages

# Load data sets
Ins_Spl <- read_excel("data/Ins_Spl_v2_R.xlsx")
View(Ins_Spl)

Ins_Cal <- read_excel("data/Ins_Cal_v2_R .xlsx")
View(Ins_Cal)

# Tidy up sample file -----------------------------------------------------

# Reshape wide format to long format in sample file
Ins_Spl_v2 <- Ins_Spl %>%
  gather(Minute, OD450, 6:44) %>%
  arrange(ID)

View(Ins_Spl_v2)

str(Ins_Spl_v2)

# Convert ID from numeric to character
Ins_Spl_v2$ID <- as.character(Ins_Spl_v2$ID)

# Convert Minute from character to numeric
Ins_Spl_v2$Minute <- as.numeric(Ins_Spl_v2$Minute)

# Save wrangled file
write.csv(Ins_Spl_v2, "data/Ins_Spl_v2.csv")

# Tidy up calibrator file -------------------------------------------------

# Calculate average of calibrators
Ins_Cal$Avg <- apply(Ins_Cal[, 3:9], 1, mean)

# Save wrangled file
write.csv(Ins_Cal, "data/Ins_Ins_v2.csv")
