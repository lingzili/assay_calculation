# Open the script package-loading.R and load all the packages

# Check where is here
here()

# Load packages in this script and note to adapt the file path
source(here::here("R/package-loading.R"))

# Load data sets
Ins_Spl <- read_excel("data/Ins_Spl_v2_R.xlsx")
View(Ins_Spl)

Ins_Cal <- read_excel("data/Ins_Cal_v2_R .xlsx")
View(Ins_Cal)

# Reshape wide format to long format
Ins_Spl_v2 <- Ins_Spl %>%
  gather(Minute, Insulin, 6:44) %>%
  arrange(ID)

View(Ins_Spl_v2)

str(Ins_Spl_v2)

# Convert ID from numeric to character
Ins_Spl_v2$ID <- as.character(Ins_Spl_v2$ID)

# Convert Minute from character to numeric
Ins_Spl_v2$Minute <- as.numeric(Ins_Spl_v2$Minute)

str(Ins_Spl_v2)

# Save wrangled file
write.csv(Ins_Spl_v2, "data/Ins_Spl_v2.csv")
