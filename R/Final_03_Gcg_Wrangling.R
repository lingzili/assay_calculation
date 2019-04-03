# Re-calculation of glucagon ELISA, based on the individial calibration curve of each ELISA plate
# As suggested by Jakob

# Load the packages from package-loading.R
source(here::here("R/package-loading.R"))

# Tidy up sample file -----------------------------------------------------

# Load data set samples
OD450 <- read_excel("data/OD450_Gcg.xlsx")
View(OD450)

# Create a function to tidy up data frame OD450
tidy_func <- function(x) {
  # Reshape wide format to long format
  x <- x %>%
    gather(Minute, OD450, 6:44) %>%
    arrange(ID)

  x$ID <- as.character(x$ID) # Convert ID from numeric to character

  x$Minute <- as.numeric(x$Minute) # Convert Minute from character to numeric

  # Add glucose value to data set
  y <- x %>%
    mutate(Glucose = if_else(Minute < 5, "Glc_1mM",
      if_else(Minute < 18, "Glc_6mM",
        if_else(Minute < 33, "Glc_20mM", "KCl")
      )
    ))

  return(y)
}

# Apply tidy_func to data frame OD450
OD450_v2 <- tidy_func(OD450)

View(OD450_v2)

# Save tidied data set as csv
write.csv(OD450_v2, "data/OD450_Gcg.csv")

# Remove old data sets from the enviroment
rm(OD450)

# Tidy up calibration file ------------------------------------------------

# Load data set calibrator
Cal_Gcg <- read.csv("~/assay_calculation/data/Cal_Gcg.csv", stringsAsFactors = FALSE)
View(Cal_Gcg)

# Reshape wide format to long format
Cal_Gcg_v2 <- Cal_Gcg %>%
  gather(ELISA_Date, OD450, -Calibrator, -Glucagon) %>%
  arrange(ELISA_Date)

View(Cal_Gcg_v2)

rm(Cal_Gcg)

# Tidy up ELISA_Date ------------------------------------------------------

# Check the ELISA dates from OD450_v2
unique(OD450_v2$ELISA_Date)

# Remove X from ELISA dates in Cal_Gcg_v2
Cal_Gcg_v2$ELISA_Date <- gsub("X", "", Cal_Gcg_v2$ELISA_Date)
View(Cal_Gcg_v2)

# Replace "14.03.2019 p1" with "14.03.2019.p1" in OD450_v2
OD450_v2$ELISA_Date[OD450_v2$ELISA_Date == "14.03.2019 p1"] <- "14.03.2019.p1"

# Replace "14.03.2019 p2" with "14.03.2019.p2" in OD450_v2
OD450_v2$ELISA_Date[OD450_v2$ELISA_Date == "14.03.2019 p2"] <- "14.03.2019.p2"

# Check if the ELISA dates are the same in OD450_v2 and Cal_Gcg_v2
OD <- sort(unique(OD450_v2$ELISA_Date))

Cal <- sort(unique(Cal_Gcg_v2$ELISA_Date))

OD == Cal

rm(OD, Cal)

# Save both OD450_v2 and Cal_Gcg_v2
write.csv(OD450_v2, "data/OD450_Gcg_Tidy.csv")
write.csv(Cal_Gcg_v2, "data/Cal_Gcg_v2.csv")