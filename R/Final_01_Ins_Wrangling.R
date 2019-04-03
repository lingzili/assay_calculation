# Re-calculation of insulin ELISA, based on the individial calibration curve of each ELISA plate
# As suggested by Jakob

# Load the packages from package-loading.R
source(here::here("R/package-loading.R"))

# Tidy up sample file -----------------------------------------------------

# Load data set samples
OD450 <- read_excel("data/OD450_Ins.xlsx")
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
      if_else(Minute < 11, "Glc_6mM_1st",
        if_else(Minute < 18, "Glc_6mM_2nd",
          if_else(Minute < 24, "Glc_20mM_1st",
            if_else(Minute < 33, "Glc_20mM_2nd", "KCl")
          )
        )
      )
    ))

  return(y)
}

# Apply tidy_func to data frame OD450
OD450_v2 <- tidy_func(OD450)

View(OD450_v2)

# Add dilution factor to data set
OD450_v2 <- OD450_v2 %>%
  mutate(Dilution = if_else(Minute >= 32, 20, 5))

# Correct dilution factor for ID 1485, 1480
# Create a new data frame with ID 1485 and 1480, with new dilution factors
Ins_1485_1480 <- OD450_v2 %>%
  filter(ID == 1485 | ID == 1480) %>%
  mutate(Dilution = (if_else(Dilution == 5, 10, 25)))

View(Ins_1485_1480)

# Remove rows with ID 1485 and 1480 from old data set
Ins_Remove <- OD450_v2[ !(OD450_v2$ID %in% c(1485, 1480)), ]

# Check the IDs of Ins_OD450_v2
unique(Ins_Remove$ID)

# Combine Ins_Remove and Ins_1485_1480 together
OD450_v3 <- rbind(Ins_Remove, Ins_1485_1480)

View(OD450_v3)

# Save tidied data set as csv
write.csv(OD450_v3, "data/OD450_Ins.csv")

# Remove old data sets from the enviroment
rm(OD450, OD450_v2, Ins_1485_1480, Ins_Remove)

# Tidy up calibration file ------------------------------------------------

# Load data set calibrator
Cal_Ins <- read.csv("~/assay_calculation/data/Cal_Ins.csv", stringsAsFactors = FALSE)
View(Cal_Ins)

# Reshape wide format to long format
Cal_Ins_v2 <- Cal_Ins %>%
  gather(ELISA_Date, OD450, -Calibrator, -Insulin) %>%
  arrange(ELISA_Date)

View(Cal_Ins_v2)

rm(Cal_Ins)

# Tidy up ELISA_Date ------------------------------------------------------

# Check the ELISA dates from OD450_v3
unique(OD450_v3$ELISA_Date)

# Remove X from ELISA dates in Cal_Ins_v2
Cal_Ins_v2$ELISA_Date <- gsub("X", "", Cal_Ins_v2$ELISA_Date)
View(Cal_Ins_v2)

# Check the ELISA dates from Cal_Ins_v2
unique(Cal_Ins_v2$ELISA_Date)

# Replace "08.03.2019 p1" with "08.03.2019.p1" in OD450_v3
OD450_v3$ELISA_Date[OD450_v3$ELISA_Date == "08.03.2019 p1"] <- "08.03.2019.p1"

# Replace "08.03.2019 p2" with "08.03.2019.p2" in OD450_v3
OD450_v3$ELISA_Date[OD450_v3$ELISA_Date == "08.03.2019 p2"] <- "08.03.2019.p2"

# Check if the ELISA dates are the same in OD450_v3 and Cal_Ins_v2
OD <- sort(unique(OD450_v3$ELISA_Date))

Cal <- sort(unique(Cal_Ins_v2$ELISA_Date))

OD == Cal

rm(OD, Cal)

# Save both OD450_v3 and Cal_Ins_v2
write.csv(OD450_v3, "data/OD450_Ins_Tidy.csv")
write.csv(Cal_Ins_v2, "data/Cal_Ins_v2.csv")