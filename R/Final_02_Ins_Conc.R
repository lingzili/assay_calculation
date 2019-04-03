# Re-calculation of insulin ELISA, based on the individial calibration curve of each ELISA plate
# As suggested by Jakob

# Load the packages from package-loading.R
source(here::here("R/package-loading.R"))

# Load tidied OD450 data set
OD450_Ins_Tidy <- read.csv("~/assay_calculation/data/OD450_Ins_Tidy.csv", row.names = 1, stringsAsFactors = FALSE)
View(OD450_Ins_Tidy)

# Load tidied calibrator data set
Cal_Ins <- read.csv("~/assay_calculation/data/Cal_Ins_v2.csv", row.names = 1, stringsAsFactors = FALSE)
View(Cal_Ins)

# Inspect variations of each calibrator´s OD450 value
plot_Cal_v1 <- ggplot(Cal_Ins, aes(Calibrator, OD450, fill = ELISA_Date)) +
  geom_bar(stat = "identity", width = .5, position = "dodge") +
  theme_bw()

ggsave(here::here("doc/calibrator_OD450.png"), plot_Cal_v1)

# Inspect the change of OD450 values in each ELISA date
plot_Cal_v2 <- ggplot(Cal_Ins, aes(Calibrator, OD450)) +
  geom_col() +
  facet_wrap(~ELISA_Date) +
  theme_bw()

ggsave(here::here("doc/calibrator_date.png"), plot_Cal_v2)

# Calculate the data collected on each date ------------------------------

# Create an empty data frame Insulin_ng_L
Insulin_ng_L <- NULL

# Create a matrix of plots
par(mfrow=c(3,3))

for (i in unique(Cal_Ins$ELISA_Date)) {
  # Subset calibrator data
  Cal_date <- subset(Cal_Ins, ELISA_Date == i)

  # Subset OD450 data from 07.03.2019
  OD450_date <- subset(OD450_Ins_Tidy, ELISA_Date == i)

  # Create cubic spline function
  cubic_func <- splinefun(x = Cal_date$OD450, y = Cal_date$Insulin, method = "monoH.FC")

  # Interpolate insulin value
  Ins_Conc <- OD450_date %>%
    mutate(Ins_ug_L = cubic_func(OD450_date$OD450))

  # Plot calibrator curve
  plot(Cal_date$Insulin, Cal_date$OD450,
    col = "darkblue",
    xlab = "Insulin µg/L", ylab = "Calibrator OD450", main = paste(i),
    xlim = c(0.1, 10), ylim = c(0.01, 5),
    cex.axis = 0.8,
    log = "xy", las = 1
  )

  # Add regression fitting line
  lines(spline(Cal_date$Insulin, Cal_date$OD450), col = "red", lwd = 3)

  # Plot interpolated points on top
  points(Ins_Conc$Ins_ug_L, Ins_Conc$OD450, pch = 1, cex = 0.5, col = "black")
  
  # Add new data to the data frame
  Insulin_ng_L <- rbind(Insulin_ng_L, Ins_Conc)
}

View(Insulin_ng_L)

# Save the plot of spline by clicking Export on R Studio

# Multiply dilution factor
Ins_Dilution <- Insulin_ng_L %>%
  mutate(Ins_Dilute = Ins_ug_L * Dilution)

# Multiply volume to get insulin ng per min
Ins_ng <- Ins_Dilution %>%
  mutate(Insulin_ng = Ins_Dilute * Vol_ml)

head(Ins_ng)

# Save calculated file
write.csv(Ins_ng, "data/Final_Ins_Conc.csv")