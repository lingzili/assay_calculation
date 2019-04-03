# Re-calculation of glucagon ELISA, based on the individial calibration curve of each ELISA plate
# As suggested by Jakob

# Load the packages from package-loading.R
source(here::here("R/package-loading.R"))

# Load data sets ----------------------------------------------------------

# Load tidied OD450 data set
OD450_Gcg_Tidy <- read.csv("~/assay_calculation/data/OD450_Gcg_Tidy.csv", row.names = 1, stringsAsFactors = FALSE)
View(OD450_Gcg_Tidy)

# Load tidied calibrator data set
Cal_Gcg <- read.csv("~/assay_calculation/data/Cal_Gcg_v2.csv", row.names = 1, stringsAsFactors = FALSE)
View(Cal_Gcg)

# Inspect the calibrators -------------------------------------------------

# Inspect variations of each calibrator´s OD450 value
plot_Cal_v1 <- ggplot(Cal_Gcg, aes(Calibrator, OD450, fill = ELISA_Date)) +
  geom_bar(stat = "identity", width = .5, position = "dodge") +
  theme_bw()

plot_Cal_v1

ggsave(here::here("doc/Gcg_calibrator.png"), plot_Cal_v1)

# Inspect the change of OD450 values in each ELISA date
plot_Cal_v2 <- ggplot(Cal_Gcg, aes(Calibrator, OD450)) +
  geom_col() +
  facet_wrap(~ELISA_Date) +
  theme_bw()

plot_Cal_v2

ggsave(here::here("doc/Gcg_calibrator_date.png"), plot_Cal_v2)

# Calculate data from each ELISA plate ------------------------------------

# Create an empty data frame Glucagon_pmol_L
Glucagon_pmol_L <- NULL

# Create a matrix of plots
par(mfrow = c(3, 3))

for (i in unique(Cal_Gcg$ELISA_Date)) {
  # Subset calibrator data
  Cal_date <- subset(Cal_Gcg, ELISA_Date == i)

  # Subset OD450 data from 07.03.2019
  OD450_date <- subset(OD450_Gcg_Tidy, ELISA_Date == i)

  # Predict based on 4 paramter logistic
  model <- drm(Glucagon ~ OD450, data = Cal_date, fct = LL.4())

  Gcg_Conc <- OD450_date %>%
    mutate(Gcg_pmol_L = predict(model, as.data.frame(OD450_date$OD450), se.fit = FALSE))

  # Plot calibrator curve by 4 parameter logistic
  Cal_curve <- drm(OD450 ~ Glucagon, data = Cal_date, fct = LL.4())
  
  plot(Cal_curve,
       broken = TRUE, col = "red",
       xlab = "Glucagon pmol/L", ylab = "Calibrator OD450", main = paste(i),
       xlim = c(1, 130), ylim = c(0.05, 2.5),
       log = "xy", las = 1, lwd = 2
  )
  
  # Plot interpolated points on top
  points(Gcg_Conc$Gcg_pmol_L, Gcg_Conc$OD450, pch = 19, cex = 0.5, col = "black")

  # Add new data to the data frame
  Glucagon_pmol_L <- rbind(Glucagon_pmol_L, Gcg_Conc)
}

View(Glucagon_pmol_L)

# Save the plot of spline by clicking Export on R Studio

# Multiply conversion factor and volume to get glucagon ng per min
Glucagon_pmol_L <- Glucagon_pmol_L %>%
  mutate(Glucagon_ng = Gcg_pmol_L * 3.5 * 0.001 * Vol_ml)

head(Glucagon_pmol_L)

# Save calculated file
write.csv(Glucagon_pmol_L, "data/Final_Gcg_Conc.csv")
