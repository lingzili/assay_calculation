library(tidyverse)

# Open data sets
Ins_Conc <- read.csv("~/assay_calculation/data/Ins_rm_1493_1483_facet.csv", row.names = 1, stringsAsFactors = FALSE)
head(Ins_Conc)

Gcg_Conc <- read.csv("~/assay_calculation/data/Gcg_rm_1493_1483_facet.csv", row.names = 1, stringsAsFactors = FALSE)
head(Gcg_Conc)

# Extract data of mouse 1433
Ins_1433 <- Ins_Conc %>%
  filter(ID == 1433)

Gcg_1433 <- Gcg_Conc %>%
  filter(ID == 1433)

# Plot insulin data
# Set margins
par(mar = c(4, 4, 3, 4))

plot(Ins_1433$Minute, Ins_1433$Insulin_ng,
  col = "darkgreen", type = "b", pch = 16,
  xaxt='n', xlab = "", 
  ylab = "ng/minute", ylim = c(0, 40), 
  lwd = 2, las = 2
)

axis(side = 1, at = c(seq(from = 0,to = 40, by = 5)))

# Overlay new plot
par(new = TRUE)

# Plot glucagon data
plot(Ins_1433$Minute, Gcg_1433$Glucagon_ng,
  col = "red", type = "b", lwd = 2, pch = 16,
  axes = FALSE, xlab = "Minute", ylab = "",
  main = "Mouse 1433, chow"
)

# Move the y-axis to the side
axis(side = 4, las = 2)

# Add legend
legend("topleft", c("Insulin", "Glucagon"),
  col = c("darkgreen", "red"),
  lty = c(1, 1), lwd = 2
)


