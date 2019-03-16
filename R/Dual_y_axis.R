library(tidyverse)

# Open data sets
Ins_Conc <- read.csv("~/assay_calculation/data/Ins_Concentration.csv", row.names=1, stringsAsFactors=FALSE)
Gcg_Conc <- read.csv("~/assay_calculation/data/Gcg_Concentration.csv", row.names=1, stringsAsFactors=FALSE)

# Combine the Ins_Conc and Gcg_Conc together
Ins_Conc_v2 <- Ins_Conc %>%
  select(Diet, ID, Minute, Insulin_ng)

Gcg_Conc_v2 <- Gcg_Conc %>%
  select(ID, Minute, Glucagon_ng)

Ins_n_Gcg <- merge(Ins_Conc_v2, Gcg_Conc_v2, by = c("ID", "Minute"))

View(Ins_n_Gcg)

write.csv(Ins_n_Gcg, "data/Combine_Ins_Gcg.csv")

# Loop to create multiple graphs ------------------------------------------
# Set plot margins
par(mar = c(4, 4, 3, 4))

for (i in unique(Ins_n_Gcg$ID)) {

  # subset data according to ID
  df <- subset(Ins_n_Gcg, Ins_n_Gcg$ID == i)

  # order the data frame by Minute
  df <- df[order(df$Minute), ]

  # dotplot with line
  plot(df$Minute, df$Insulin_ng,
    col = "darkgreen", type = "o", pch = 16,
    xaxt = "n", xlab = "",
    ylab = "ng/minute", ylim = c(0, 40), las = 2
  )

  axis(side = 1, at = c(seq(from = 0, to = 40, by = 5)))

  # Overlay new plot
  par(new = TRUE)

  # Plot glucagon data
  plot(df$Minute, df$Glucagon_ng,
    col = "red", type = "o", pch = 16,
    axes = FALSE, xlab = "Minute", ylab = "",
    main = paste("ID", i, unique(df$Diet))
  )

  # Move the y-axis to the side
  axis(side = 4, las = 2)

  # Add legend
  legend("topleft", c("Insulin", "Glucagon"),
    col = c("darkgreen", "red"),
    lty = c(1, 1), lwd = 2
  )
}

# Save plots by clicking "Export"