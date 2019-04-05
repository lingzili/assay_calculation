# Re-calculation of insulin and glucagon ELISA, based on the individial calibration curve of each ELISA plate
# As suggested by Jakob

# Load the packages from package-loading.R
source(here::here("R/package-loading.R"))

# Combine insulin and glucagon data sets ----------------------------------

# Open data sets
Ins_Conc <- read.csv("~/assay_calculation/data/Final_Ins_Conc.csv", row.names = 1, stringsAsFactors = FALSE)
Gcg_Conc <- read.csv("~/assay_calculation/data/Final_Gcg_Conc.csv", row.names = 1, stringsAsFactors = FALSE)

# Combine the Ins_Conc and Gcg_Conc together
Ins_Conc_v2 <- Ins_Conc %>%
  dplyr::select(Diet, ID, Minute, Insulin_ng)

Gcg_Conc_v2 <- Gcg_Conc %>%
  dplyr::select(ID, Minute, Glucagon_ng)

Ins_n_Gcg <- merge(Ins_Conc_v2, Gcg_Conc_v2, by = c("ID", "Minute"))

# Sort the data frame based on ID and Minute
Ins_n_Gcg <- Ins_n_Gcg %>%
  arrange(ID, Minute)

View(Ins_n_Gcg)

# Save Ins_n_Gcg
write.csv(Ins_n_Gcg, "data/Final_Ins_Gcg.csv")

# Plot individual mouse data ----------------------------------------------

# Set plot margins
par(mar = c(4, 4, 3, 4))

for (i in unique(Ins_n_Gcg$ID)) {

  # subset data according to ID
  df <- subset(Ins_n_Gcg, Ins_n_Gcg$ID == i)

  # order the data frame by Minute
  df <- df[order(df$Minute), ]

  # dotplot with line
  plot(df$Minute, df$Insulin_ng,
    col = "darkgreen", type = "o", pch = 16, # pch define symbol type
    axes = FALSE, xlab = "", ylab = "",
    ylim = range(0:40)
  )
  
  # Set tick marks at x axis
  axis(side = 1, at = c(seq(from = 0, to = 40, by = 5))) # at indicates where tic marks should be drawn
  
  # Set color and label of y axis
  axis(side = 2, col.axis = "darkgreen")
  mtext("Insulin ng", side = 2, line = 2.5, las = 0, col = "darkgreen")
  # line indicates the distance from the axis
  # las indicates parallel (=0) or perpendicular(=2) to the axis
  
  # Overlay new plot
  par(new = TRUE)

  # Plot glucagon data
  plot(df$Minute, df$Glucagon_ng,
    col = "red", type = "o", pch = 16,
    axes = FALSE, xlab = "", ylab = "", ylim = c(0, 0.14),
    main = paste("ID", i, unique(df$Diet))
  )
  
  axis(side = 4, las = 0, col.axis = "red")
  mtext("Glucagon ng", side = 4, line = 2.5, las = 0, col = "red")
  
  mtext("Minute", side = 1, line = 3, las = 0, col = "black")

  # Add legend
  legend("topleft", c("Insulin", "Glucagon"),
    col = c("darkgreen", "red"),
    lty = c(1, 1), lwd = 2 # lty specifies the line texture and lwd specifies line width
  )
}

# Save plots by clicking "Export"

# Plot each stimulation phase ---------------------------------------------

# Insulin
# Change to character for variable ID
Ins_Conc$ID <- as.character(Ins_Conc$ID)

# Ranck the order of Glucose and Diet
Ins_Conc$Glucose <- factor(Ins_Conc$Glucose, levels = c("Glc_1mM", "Glc_6mM_1st", "Glc_6mM_2nd", "Glc_20mM_1st", "Glc_20mM_2nd", "KCl"))

Ins_Conc$Diet <- factor(Ins_Conc$Diet, levels = c("Chow", "2d HFD", "1wk HFD"))

Ins_dotplot <- Ins_Conc %>% 
  ggplot(aes(x = Minute, y = Insulin_ng, color = ID))

Ins_dotplot_line <- Ins_dotplot +
  geom_point() + geom_line() +
  facet_grid(Diet ~ Glucose) +
  labs(y = "Insulin (ng)") +
  theme_classic() +
  scale_fill_brewer(palette = "Dark2")

Ins_dotplot_line

ggsave(here::here("doc/Final_Ins_Phase.png"), Ins_dotplot_line)

# Glucagon
# Change to character for variable ID
Gcg_Conc$ID <- as.character(Gcg_Conc$ID)

# Ranck the order of Glucose and Diet
Gcg_Conc$Glucose <- factor(Gcg_Conc$Glucose, levels = c("Glc_1mM", "Glc_6mM", "Glc_20mM", "KCl"))

Gcg_Conc$Diet <- factor(Gcg_Conc$Diet, levels = c("Chow", "2d HFD", "1wk HFD"))

Gcg_dotplot <- Gcg_Conc %>% 
  ggplot(aes(x = Minute, y = Glucagon_ng, color = ID))

Gcg_dotplot_line <- Gcg_dotplot +
  geom_point() + geom_line() +
  facet_grid(Diet ~ Glucose) +
  labs(y = "Glucagon (ng)") +
  theme_classic() +
  scale_fill_brewer(palette = "Dark2")

Gcg_dotplot_line

ggsave(here::here("doc/Final_Gcg_Phase.png"), Gcg_dotplot_line)

# Plot line graph with mean and SD ----------------------------------------

# Calculate mean and sd 
FinStat <- Ins_n_Gcg %>%
  group_by(Diet, Minute) %>%
  summarise(
    Avg_Ins = mean(Insulin_ng, na.rm = TRUE),
    SD_Ins = sd(Insulin_ng, na.rm = TRUE),
    Avg_Gcg = mean(Glucagon_ng, na.rm = TRUE),
    SD_Gcg = sd(Glucagon_ng, na.rm = TRUE)
  )

FinStat

# Create a certain order within Diet
FinStat$Diet <- factor(FinStat$Diet, levels = c("Chow", "2d HFD", "1wk HFD"))

write.csv(FinStat, "data/Final_Stat.csv")

# Insulin line plot
Ins_p1 <- FinStat %>%
  ggplot(aes(x = Minute, y = Avg_Ins, color = Diet))

Ins_p2 <- Ins_p1 +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = Avg_Ins - SD_Ins, ymax = Avg_Ins + SD_Ins), width = .4) +
  theme_classic() +
  labs(y = "Insulin (ng)") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  scale_y_continuous(breaks = seq(0, 30, 10))

Ins_p2

ggsave(here::here("doc/Final_Ins_stat.png"), Ins_p2)

# Glucagon line plot
Gcg_p1 <- FinStat %>%
  ggplot(aes(x = Minute, y = Avg_Gcg, color = Diet))

Gcg_p2 <- Gcg_p1 +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = Avg_Gcg - SD_Gcg, ymax = Avg_Gcg + SD_Gcg), width = .4) +
  theme_classic() +
  labs(y = "Glucagon (ng)") +
  scale_x_continuous(breaks = seq(0, 40, 5))

Gcg_p2

ggsave(here::here("doc/Final_Gcg_stat.png"), Gcg_p2)
