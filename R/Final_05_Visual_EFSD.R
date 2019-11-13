# Re-plotting of pancreas perfusion insulin data for EFSD grant

# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)

# Load data on concentration ----------------------------------------------
Ins_n_Gcg <- read.csv("~/assay_calculation/data/Final_Ins_Gcg.csv", row.names = 1)
View(Ins_n_Gcg)

# Calculate mean and SEM --------------------------------------------------
FinStat <- Ins_n_Gcg %>%
  group_by(Diet, Minute) %>%
  summarise(
    Avg_Ins = mean(Insulin_ng, na.rm = TRUE),
    SEM_Ins = sd(Insulin_ng, na.rm = TRUE)/sqrt(length(Insulin_ng))
  )

head(FinStat)

# Create a certain order within Diet
FinStat$Diet <- factor(FinStat$Diet, levels = c("Chow", "2d HFD", "1wk HFD"))

write.csv(FinStat, "data/EFSD_Ins_mean_SEM.csv")

# Insulin line plot -------------------------------------------------------
# Set standard theme for faceted line graph
standard_theme_facet_line <- theme(
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(color = "black", size = 16, face = "bold"),
  axis.text.y = element_text(color = "black", size = 16, face = "bold"),
  axis.title.x = element_text(color = "black", size = 18, face = "bold"),
  axis.title.y = element_text(color = "black", size = 18, face = "bold"),
  strip.text.x = element_text(color = "black", size = 18, face = "bold"),
  strip.background = element_rect(fill = "white"),
  legend.title = element_blank(),
  legend.text = element_text(color = "black", size = 18, face = "bold"),
  legend.key = element_rect(fill = "white"), # Remove grey background of the legend
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, size = 2),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
  plot.title = element_text(color = "black", size = 20, face = "bold")
)

Ins_p1 <- FinStat %>%
  ggplot(aes(x = Minute, y = Avg_Ins, color = Diet))

Ins_p2 <- Ins_p1 +
  geom_point(size = 4) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = Avg_Ins - SEM_Ins, ymax = Avg_Ins + SEM_Ins), size = 1, width = .5) +
  labs(y = "Insulin (ng)") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  ylim(0, 30) +
  standard_theme_facet_line

Ins_p2

ggsave(here::here("doc/EFSD_Ins_mean_SEM.png"), Ins_p2)
