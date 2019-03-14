# Calculate calibration curve ---------------------------------------------

# Plot calibrator curve
plot(Ins_Cal$Insulin, Ins_Cal$Avg,
  col = "darkblue",
  xlab = "Insulin µg/L", ylab = "Calibrator OD450",
  xlim = c(0.1, 10), ylim = c(0.01, 5),
  log = "xy", las = 1
)

# Add regression fitting line
lines(spline(Ins_Cal$Insulin, Ins_Cal$Avg), col = "red", lwd = 3)

# Create a new function out of splinefun
func <- splinefun(x = Ins_Cal$Avg, y = Ins_Cal$Insulin, method = "natural")

# Calculate insulin value -------------------------------------------------

# Interpolate insulin value
Ins_Conc_v3 <- Ins_Conc_v2 %>% mutate(Insulin = func(Ins_Conc_v2$OD450))

# Plot ontop
points(Ins_Conc_v3$Insulin, Ins_Conc_v3$OD450, pch = 1, cex = 0.2, col = "black")

# Multiply dilution factor
Ins_Conc_v3 <- Ins_Conc_v3 %>%
  mutate(Insulin_Dilute = Insulin * Dilution)

# Multiply volume to get insulin ng per min
Ins_Conc_v3 <- Ins_Conc_v3 %>%
  mutate(Insulin_ng = Insulin_Dilute * Vol.ml)

# Save calculated file
write.csv(Ins_Conc_v3, "data/Ins_Concentration.csv")

# Visualization of insulin secretion --------------------------------------

# Dotplot with line
dotplot <- Ins_Conc_v3 %>% ggplot(aes(x = Minute, y = Insulin_ng, color = ID, group = ID))

dotplot_line <- dotplot +
  geom_point() + geom_line() +
  facet_grid(cols = vars(Diet)) +
  theme_classic() +
  labs(y = "Insulin (ng/minute)") +
  scale_x_continuous(breaks = seq(0, 40, 5))

dotplot_line

# Save plot
ggsave(here::here("doc/secretion_ID.png"), dotplot_line, width = 7, height = 5)

# Check which mouse to remove in Chow
p1 <- Ins_Conc_v3 %>% 
  filter(Diet == "Chow") %>%
  ggplot(aes(x = Minute, y = Insulin_ng, color = ID, group = ID))

plot_chow <- p1 +
  geom_point() + geom_line() +
  theme_classic() +
  labs(y = "Insulin (ng/minute)") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  ggtitle("Chow")

plot_chow

ggsave(here::here("doc/plot_chow.png"), plot_chow, width = 7, height = 5)

# Check which mouse to remove in 2d HFD
p2 <- Ins_Conc_v3 %>% 
  filter(Diet == "2d HFD") %>%
  ggplot(aes(x = Minute, y = Insulin_ng, color = ID, group = ID))

plot_2d_hfd <- p2 +
  geom_point() + geom_line() +
  theme_classic() +
  labs(y = "Insulin (ng/minute)") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  ggtitle("2-day HFD")

plot_2d_hfd

ggsave(here::here("doc/plot_2d_HFD.png"), plot_2d_hfd, width = 7, height = 5)

# Check which mouse to remove in 1wk HFD
p3 <- Ins_Conc_v3 %>% 
  filter(Diet == "1wk HFD") %>%
  ggplot(aes(x = Minute, y = Insulin_ng, color = ID, group = ID))

plot_1wk_hfd <- p3 +
  geom_point() + geom_line() +
  theme_classic() +
  labs(y = "Insulin (ng/minute)") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  ggtitle("1-week HFD")

plot_1wk_hfd

ggsave(here::here("doc/plot_1wk_HFD.png"), plot_1wk_hfd, width = 7, height = 5)

# Remove ID 1493 and ID 1483 from data set
Ins_rm_1493_1483 <- Ins_Conc_v3[ !(Ins_Conc_v3$ID %in% c(1493, 1483)), ]

unique(Ins_rm_1493_1483$ID)

head(Ins_rm_1493_1483)

# Save calculated file
write.csv(Ins_rm_1493_1483, "data/Ins_rm_1493_1483.csv")
