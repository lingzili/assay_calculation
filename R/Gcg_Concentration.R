# Calculate calibration curve ---------------------------------------------

# Plot calibrator curve by 4 parameter logistic
Cal_curve <- drm(Avg ~ Glucagon, data = Gcg_Cal, fct = LL.4())

plot(Cal_curve, broken = TRUE, col = "red",
     xlab = "Glucagon pmol/L", ylab = "Calibrator OD450",
     xlim = c(1, 130), ylim = c(0.05, 2.5), 
     log = "xy", las = 1, lwd = 2) 

# Predict based on 4 paramter logistic
model <- drm(Glucagon ~ Avg, data = Gcg_Cal, fct = LL.4())

Gcg_Conc_v3 <- Gcg_Conc_v2 %>% 
  mutate(Glucagon = predict(model, as.data.frame(Gcg_Conc_v2$OD450), se.fit = FALSE))

head(Gcg_Conc_v3)

# Plot ontop
points(Gcg_Conc_v3$Glucagon, Gcg_Conc_v3$OD450, pch = 19, cex = 0.5, col = "black")

# Multiply conversion factor and volume to get glucagon ng per min
Gcg_Conc_v4 <- Gcg_Conc_v3 %>%
  mutate(Glucagon_ng = Glucagon * 3.5 * 0.001 * `Vol ml`)

# Save calculated file
write.csv(Gcg_Conc_v4, "data/Gcg_Concentration.csv")

# Visualization of glucagon secretion --------------------------------------

# Dotplot with line
dotplot <- Gcg_Conc_v4 %>% ggplot(aes(x = Minute, y = Glucagon_ng, color = ID, group = ID))

dotplot_line <- dotplot +
  geom_point() + geom_line() +
  facet_grid(cols = vars(Diet)) +
  theme_classic() +
  labs(y = "Glucagon (ng/minute)") +
  scale_x_continuous(breaks = seq(0, 40, 5))

dotplot_line

# Save plot
ggsave(here::here("doc/Gcg_secretion_ID.png"), dotplot_line)

# Check which mouse to remove in Chow
p1 <- Gcg_Conc_v4 %>%
  filter(Diet == "Chow") %>%
  ggplot(aes(x = Minute, y = Glucagon_ng, color = ID, group = ID))

plot_chow <- p1 +
  geom_point() + geom_line() +
  theme_classic() +
  labs(y = "Glucagon (ng/minute)") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  ggtitle("Chow")

plot_chow

ggsave(here::here("doc/gcg_plot_chow.png"), plot_chow, width = 7, height = 5)

# Check which mouse to remove in 2d HFD
p2 <- Gcg_Conc_v4 %>%
  filter(Diet == "2d HFD") %>%
  ggplot(aes(x = Minute, y = Glucagon_ng, color = ID, group = ID))

plot_2d_hfd <- p2 +
  geom_point() + geom_line() +
  theme_classic() +
  labs(y = "Glucagon (ng/minute)") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  ggtitle("2-day HFD")

plot_2d_hfd

ggsave(here::here("doc/gcg_plot_2d_HFD.png"), plot_2d_hfd, width = 7, height = 5)

# Check which mouse to remove in 1wk HFD
p3 <- Gcg_Conc_v4 %>%
  filter(Diet == "1wk HFD") %>%
  ggplot(aes(x = Minute, y = Glucagon_ng, color = ID, group = ID))

plot_1wk_hfd <- p3 +
  geom_point() + geom_line() +
  theme_classic() +
  labs(y = "Glucagon (ng/minute)") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  ggtitle("1-week HFD")

plot_1wk_hfd

ggsave(here::here("doc/gcg_plot_1wk_HFD.png"), plot_1wk_hfd, width = 7, height = 5)

# Remove ID 1493 and ID 1483 from data set
Gcg_rm_1493_1483 <- Gcg_Conc_v4[ !(Gcg_Conc_v4$ID %in% c(1493, 1483)), ]

unique(Gcg_rm_1493_1483$ID)

head(Gcg_rm_1493_1483)

# Save calculated file
write.csv(Gcg_rm_1493_1483, "data/Gcg_rm_1493_1483.csv")
