# Cubic spline regression -------------------------------------------------
# Plot calibrator curve
plot(Ins_Cal$Avg, Ins_Cal$Insulin,
  col = "darkblue",
  xlim = c(0, 2.5), ylim = c(0, 8),
  xlab = "Calibrator OD450", ylab = "Insulin µg/L"
)

# Add regression fitting line
lines(spline(Ins_Cal$Avg, Ins_Cal$Insulin), col = "red", lwd = 3)

# Create a new function out of splinefun
func <- splinefun(x = Ins_Cal$Avg, y = Ins_Cal$Insulin, method = "fmm", ties = mean)
lines(Ins_Cal$Avg, func(Ins_Cal$Avg, deriv = 0), lwd=2, col="black")


# Interpolate insulin value
Ins_Spl_v3 <- Ins_Spl_v2 %>% mutate(spline_Ins = func(Ins_Spl_v2$OD450))

View(Ins_Spl_v3)

# Plot samples OD450 against interpolated insulin value
plot(Ins_Spl_v3$OD450, Ins_Spl_v3$spline_Ins,
  col = "black",
  xlim = c(0, 2.5), ylim = c(0, 8),
  xlab = "Sample OD450", ylab = "Insulin µg/L"
)

# Add regression fitting line again
lines(spline(Ins_Cal$Avg, Ins_Cal$Insulin), col = "red", lwd = 3)

# Calulate final insulin value --------------------------------------------

# Multiply dilution factor
Ins_Spl_v3 <- Ins_Spl_v3 %>%
  mutate(Dil_Ins = if_else(Minute >= 32, spline_Ins * 20, spline_Ins * 5))

# Multiply volume to get insulin ng per min
Ins_Spl_v3 <- Ins_Spl_v3 %>%
  mutate(Insulin_ng = Dil_Ins * Vol.per.well.ml)

# Add glucose value to data set
Ins_Spl_v3 <- Ins_Spl_v3 %>%
  mutate(Group = if_else(Minute <= 4, "1mM",
    if_else(Minute <= 17, "6mM",
      if_else(Minute <= 32, "20mM", "KCl")
    )
  ))

# Save calculated file
write.csv(Ins_Spl_v3, "data/Ins_Spl_v3.csv")

# Visualization of insulin secretion --------------------------------------
# Dotplot with line
dotplot <- Ins_Spl_v3 %>% ggplot(aes(x = Minute, y = Insulin_ng, color = ID, group = ID))

dotplot_line <- dotplot +
  geom_point() + geom_line() +
  facet_grid(cols = vars(Diet)) +
  theme_classic() +
  labs(y = "Insulin (ng/minute)") +
  scale_x_continuous(breaks = seq(0, 40, 5))

# Save plot
ggsave(here::here("doc/secretion_ID.png"), dotplot_line, width = 7, height = 5)

# SI Index ----------------------------------------------------------------

# FInd max value
SI_index <- Ins_Spl_v3 %>%
  group_by(ID, Group) %>%
  summarise(Peak = max(Insulin_ng, na.rm = TRUE))

View(SI_index)

# Save SI index file
write.csv(SI_index, "data/SI_index.csv")

# Chnage from long to wide form
SI_index_v2 <- SI_index %>%
  spread(key = Group, value = Peak)

View(SI_index_v2)

# Calulate stimulation index by peak value of 20 mM and 6 mM
SI_index_v2 <- SI_index_v2 %>%
  mutate(Index = `20mM` / `6mM`)

# Save SI index file
write.csv(SI_index_v2, "data/SI_index_v2.csv")

# Add new column to data frame
SI_index_v2$Diet <- AUC_index_v2$Diet

# Calculate mean
SI_Mean <- SI_index_v2 %>% 
  group_by(Diet) %>% 
  summarise(Mean_Index = mean(Index))

colnames(SI_Mean)[2] <- "Index"

ggplot(SI_index_v2, aes(x = Diet, y = Index)) +
  geom_point(size = 3) +
  geom_bar(data = SI_Mean, stat = "identity", fill = "transparent", colour = "darkgrey") +
  theme_classic()
