# Cubic spline regression -------------------------------------------------
# Plot calibrator curve
plot(Ins_Cal$Insulin, Ins_Cal$Avg,
     col = "darkblue",
     ylab = "Calibrator OD450", xlab = "Insulin µg/L", log = "xy",
     xlim = c(0.1, 10), ylim = c(0.01, 10), las = 1
)

# Add regression fitting line
lines(spline(Ins_Cal$Insulin, Ins_Cal$Avg), col = "red", lwd = 3)

# Create a new function out of splinefun
func <- splinefun(x = Ins_Cal$Insulin, y = Ins_Cal$Avg)

# Calculate absorance for insulin
Table <- data.frame(Insulin = seq(0.05, 8,by = 0.0001))
Table$Absorbance <- func(Table$Insulin)

# Import ELISA data 
Ins_Spl_v2 <- read.csv("~/assay_calculation/data/Ins_Spl_v2.csv", row.names=1, stringsAsFactors=FALSE)
Ins_Spl_v2 <- Ins_Spl_v2[ !is.na(Ins_Spl_v2$OD450),]
Ins_Spl_v2$Insulin <- 0

# Look up absorbance in Table
for (i in 1:nrow(Ins_Spl_v2)) {
  Ins_Spl_v2[i,"Insulin"] <- Table[which.min(abs(Ins_Spl_v2[i,"OD450"]-Table$Absorbance)),1]
}

# Plot ontop
points(Ins_Spl_v2$Insulin, Ins_Spl_v2$OD450, cex=0.01, col="blue")

# Multiply dilution factor
Ins_Spl_v2 <- Ins_Spl_v2 %>%
  mutate(Dil_Ins = if_else(Minute >= 32, Insulin * 20, Insulin * 5))

# Multiply volume to get insulin ng per min
Ins_Spl_v2 <- Ins_Spl_v2 %>%
  mutate(Insulin_ng = Dil_Ins * Vol.per.well.ml)

# Add glucose value to data set
Ins_Spl_v2 <- Ins_Spl_v2 %>%
  mutate(Group = if_else(Minute <= 4, "1mM",
                         if_else(Minute <= 17, "6mM",
                                 if_else(Minute <= 32, "20mM", "KCl")
                         )
  ))

# Save calculated file
write.csv(Ins_Spl_v2, "data/Ins_Spl_v4.csv")
Ins_Spl_v3 <- Ins_Spl_v2

Ins_Spl_v3$ID <- as.character(Ins_Spl_v3$ID)

# Visualization of insulin secretion --------------------------------------
# Dotplot with line
dotplot <- Ins_Spl_v3 %>% ggplot(aes(x = Minute, y = Insulin_ng, color = ID, group = ID))

dotplot_line <- dotplot +
  geom_point() + geom_line() +
  facet_grid(cols = vars(Diet)) +
  theme_classic() +
  labs(y = "Insulin (ng/minute)") +
  scale_x_continuous(breaks = seq(0, 40, 5))

dotplot_line
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

### AUC
AUC_index <- Ins_Spl_v3 %>%
  group_by(ID, Group) %>%
  summarise(AUC = AUC(y=Insulin_ng, x=Minute))

AUC_index_v2 <- AUC_index %>%
  spread(key = Group, value = AUC)

AUC_index_v2 <- as.data.frame(AUC_index_v2)
AUC_index_v2 <- merge(AUC_index_v2, Ins_Spl_v2[,c("ID","Diet")], by="ID")
AUC_index_v2 <- AUC_index_v2[ duplicated(AUC_index_v2)==F,]
colnames(AUC_index_v2) <- c("ID","mm1","mm20","mm6","KCl","Diet")

head(AUC_index_v2)

par(mfcol=c(1,3))
barplot(c(
mean(AUC_index_v2[ AUC_index_v2$Diet == "Chow","mm1"]),
mean(AUC_index_v2[ AUC_index_v2$Diet == "Chow","mm6"]),
mean(AUC_index_v2[ AUC_index_v2$Diet == "Chow","mm20"]),
mean(AUC_index_v2[ AUC_index_v2$Diet == "Chow","KCl"])), ylim=c(0,50))

barplot(c(
  mean(AUC_index_v2[ AUC_index_v2$Diet == "2d HFD","mm1"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "2d HFD","mm6"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "2d HFD","mm20"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "2d HFD","KCl"])), ylim=c(0,50))

barplot(c(
  mean(AUC_index_v2[ AUC_index_v2$Diet == "1wk HFD","mm1"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "1wk HFD","mm6"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "1wk HFD","mm20"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "1wk HFD","KCl"])), ylim=c(0,50))

par(mfcol=c(1,4))
barplot(c(
  mean(AUC_index_v2[ AUC_index_v2$Diet == "Chow","mm1"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "2d HFD","mm1"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "1wk HFD","mm1"])), ylim=c(0,2), names=c("Chow","2D","7D"), main="1mM")

barplot(c(
  mean(AUC_index_v2[ AUC_index_v2$Diet == "Chow","mm6"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "2d HFD","mm6"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "1wk HFD","mm6"])), ylim=c(0,30), names=c("Chow","2D","7D"), main="6mM")

barplot(c(
  mean(AUC_index_v2[ AUC_index_v2$Diet == "Chow","mm20"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "2d HFD","mm20"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "1wk HFD","mm20"])), ylim=c(0,50), names=c("Chow","2D","7D"), main="20mM")

barplot(c(
  mean(AUC_index_v2[ AUC_index_v2$Diet == "Chow","KCl"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "2d HFD","KCl"]),
  mean(AUC_index_v2[ AUC_index_v2$Diet == "1wk HFD","KCl"])), ylim=c(0,50), names=c("Chow","2D","7D"), main="KCl")



