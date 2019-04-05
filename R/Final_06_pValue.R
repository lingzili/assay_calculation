# Re-calculation of insulin and glucagon ELISA, based on the individial calibration curve of each ELISA plate
# As suggested by Jakob

# Load the packages from package-loading.R
source(here::here("R/package-loading.R"))

# Open data sets
Ins_Conc <- read.csv("~/assay_calculation/data/Final_Ins_Conc.csv", row.names = 1, stringsAsFactors = FALSE)
Gcg_Conc <- read.csv("~/assay_calculation/data/Final_Gcg_Conc.csv", row.names = 1, stringsAsFactors = FALSE)

# AUC calculation ---------------------------------------------------------
# Insulin AUC
Ins_AUC <- Ins_Conc %>%
  group_by(ID, Glucose, Diet) %>%
  summarise(AUC = AUC(y = Insulin_ng, x = Minute))

View(Ins_AUC)

write.csv(Ins_AUC, "data/Final_Ins_AUC.csv")

# Glucagon AUC
Gcg_AUC <- Gcg_Conc %>%
  group_by(ID, Glucose, Diet) %>%
  summarise(AUC = AUC(y = Glucagon_ng, x = Minute))

View(Gcg_AUC)

write.csv(Gcg_AUC, "data/Final_Gcg_AUC.csv")

# Visualisation of AUC in each diet group ---------------------------------

# Insulin AUC boxplots

# Ranck the order of Glucose and Diet
Ins_AUC$Glucose <- factor(Ins_AUC$Glucose, levels = c("Glc_1mM", "Glc_6mM_1st", "Glc_6mM_2nd", "Glc_20mM_1st", "Glc_20mM_2nd", "KCl"))

Ins_AUC$Diet <- factor(Ins_AUC$Diet, levels = c("Chow", "2d HFD", "1wk HFD"))

# Boxplot with points
Ins_p1 <- Ins_AUC %>%
  ggplot(aes(x = Diet, y = AUC, fill = Diet))

Ins_p2 <- Ins_p1 +
  geom_boxplot(alpha = .4) +
  geom_jitter(width = .05, alpha = .4, size = 3) +
  facet_grid(cols = vars(Glucose)) +
  guides(fill = "none") +
  theme_classic() +
  labs(x = NULL, y = "Area under the curve") +
  theme(axis.text.x = element_text(size = 8, face = "bold"))

Ins_p2

ggsave(here::here("doc/Final_AUC_Ins.png"), Ins_p2)

# Glucagon AUC boxplots

# Ranck the order of Glucose and Diet
Gcg_AUC$Glucose <- factor(Gcg_AUC$Glucose, levels = c("Glc_1mM", "Glc_6mM", "Glc_20mM", "KCl"))

Gcg_AUC$Diet <- factor(Gcg_AUC$Diet, levels = c("Chow", "2d HFD", "1wk HFD"))

# Boxplot with points 
Gcg_p1 <- Gcg_AUC %>%
  ggplot(aes(x = Diet, y = AUC, fill = Diet))

Gcg_p2 <- Gcg_p1 +
  geom_boxplot(alpha = .4) +
  geom_jitter(width = .05, alpha = .4, size = 3) +
  facet_grid(cols = vars(Glucose)) +
  guides(fill = "none") +
  theme_classic() +
  labs(x = NULL, y = "Area under the curve") +
  theme(axis.text.x = element_text(size = 8, face = "bold"))

Gcg_p2

ggsave(here::here("doc/Final_AUC_Gcg.png"), Gcg_p2)

# ANOVA on AUC ------------------------------------------------------------
# One-way ANOVA test followed by Tukey multiple pairwise-comparisons
# Define fitted class with aov
aov_Ins <- Ins_AUC %>%
  aov(AUC ~ Diet, data = .)

aov_Gcg <- Gcg_AUC %>%
  aov(AUC ~ Diet, data = .)

# Calculate ANOVA and TukeyHSD
Ins_AUC_ANOVA <-
  append("Insulin ANOVA below", summary(aov_Ins)) %>%
  append(., TukeyHSD(aov_Ins))

Ins_AUC_ANOVA

Gcg_AUC_ANOVA <-
  append("Glucagon ANOVA below", summary(aov_Gcg)) %>%
  append(., TukeyHSD(aov_Gcg))

Gcg_AUC_ANOVA

FinStat_AUC <- append(Ins_AUC_ANOVA, Gcg_AUC_ANOVA)

FinStat_AUC

save(FinStat_AUC, file = "data/FinStat_AUC.RData")

# Insulin stimulation index -----------------------------------------------
# Find mean of each glucose condition
Ins_Avg <- Ins_Conc %>%
  group_by(ID, Glucose, Diet) %>%
  summarise(Avg = mean(Insulin_ng, na.rm = TRUE)) %>%
  spread(key = Glucose, value = Avg) # Change from long form to wide form

View(Ins_Avg)

# Stimulation index is (average of 20mM)/ (average at 1mM), and the same for 6mM
SI_Index <- Ins_Avg %>%
  mutate(
    Index_6mM_1st = Glc_6mM_1st / Glc_1mM,
    Index_6mM_2nd = Glc_6mM_2nd / Glc_1mM,
    Index_20mM_1st = Glc_20mM_1st / Glc_1mM,
    Index_20mM_2nd = Glc_20mM_2nd / Glc_1mM
  )

View(SI_Index)

write.csv(SI_Index, "data/Final_SI_Index.csv")

# Visualisation on SI index -----------------------------------------------

SI_Index_v2 <- SI_Index[, -(3:8), drop = FALSE] %>%
  gather(Glucose, SI_Index, -ID, -Diet) # Chnage from wide to long

View(SI_Index_v2)

# Remove Index_ from Glucose
SI_Index_v2$Glucose <- gsub("Index", "Glc", SI_Index_v2$Glucose)

# Ranck the order of Glucose and Diet
SI_Index_v2$Glucose <- factor(SI_Index_v2$Glucose, levels = c("Glc_6mM_1st", "Glc_6mM_2nd", "Glc_20mM_1st", "Glc_20mM_2nd"))

SI_Index_v2$Diet <- factor(SI_Index_v2$Diet, levels = c("Chow", "2d HFD", "1wk HFD"))

write.csv(SI_Index_v2, "data/Final_SI_Index_long.csv")

# Boxplot with points for SI index
SI_plot <- SI_Index_v2 %>%
  ggplot(aes(x = Diet, y = SI_Index, fill = Diet)) +
  geom_boxplot(alpha = .4) +
  geom_jitter(width = .05, alpha = .6, size = 3) +
  facet_grid(cols = vars(Glucose)) +
  guides(fill = "none") +
  theme_classic() +
  labs(x = NULL, y = "Stimulation index")

SI_plot

ggsave(here::here("doc/Final_SI.png"), SI_plot)

# ANOVA on stimulation index ----------------------------------------------
# One-way ANOVA test followed by Tukey multiple pairwise-comparisons
# Define fitted class with aov
Fvalue_SI <- SI_Index_v2 %>%
  split(.$Glucose) %>%
  map(~summary(aov(SI_Index ~ Diet, data = .))) %>%
  append("ANOVA on Insulin stimulation index below", .)

Fvalue_SI

Tukey_SI <- SI_Index_v2 %>%
  split(.$Glucose) %>%
  map(~TukeyHSD(aov(SI_Index ~ Diet, data = .)))

Tukey_SI

SI_ANOVA <- append(Fvalue_SI, Tukey_SI)

SI_ANOVA

save(SI_ANOVA, file = "data/FinStat_SI.RData")
