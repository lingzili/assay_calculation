# Re-calculation of insulin and glucagon ELISA, based on the individial calibration curve of each ELISA plate
# As suggested by Jakob

# Load the packages from package-loading.R
source(here::here("R/package-loading.R"))

# Tidy up calibrator file -------------------------------------------------

# Load calibrator file
Calibrator <- read_excel("data/Duplicate_Cal.xlsx")
View(Calibrator)

# Change from wide to long format
Cal_long <- Calibrator %>%
  gather(Cal_Number, OD450, 4:9) %>%
  arrange(Test)

View(Cal_long)

# Calculate mean and SD of each calibrator
Cal_Stat <- Cal_long %>%
  group_by(Test, Cal_Number) %>%
  summarise(
    OD_mean = mean(OD450),
    OD_SD = sd(OD450)
  )

View(Cal_Stat)

# Save both the raw data and stat
write.csv(Cal_long, "data/Duplicate_Cal_long.csv")
write.csv(Cal_Stat, "data/Duplicate_Cal_Stat.csv")

# Dotplots ----------------------------------------------------------------

# Insulin
Ins_p1 <- Cal_long %>%
  filter(Test == "Insulin") %>%
  ggplot(aes(x = Cal_Number, y = OD450, fill = `ELISA Date`)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", dotsize = 0.4) +
  labs(title = "Insulin calibrators", x = NULL) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()

Ins_p1

ggsave(here::here("doc/Insulin_Calibrators.png"), Ins_p1)

# Glucagon
Gcg_p1 <- Cal_long %>%
  filter(Test == "Glucagon") %>%
  ggplot(aes(x = Cal_Number, y = OD450, fill = `ELISA Date`)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", dotsize = 0.4) +
  labs(title = "Glucagon calibrators", x = NULL) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()

Gcg_p1

ggsave(here::here("doc/Glucagon_Calibrators.png"), Gcg_p1)

# Dotplots with mean and SD -----------------------------------------------

# Insulin
Ins_p2 <- Cal_long %>%
  filter(Test == "Insulin") %>%
  ggplot(aes(x = Cal_Number, y = OD450)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", dotsize = 0.4) +
  labs(title = "Insulin calibrators with mean and SD", x = NULL) +
  theme_minimal() +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "crossbar", width = 0.5
  )

Ins_p2

ggsave(here::here("doc/Insulin_Calibrators_v2.png"), Ins_p2)

# Glucagon
Gcg_p2 <- Cal_long %>%
filter(Test == "Glucagon") %>%
  ggplot(aes(x = Cal_Number, y = OD450)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", dotsize = 0.4) +
  labs(title = "Glucagon calibrators with mean and SD", x = NULL) +
  theme_minimal() +
  stat_summary(
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "crossbar", width = 0.5
  )

Gcg_p2

ggsave(here::here("doc/Glcuagon_Calibrators_v2.png"), Gcg_p2)