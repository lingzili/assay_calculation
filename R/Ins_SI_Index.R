# FInd max value
SI_index <- Ins_rm_1493_1483 %>%
  group_by(ID, Glucose) %>%
  summarise(Peak = max(Insulin_ng, na.rm = TRUE))

View(SI_index)

# Chnage from long to wide form
SI_index_v2 <- SI_index %>%
  spread(key = Glucose, value = Peak)

View(SI_index_v2)

# Calulate stimulation index by peak value of 20 mM 1st phase to 6 mM 1st phase
SI_index_v2 <- SI_index_v2 %>%
  mutate(Index = Glc_20mM_1st / Glc_6mM_1st)

# Add diet to data set
df_Diet_ID <- Ins_rm_1493_1483 %>%
  select(Diet, ID)

View(df_Diet_ID)

# Remove duplicates based on ID
df_Diet_ID_v2 <- df_Diet_ID[!duplicated(df_Diet_ID$ID), ]

# Add a diet column to SI index v2

# Arrange both data frams by ID
df_Diet_ID_v2 <- df_Diet_ID_v2 %>% arrange(ID)

View(df_Diet_ID_v2)

SI_index_v2 <- SI_index_v2 %>% arrange(ID)

View(SI_index_v2)

SI_index_v2$Diet <- df_Diet_ID_v2$Diet

# Save SI index file
write.csv(SI_index_v2, "data/SI_index.csv")
write.csv(df_Diet_ID_v2, "data/Diet_ID.csv")

# Calculate mean and sd
SI_index_stat <- SI_index_v2 %>%
  group_by(Diet) %>%
  summarise(
    Mean = mean(Index),
    SD = sd(Index)
  )

View(SI_index_stat)

write.csv(SI_index_stat, "data/SI_Index_Stat.csv")

# Boxplots with points
Boxplot_SI <- SI_index_v2 %>%
  ggplot(aes(x = Diet, y = Index, group = Diet, fill = Diet)) +
  geom_boxplot(alpha = .4) +
  geom_jitter(width = .05, alpha = .4, size = 3) +
  guides(fill = "none") +
  theme_classic() +
  labs(x = NULL, y = "Stimulation index")

ggsave(here::here("doc/SI_index_boxplot.png"), Boxplot_SI)

# p values of SI index
pairwise.t.test(SI_index_v2$Index, SI_index_v2$Diet, pool.sd = FALSE)
