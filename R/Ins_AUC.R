# Calculate area under the curve
AUC_index <- Ins_rm_1493_1483 %>%
  group_by(ID, Glucose) %>%
  summarise(AUC = AUC(y = Insulin_ng, x = Minute))

View(AUC_index)

AUC_index <- as.data.frame(AUC_index)

# Add Diet to AUC data set
AUC_index <- merge(AUC_index, df_Diet_ID_v2[, c("ID", "Diet")], by = "ID")

write.csv(AUC_index, "data/AUC_index.csv")

# Boxplot with points
p <- AUC_index %>%
  ggplot(aes(x = Diet, y = AUC, group = Diet, fill = Diet))

AUC_index$Glc_facet <- factor(AUC_index$Glucose, levels = c("Glc_1mM", "Glc_6mM_1st", "Glc_6mM_2nd", "Glc_20mM_1st", "Glc_20mM_2nd", "KCl"))

AUC_Boxplot <- p +
  geom_boxplot(alpha = .4) +
  geom_jitter(width = .05, alpha = .4, size = 3) +
  facet_grid(cols = vars(Glc_facet)) +
  guides(fill = "none") +
  theme_classic() +
  labs(x = NULL, y = "Area under the curve") +
  theme(axis.text.x = element_text(size = 8, face = "bold"))

ggsave(here::here("doc/AUC_Boxplot.png"), AUC_Boxplot)

write.csv(AUC_index, "data/AUC_index_v2_facet.csv")

# Dot plot to compare
Ins_rm_1493_1483$Glc_facet <- factor(Ins_rm_1493_1483$Glucose, levels = c("Glc_1mM", "Glc_6mM_1st", "Glc_6mM_2nd", "Glc_20mM_1st", "Glc_20mM_2nd", "KCl"))

dotplot <- Ins_rm_1493_1483 %>% 
  ggplot(aes(x = Minute, y = Insulin_ng, color = ID, group = ID))

dotplot_line <- dotplot +
  geom_point() + geom_line() +
  facet_grid(Diet ~ Glc_facet) +
  theme_classic() +
  labs(y = "Insulin (ng/minute)")

ggsave(here::here("doc/secretion_phase.png"), dotplot_line)

write.csv(Ins_rm_1493_1483, "data/Ins_rm_1493_1483_facet.csv")

