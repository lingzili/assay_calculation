# Calculate area under the curve
AUC_index <- Gcg_rm_1493_1483 %>%
  group_by(ID, Glucose) %>%
  summarise(AUC = AUC(y = Glucagon_ng, x = Minute))

View(AUC_index)

AUC_index <- as.data.frame(AUC_index)

# Add Diet to AUC data set
Diet_ID <- read.csv("~/assay_calculation/data/Diet_ID.csv", row.names = 1, stringsAsFactors = FALSE)

AUC_index_2 <- merge(AUC_index, Diet_ID[, c("ID", "Diet")], by = "ID")

View(AUC_index_2)

write.csv(AUC_index_2, "data/Gcg_AUC_index.csv")

# Boxplot with points
# Order column Glucose
AUC_index_2$Glc_facet <- factor(AUC_index_2$Glucose, levels = c("Glc_1mM", "Glc_6mM", "Glc_20mM", "KCl"))

write.csv(AUC_index_2, "data/Gcg_AUC_index_facet.csv")

p <- AUC_index_2 %>%
  ggplot(aes(x = Diet, y = AUC, group = Diet, fill = Diet))

AUC_Boxplot <- p +
  geom_boxplot(alpha = .4) +
  geom_jitter(width = .05, alpha = .4, size = 3) +
  facet_grid(cols = vars(Glc_facet)) +
  guides(fill = "none") +
  theme_classic() +
  labs(x = NULL, y = "Area under the curve") +
  theme(axis.text.x = element_text(size = 8, face = "bold"))

AUC_Boxplot

ggsave(here::here("doc/Gcg_AUC_Boxplot.png"), AUC_Boxplot)

# Dot plot to compare
Gcg_rm_1493_1483$Glc_facet <- factor(Gcg_rm_1493_1483$Glucose, levels = c("Glc_1mM", "Glc_6mM", "Glc_20mM", "KCl"))

dotplot <- Gcg_rm_1493_1483 %>%
  ggplot(aes(x = Minute, y = Glucagon_ng, color = ID, group = ID))

dotplot_line <- dotplot +
  geom_point() + geom_line() +
  facet_grid(Diet ~ Glc_facet) +
  theme_classic() +
  labs(y = "Glucagon (ng/minute)")

dotplot_line

ggsave(here::here("doc/gcg_secretion_phase.png"), dotplot_line)

write.csv(Gcg_rm_1493_1483, "data/Gcg_rm_1493_1483_facet.csv")
