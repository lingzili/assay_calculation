# Open the script package-loading.R and load all the packages

# Does recovery rate have an impact on AUC --------------------------------
# Open raw data
Ins_rm_1493_1483 <- read.csv("~/assay_calculation/data/Ins_rm_1493_1483.csv", row.names = 1, stringsAsFactors = FALSE)
View(Ins_rm_1493_1483)

# Open AUC data
AUC_v2 <- read.csv("~/assay_calculation/data/AUC_index_v2_facet.csv", row.names = 1, stringsAsFactors = FALSE)
View(AUC_v2)

# Add recovery rate to AUC data
AUC_v3 <- merge(AUC_v2, Ins_rm_1493_1483[, c("ID", "Recovery.rate")], by = "ID")
View(AUC_v3)
