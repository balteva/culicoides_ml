

#the 10/04/2025 hard disk data:
predictors_abundance <- c("TX_0_0","SWV_0_3","FG_0_1","NDVI_0_5", "ALT")

#predictions seem to be made on this data but from Loic's computer local data

model_abundance_LTO <- readRDS("E:/ieva/models/LLTO_abundance_predictive/reduced_abundance_LTO.rds")
# mtry = 2, splitrule = variance and min.node.size = 5.
model_abundance_LLO <- readRDS("E:/ieva/models/LLTO_abundance_predictive/reduced_abundance_LLO.rds")
# mtry = 3, splitrule = extratrees and min.node.size = 5.

full_abundance <- readRDS("E:/ieva/models/LLTO_abundance_predictive/full_model_abundance_LTO_tune.rds")
#best tune was:
# mtry : 1
# splitrune : variance
# min.node.size : 5
#"TX_0_0"   "SWV_0_3"  "FG_0_1"   "NDVI_0_5" "ALT" vars used to run preditions on 2000-2025 data
###############################################################

