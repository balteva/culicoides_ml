library(tidyverse) 
library(caret) 
library(CAST) 
library(ranger) 
library(correlation) 
library(fuzzySim) 

predictors_abundance <- c("TX_0_0","SWV_0_2","FG_0_5","EVI_0_5", "ALT", "OVI", "BETAIL")

df_model <- read.csv("./data/predictive_femelles_pares_obscot_RF.csv")

  df_model_abundance <- df_model %>%
  mutate(year = year(as.Date(DATE)))%>%
  filter(NBINDIV>0) %>%
  dplyr::select("ID_SITE","DATE","ECO_CLI","Cell", "year", "NBINDIV", "PRES_OBSCOT", predictors_abundance)%>%
  mutate(across(ID_SITE:year, as.factor))%>%
  filter(!is.na(EVI_0_5), !is.na(FG_0_5))

df_model_abundance$NBINDIV <- log(df_model_abundance$NBINDIV) 

#############
multiv_model_abundance_LLO <- readRDS("./models/abundance/TX_EVI/1NBINDIV_abundance_LLO_TX_EVI.rds")


#CV with LTO
multiv_model_abundance_LTO <- readRDS("./models/abundance/TX_EVI/1NBINDIV_abundance_LTO_TX_EVI.rds")

model_abundance_LLO <- multiv_model_abundance_LLO[[1]]
# > model_abundance_LLO$bestTune
# mtry  splitrule min.node.size
# 4    3 extratrees             5

model_abundance_LTO <- multiv_model_abundance_LTO[[1]] #### sum up of presence model
# > model_abundance_LTO$bestTune
# mtry splitrule min.node.size
# 1    2  variance             5

my_grid <- expand.grid(#LTO BASED
  mtry = 2, splitrule = "variance",min.node.size = 5)


##### full model
tr = trainControl(method="none")
## realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the MAE
mod_abundance_final <- caret::train(x = df_model_abundance[,predictors_abundance], y = df_model_abundance$NBINDIV, method = "ranger", tuneGrid=my_grid, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")



saveRDS(mod_abundance_final,"./1NBINDIV_abundance_model_LTO_tune.rds")




