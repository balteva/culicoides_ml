library(tidyverse) 
library(caret) 
library(CAST) 
library(ranger) 
library(correlation) 
library(fuzzySim) 


#check mod_tuned$bestTune from the output presence model to define in my_grid


###opening files
#cv with location LLO
multiv_model_presence_LLO <- readRDS("./models/presence/TX_TN_EVI/5PARES_presence_LLO_TX_EVI.rds")


#CV with LTO
multiv_model_presence_LTO <- readRDS("./models/presence/TX_TN_EVI/5PARES_presence_LTO_TX_EVI.rds")



###interpretable presence model without cv

#mod_tuned$bestTune 
model_presence_LLO <- multiv_model_presence_LLO[[1]] #### sum up of presence model 
#for nbindiv
# > model_presence_LLO$bestTune 
# mtry  splitrule min.node.size
# 4    3 extratrees             1
# for femelles
# > model_presence_LLO$bestTune
# mtry  splitrule min.node.size
# 4    3 extratrees             1
#for pares
# > model_presence_LLO$bestTune
# mtry  splitrule min.node.size
# 2    2 extratrees             1
model_presence_LTO <- multiv_model_presence_LTO[[1]]
#for nbindiv
# > model_presence_LTO$bestTune
# mtry  splitrule min.node.size
# 2    2 extratrees             1
#for femelles
# > model_presence_LTO$bestTune
# mtry  splitrule min.node.size
# 2    2 extratrees             1
#for pares
# > model_presence_LTO$bestTune
# mtry  splitrule min.node.size
# 2    2 extratrees             1



#check mod_tuned$bestTune from the output presence model to define in my_grid
df_model <- read.csv("./data/predictive_femelles_pares_obscot_RF.csv")

#predictors_presence_nbindiv <-c("TX_0_4", "EVI_0_3","SWV_0_4", "FG_4_6","ALT", "OVI","BETAIL")
#predictors_presence_femelles <-c("TX_0_4", "EVI_0_5","SWV_0_2", "FG_0_5","ALT", "OVI","BETAIL")
predictors_presence_pares <-c("TN_0_0", "EVI_0_0","SWV_0_2", "FG_0_5","ALT", "OVI","BETAIL")



df_model_presence <- df_model %>%
  mutate(year=year(as.Date(DATE)))%>%
  dplyr::select("ID_SITE", "ECO_CLI","DATE", "Cell", "year",  "NBINDIV", "PRES_PARES", predictors_presence_pares) %>%
  mutate(across(ID_SITE:year, as.factor))%>%
  filter(!is.na(EVI_0_0))

my_grid <- expand.grid(
  mtry = 2, splitrule = "extratrees",min.node.size = 1) #model_presence_LTO$bestTune


tr = trainControl(method="none", ## Definition of method sampling: cross validation
                  summaryFunction = twoClassSummary,#comboSummary, ## Calcul of ROC and AUC
                  classProbs = TRUE,
                  savePredictions = 'final',
                  verboseIter = FALSE)

mod_presence_full <- caret::train(
  x = df_model_presence[,predictors_presence_pares], 
  y = df_model_presence$PRES_PARES, 
  method = "ranger", tuneGrid=my_grid,
  trControl = tr, 
  metric = "ROC",
  maximize = TRUE,  
  preProcess = c("center","scale"),
  importance = "permutation",
  local.importance = "TRUE") 



saveRDS(mod_presence_full,"./5PARES_presence_model_LTO_tune.rds" )
