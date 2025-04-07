###interpretable presence model without cv

#mod_tuned$bestTune 
model_presence_LTO <- multiv_model_presence_LTO[[1]] #### sum up of presence model 
###mtry  splitrule min.node.size
#12    7 extratrees             1 for LLO presence model
# model_presence_LTO$bestTune
#mtry  splitrule min.node.size
#4    3 extratrees             1 for LTO presence model




#check mod_tuned$bestTune from the output presence model to define in my_grid
df_model <- read.csv("./updated_scripts/df_model_for_RF.csv")

predictors_presence <- c("TN_0_0","SWV_2_5","FG_4_6","NDVI_0_1","ALT","OVI","BETAIL","OUVERTURE_BAT", "PLUIEDEBUT")


df_model_presence <- df_model %>%
  mutate(year=year(as.Date(DATE)))%>%
  dplyr::select("idpointdecapture", "ID_SITE", "ECO_CLI","DATE", "Cell", "year",  "NBINDIV", "PRES_CUL", predictors_presence) %>%
  mutate(across(OUVERTURE_BAT:PLUIEDEBUT, as.factor))%>% 
  mutate(across(ID_SITE:year, as.factor))%>%
  filter(!is.na(OUVERTURE_BAT), !is.na(PLUIEDEBUT), !is.na(NDVI_0_1), !is.na(FG_4_6))

my_grid <- expand.grid(
  mtry = floor(sqrt(ncol(df_model_presence[,predictors_presence]))), splitrule = "gini",min.node.size = 1)
  

tr = trainControl(method="none", ## Definition of method sampling: cross validation
                  summaryFunction = twoClassSummary,#comboSummary, ## Calcul of ROC and AUC
                  classProbs = TRUE,
                  savePredictions = 'final',
                  verboseIter = FALSE)

mod_presence_full <- caret::train(
  x = df_model_presence[,predictors_presence], 
  y = df_model_presence$PRES_CUL, 
  method = "ranger", tuneGrid=my_grid,
  trControl = tr, 
  metric = "ROC",
  maximize = TRUE,  
  preProcess = c("center","scale"),
  importance = "permutation",
  local.importance = "TRUE") 
