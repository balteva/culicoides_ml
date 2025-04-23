library(tidyverse) 
library(caret) 
library(CAST) 
library(ranger) 
library(correlation) 
library(fuzzySim) 


#check mod_tuned$bestTune from the output presence model to define in my_grid
df_model <- read.csv("D:/ieva/models/predictive_df_model_for_RF.csv")

predictors_presence <-c("TX_0_4", "SWV_0_4", "NDVI_0_0","ALT", "OVI","BETAIL")#OUVERTURE_BAT, PLUIEDEBUT


df_model_presence <- df_model %>%
  mutate(year=year(as.Date(DATE)))%>%
  dplyr::select("idpointdecapture", "ID_SITE", "ECO_CLI","DATE", "Cell", "year",  "NBINDIV", "PRES_CUL", predictors_presence) %>%
  mutate(PRES_CUL=as.factor(PRES_CUL)) %>%
  mutate(across(ID_SITE:year, as.factor))%>%
  filter(!is.na(NDVI_0_0))

cv_col <- "Cell"


#training model using LLO
 
#### Last step: to put predictions on same data frame
df_model_presence$rowIndex <- seq(1,nrow(df_model_presence),1)
df_cv_presence_LLO <- mod_presence_LLO$pred %>%
  left_join(df_model_presence) %>%
  dplyr::select(idpointdecapture, ID_SITE, ECO_CLI, DATE, Cell, Presence ,pred, obs) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)

res_multiv_model_presence_LLO <- list(model = mod_presence_LLO, df_cv = df_cv_presence_LLO, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence_LLO,"./presence_predictive_LLO.rds")



## To parameter the model: leave-one-year_out cross validation
cv_col <- "year"
## It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)
indices_cv <- CAST::CreateSpacetimeFolds(df_model_presence, timevar = cv_col, k = length(unique(unlist(df_model_presence[,cv_col])))) 
## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
tr = trainControl(method="cv", 
                  index = indices_cv$index,  
                  indexOut = indices_cv$indexOut, 
                  summaryFunction = twoClassSummary,#
                  classProbs = TRUE,
                  savePredictions = 'final',
                  verboseIter = FALSE
)
## Third step: realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the ROC
mod_presence_LTO <- caret::train(x = df_model_presence[,predictors_presence], y = df_model_presence$PRES_CUL, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE") 


## Adding prediction with session cross validation 
df_cv_presence_LTO<-mod_presence_LTO$pred %>%
  left_join(df_model_presence) %>% 
  dplyr::select(idpointdecapture, ID_SITE, ECO_CLI, DATE, Cell, year, Presence , pred, obs) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)
res_multiv_model_presence_LTO <- list(model = mod_presence_LTO, df_cv = df_cv_presence_LTO, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence_LTO,"./presence_predictive_LTO.rds")

###opening files
#cv with location LLO
multiv_model_presence_LLO <- readRDS("./presence_predictive_LLO.rds")


#CV with LTO
multiv_model_presence_LTO <- readRDS("./presence_predictive_LTO.rds")



###interpretable presence model without cv

#mod_tuned$bestTune 
model_presence_LTO <- multiv_model_presence_LTO[[1]] #### sum up of presence model 
# > model_presence_LLO$bestTune
# mtry  splitrule min.node.size
# 2    2 extratrees             1

# > model_presence_LTO$bestTune
# mtry  splitrule min.node.size
# 2    2 extratrees             1




#check mod_tuned$bestTune from the output presence model to define in my_grid
df_model <- read.csv("D:/ieva/models/predictive_df_model_for_RF.csv")

#predictors_presence <-c("TN_0_0", "SWV_2_5","FG_4_6", "NDVI_0_1","ALT", "OVI","BETAIL")#OUVERTURE_BAT, PLUIEDEBUT
predictors_presence <-c("TX_0_4", "SWV_0_4", "NDVI_0_0","ALT", "OVI","BETAIL")


df_model_presence <- df_model %>%
  mutate(year=year(as.Date(DATE)))%>%
  dplyr::select("idpointdecapture", "ID_SITE", "ECO_CLI","DATE", "Cell", "year",  "NBINDIV", "PRES_CUL", predictors_presence) %>%
  mutate(across(ID_SITE:year, as.factor))%>%
  filter(!is.na(NDVI_0_0))

my_grid <- expand.grid(
  mtry = 2, splitrule = "extratrees",min.node.size = 1) #model_presence_LTO$bestTune


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



saveRDS(mod_presence_full,"./full_model_tuned.rds" )
