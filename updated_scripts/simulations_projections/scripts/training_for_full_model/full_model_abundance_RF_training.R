library(tidyverse) 
library(caret) 
library(CAST) 
library(ranger) 
library(correlation) 
library(fuzzySim) 

predictors_abundance <- c("TX_0_0","SWV_0_3","FG_0_1","NDVI_0_5", "ALT")

df_model <- read.csv("./df_model_for_RF.csv")

df_model_abundance <- df_model %>%
  mutate(year = year(as.Date(DATE)))%>%
  filter(NBINDIV>0) %>%
  dplyr::select("idpointdecapture", "ID_SITE","DATE","ECO_CLI","Cell", "year", "NBINDIV", "PRES_CUL", predictors_abundance)%>%
  mutate(across(ID_SITE:year, as.factor))%>%
  filter(!is.na(NDVI_0_5), !is.na(FG_0_1))

df_model_abundance$NBINDIV <- log(df_model_abundance$NBINDIV) 
cv_col <- "Cell"

indices_cv <- CAST::CreateSpacetimeFolds(df_model_abundance, spacevar = cv_col,k = length(unique(unlist(df_model_abundance[,cv_col])))) 

tr = trainControl(method="cv",
                  index = indices_cv$index, 
                  indexOut = indices_cv$indexOut,
                  savePredictions = 'final')


#### Third step: realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the MAE
mod_abundance_LLO <- caret::train(x = df_model_abundance[,predictors_abundance], y = df_model_abundance$NBINDIV, method = "ranger", tuneLength = 10, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")

#### Last step: to put predictions on same data frame
df_model_abundance$rowIndex <- seq(1,nrow(df_model_abundance),1)

df_cv_abundance_LLO <- mod_abundance_LLO$pred %>%
  left_join(df_model_abundance) %>%
  dplyr::select(pred,obs,Cell,idpointdecapture, ID_SITE, ECO_CLI, DATE, year)

res_multiv_model_abundance_LLO <- list(model = mod_abundance_LLO, df_cv = df_cv_abundance_LLO, df_mod = df_model_abundance) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_abundance_LLO, "./LLTO_abundance_predictive/reduced_abundance_LLO.rds")

## need to save!!

## To parameter the model: LTO CV


cv_col <- "year"
##It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)
indices_cv <- CAST::CreateSpacetimeFolds(df_model_abundance, timevar = cv_col,k = length(unique(unlist(df_model_abundance[,cv_col])))) 
## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
tr = trainControl(method="cv",
                  index = indices_cv$index, 
                  indexOut = indices_cv$indexOut,
                  savePredictions = 'final')
## realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the MAE
mod_abundance_LTO <- caret::train(x = df_model_abundance[,predictors_abundance], y = df_model_abundance$NBINDIV, method = "ranger", tuneLength = 10, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")

#### Last step: to put predictions on same data frame
mod_abundance_LTO$rowIndex <- seq(1,nrow(df_model_abundance),1)

df_cv_abundance_LTO <- mod_abundance_LTO$pred %>%
  left_join(df_model_abundance) %>%
  dplyr::select(pred,obs,Cell,idpointdecapture, ID_SITE, ECO_CLI, DATE, year)

res_multiv_model_abundance_LTO <- list(model = mod_abundance_LTO, df_cv = df_cv_abundance_LTO, df_mod = df_model_abundance)  ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_abundance_LTO, "./LLTO_abundance_predictive/reduced_abundance_LTO.rds")



#############
multiv_model_abundance_LLO <- readRDS("./LLTO_abundance_predictive/reduced_abundance_LLO.rds")


#CV with LTO
multiv_model_abundance_LTO <- readRDS("./LLTO_abundance_predictive/reduced_abundance_LTO.rds")


#results of LLO
model_abundance_LTO <- multiv_model_abundance_LTO[[1]] #### sum up of presence model

# > model_abundance_LLO$bestTune
# mtry  splitrule min.node.size
# 4    3 extratrees             5
# > model_abundance_LTO$bestTune
# mtry splitrule min.node.size
# 1    2  variance             5
my_grid <- expand.grid(
  mtry = 1, splitrule = "variance",min.node.size = 5)



##### full model
tr = trainControl(method="none")
## realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the MAE
mod_abundance_final <- caret::train(x = df_model_abundance[,predictors_abundance], y = df_model_abundance$NBINDIV, method = "ranger", tuneGrid=my_grid, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")



saveRDS(mod_abundance_final,"./LLTO_abundance_predictive/full_model_abundance_LTO_tune.rds")




