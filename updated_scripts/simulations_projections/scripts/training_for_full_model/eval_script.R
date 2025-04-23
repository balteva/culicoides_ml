##EVAL SCRIPT



library(tidyverse) 
library(caret) 
library(CAST) 
library(ranger) 
library(correlation) 
library(fuzzySim) 



setwd("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/updated_scripts/simulations_projections/")






#############
multiv_model_abundance_LLO <- readRDS("./models/abundance_predictive/reduced_abundance_LLO.rds")
multiv_model_abundance_LTO <- readRDS("./models/abundance_predictive/reduced_abundance_LTO.rds")

#CV with LTO

#results of LLO
model_abundance_LLO <- multiv_model_abundance_LLO[[1]] #### sum up of presence model
df_cv_abundance_LLO <- multiv_model_abundance_LLO[[2]]  #### data frame with prediction 

df_mod_abundance_LLO<- multiv_model_abundance_LLO[[3]] #### data frame which was used to build the model

##results of LTO
df_cv_abundance_LTO <- multiv_model_abundance_LTO[[2]]  #### data frame with prediction 

df_cv_abundance2 <- df_cv_abundance_LLO %>% ## separation of prediction in different groups according to the number predicted to better wizualisation
  mutate(obs = exp(obs)) %>%
  mutate(pred = exp(pred)) %>%
  mutate(rep = abs(obs - pred)/obs) %>% #wtf is rep
  mutate(residuals = obs - pred) %>% 
  mutate(year = year(as.Date(DATE)))


df_metrics_perf <- df_cv_abundance2  %>% 
  mutate(year = year(as.Date(DATE))) %>%
  group_by(Cell) %>% #ECO_CLI #or year
  summarise(mae = round(MLmetrics::MAE(y_true = obs ,y_pred = pred),2),
            mse =  round(MLmetrics::MSE(y_true = obs ,y_pred = pred),2),
            rmse =  round(MLmetrics::RMSE(y_true = obs ,y_pred = pred),2),
            mape =  round(MLmetrics::MAPE(y_true = obs ,y_pred = pred),2),
            r2 =  round(MLmetrics::R2_Score(y_true = obs, y_pred = pred),2),
            spearman = round(cor(obs, pred, method="spearman"),2),
            pearson = round(cor(obs, pred, method ="pearson"),2),
            n=n()) %>%
  as_tibble()

