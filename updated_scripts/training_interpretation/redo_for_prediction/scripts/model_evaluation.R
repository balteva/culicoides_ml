library(tidyverse) 
library(iml) 
library(patchwork) 
library(lubridate)
library(sf)
library(RColorBrewer)
library(fst)
library(caret)

library(precrec) #for ROC curves
library(MLmetrics)

setwd("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/updated_scripts/training_interpretation")

grid <- st_read("./data/traps_within_grid_only.gpkg")
grid <- st_transform(grid,4326)

france <- st_read("../simulations_projections/new_data/france_ecoclimatic_zones.gpkg")
france <- st_transform(france,4326)

#cv with location LLO
historical_model_abundance_LLO <- readRDS("./models/abundance/TX_EVI/1NBINDIV_abundance_LLO_TX_EVI.rds")
historical_model_abundance_LTO <- readRDS("./models/abundance/TX_EVI/1NBINDIV_abundance_LTO_TX_EVI.rds")



cv_abundance_LLO <- historical_model_abundance_LLO[[2]]  #### data frame with prediction 
cv_abundance_LTO <- historical_model_abundance_LTO[[2]]

# model <-historical_model_abundance_LTO[[1]]
# imp <- varImp(model, scale=F)
# plot(imp)


####EVAL with main focus being LLO

df_cv_abundance2 <- cv_abundance_LTO %>% 
  mutate(pred = exp(pred)) %>%
  mutate(rep = abs(obs - pred)/obs) %>% #wtf is rep
  mutate(residuals = obs - pred) %>% 
  mutate(year = year(as.Date(DATE)))


df_metrics_perf <- df_cv_abundance2  %>%
  group_by(Cell,year) %>% #ECO_CLI #or year
  #group_by(Cell, year) %>%
  summarise(mae = round(MLmetrics::MAE(y_true = obs ,y_pred = pred),2),
            mse =  round(MLmetrics::MSE(y_true = obs ,y_pred = pred),2),
            rmse =  round(MLmetrics::RMSE(y_true = obs ,y_pred = pred),2),
            mape =  round(MLmetrics::MAPE(y_true = obs ,y_pred = pred),2),
            r2 =  round(MLmetrics::R2_Score(y_true = obs, y_pred = pred),2),
            spearman = round(cor(obs, pred, method="spearman"),2),
            pearson = round(cor(obs, pred, method ="pearson"),2),
            n=n()) %>%
  as_tibble()%>%
  select(c(Cell,year, mae,spearman,pearson, n))
  #select(c(ECO_CLI, mae,spearman,pearson, n))

grid1 <- grid %>%
  left_join(df_metrics_perf, by="Cell")

# mean_plot <- round(mean(df_metrics_perf$spearman),2)
# median_plot <-round(median(df_metrics_perf$spearman),2)

metrics <- df_cv_abundance2  %>% 
  mutate(year = year(as.Date(DATE))) %>%
  group_by(Cell) %>% 
  summarise(mae = round(MLmetrics::MAE(y_true = obs ,y_pred = pred),2),
            spearman = round(cor(obs, pred, method="spearman"),2),
            pearson = round(cor(obs, pred, method ="pearson"),2),
            n=n()) %>%
  filter(spearman>=0)%>%
  summarise(mean_total=round(mean(spearman), 2),
            median_total=round(median(spearman),2))


ggplot()+
  geom_sf(data=france, color="cornsilk4", linewidth=0, alpha=1) +
  geom_sf(data =grid1, aes(fill = spearman), color = "black", alpha = 0.4) +
  scale_fill_gradient(low = "red", high = "green3", na.value  = "grey",
                      #oob = scales::squish,
                      limits = c(0, 1))+
  theme_minimal()+
  labs(title="Abundance model performance (LTO CV)\n(vars: EVI + TX + BETAIL)",
       subtitle = paste("Spearman's mean per cell =", metrics$mean_total, "\nSpearman's median per cell =", metrics$median_total))+
  theme(legend.position = 'right',
        plot.subtitle =element_text(hjust=0.5, size=10),
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))+
  facet_wrap(~year)


##################"
#for presence absence models
###################
historical_model_presence_LLO <- readRDS("../simulations_projections/models/EVI/presence/TX_TN_EVI/5PARES_presence_LLO_TX_EVI.rds")
historical_model_presence_LTO <- readRDS("../simulations_projections/models/EVI/presence/TX_TN_EVI/5PARES_presence_LTO_TX_EVI.rds")

###
model_LLO <- historical_model_presence_LLO[[1]]
model_LTO <- historical_model_presence_LTO[[1]]
imp_LTO <- varImp(model_LTO, scale=F)
plot(imp_LTO)


###

library(precrec)

df_cv_presence_LLO <- historical_model_presence_LLO[[2]]
df_cv_presence_LTO <- historical_model_presence_LTO[[2]]

df_LTO_presence <- df_cv_presence_LTO %>% ## grouping in a same dataframe both predictions with LLO and LTO cross validation 
  select(ID_SITE, DATE, pred) %>%
  rename(pred_year=pred) #renaming LTO pred to indicate the cv was by year

df_fin_cv<-merge(df_cv_presence_LLO, df_LTO_presence, by=c("ID_SITE", "DATE")) %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=year(DATE), week=week(DATE))




roc_plot_func <- function(df, var){
  roc_plots <-  list()
  
  df_new <- df %>%
    mutate(!!sym(var) := "ALL ZONES") %>%
    rbind(df)  
  
  for (i in unique(df_new[[var]])) {  
    df_temp <- df_new %>%
      filter(df_new[[var]] == i)
    
    LLTO_scores <- precrec::join_scores(df_temp$pred, df_temp$pred_year)
    
    i_mmdat <- mmdata(LLTO_scores, df_temp$obs, modnames=c("LLO", "LTO"))
    AUC_i_LLO <- MLmetrics::AUC(df_temp$pred, df_temp$obs)
    AUC_i_LTO <- MLmetrics::AUC(df_temp$pred_year, df_temp$obs)
    i_obj <- precrec::evalmod(i_mmdat)
    roc_plot <- autoplot(i_obj, curvetype= c("ROC"))+
      geom_line(linewidth=1)+
      ggtitle(paste0("Presence model ROC curve for ", i, "\n LLO AUC = ", round(AUC_i_LLO, 2), "\n LTO AUC = ", round(AUC_i_LTO, 2), "")) + 
      theme(axis.title.x = element_text(size = 10, face="bold"),
            axis.title.y = element_text(size = 10, face="bold"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5, size= 10, face='bold'))
    roc_plots[[i]] <- roc_plot
  }
  return(roc_plots)
}


roc_plots <- roc_plot_func(df_fin_cv, "ECO_CLI")
roc_plots_all <- patchwork::wrap_plots(roc_plots, ncol = 3) + plot_annotation(title = "Presence Model Evaluation Per Ecoclimatic Zone")


#### Second step: Model validation plots: ROC  (simple plot)

AUC = MLmetrics::AUC(df_cv_presence_LTO$pred, df_cv_presence_LTO$obs) ## To calculate the AUC

precrec_obj <- precrec::evalmod(scores = df_cv_presence_LTO$pred, labels = df_cv_presence_LTO$obs)

plot_validation_presence <- autoplot(precrec_obj,curvetype = c("ROC")) + 
  ggtitle(paste0("Presence model : ROC curve (AUC = ",round(AUC,2),")")) +     
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))


