library(tidyverse) 
library(iml) 
library(patchwork) 
library(precrec) 
library(lubridate)



# grid <- read.csv("../../qgis/culicoides_point_locations_grid_150_150.csv")%>%
#   select(ID_SITE, Cell, ECO_CLI)

#cv with location LLO
multiv_model_abundance_LLO <- readRDS("./updated_scripts/models/alt/RF_model_abundance_LLO_interpretation.rds")


#CV with LTO
multiv_model_abundance_LTO <- readRDS("./updated_scripts/models/alt/RF_model_abundance_LTO_interpretation.rds")


#results of LLO
model_abundance_LLO <- multiv_model_abundance_LLO[[1]] #### sum up of presence model
df_cv_abundance_LLO <- multiv_model_abundance_LLO[[2]]  #### data frame with prediction 

df_mod_abundance_LLO<- multiv_model_abundance_LLO[[3]] #### data frame which was used to build the model

##results of LTO
df_cv_abundance_LTO <- multiv_model_abundance_LTO[[2]]  #### data frame with prediction 



# LTO == "maroon2"
# LLO == "deepskyblue3"
# Observed = "grey70"
# ploting with LLO CV
plot_eval_abundance_model <- df_cv_abundance_LTO %>%
  mutate(obs=exp(obs),pred=exp(pred)) %>%
  mutate(week=week(as.Date(DATE)))%>%
  dplyr::group_by(week, year) %>% #ECO_CLI
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
  ggplot(aes(x=week, y = value, color = name)) +
  #geom_point() + 
  geom_line(linewidth = 0.8) + 
  facet_wrap(~year, ncol=2) + #~ECO_CLI #ncol=2 if only by year
  theme_bw() + 
  scale_colour_manual(values=c("grey70","maroon2"),na.translate = F) + 
  scale_y_log10() +
  scale_x_continuous(breaks = seq(4,52,4)) +
  xlab("week") +
  ylab("Mean Abundance (log scale)") + 
  labs(color='Number of C. obsoletus/scoticus') + 
  theme(legend.position="bottom") + 
  ggtitle('Abundance models : observed vs. predicted values (LTO CV)')+
  theme(axis.title.x = element_text(size = 12, face="bold"),
        axis.title.y = element_text(size = 12, face="bold"),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5, size= 14, face='bold'))


####################### LLO vs LTO in one (abundance)

df_temp <- df_cv_abundance_LTO %>% 
  select(idpointdecapture, pred) %>%
  rename(pred_year=pred) #marking the predictions of LTO differently to distinguish in joint df

df_fin_cv<-merge(df_cv_abundance_LLO, df_temp, by="idpointdecapture")
# LTO == "maroon2"
# LLO == "deepskyblue3"
# Observed = "grey70"

plot_eval_abundance_model <- df_fin_cv %>%
  mutate(week=week(as.Date(DATE)), year=year(as.Date(DATE))) %>%
  mutate(obs=exp(obs),pred=exp(pred), pred_year=exp(pred_year)) %>%
  dplyr::group_by(ECO_CLI,year, week) %>%    
  dplyr::summarise(pred = mean(pred), obs = mean(obs), pred_year=mean(pred_year)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs', 'pred_year')) %>%
  mutate(name = case_when(name=="pred"~"Predicted with LLO",name=='pred_year'~"Predicted with LTO", name=="obs"~"Observed")) %>%
  ggplot(aes(x=week, y = value, color = name)) +
  #geom_point() + 
  geom_line(linewidth = 0.8) + 
  facet_wrap(~ECO_CLI~year) + # ncol=2
  theme_bw()+
  scale_colour_manual(values=c("grey70","deepskyblue3","maroon2"),na.translate = F) + 
  scale_x_continuous(breaks = seq(4,52,4)) +
  scale_y_log10() +
  xlab("week") +
  ylab("Mean Abundance (log scale)") + 
  labs(color='Number of C. obsoletus/scoticus ') + 
  theme(legend.position="bottom") + 
  ggtitle('Abundance models : observed vs. predicted values with LLO or LTO CV') +
  theme(axis.title.x = element_text(size = 12, face="bold"),
        axis.title.y = element_text(size = 12, face="bold"),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5, size= 14, face='bold'))


#### Second step: Model validation plots: visually with the RMSE

df_cv_abundance2 <- df_cv_abundance_LLO %>% ## separation of prediction in different groups according to the number predicted to better wizualisation
  mutate(obs = exp(obs)) %>%
  mutate(pred = exp(pred)) %>%
  mutate(rep = abs(obs - pred)/obs) %>% #wtf is rep
  mutate(residuals = obs - pred) %>% 
  mutate(year = year(as.Date(DATE)))

  # mutate(groups = case_when(
  #   obs>=1 & obs<=10 ~ "1-10",
  #   obs>10 & obs<=30 ~ "10-30",
  #   obs>30 & obs<=60 ~ "30-60",
  #   obs>60 & obs<=100 ~ "60-100",
  #   obs>100 & obs<=200~ "100-200",
  #   obs>200 ~ ">200")) %>%
  # mutate(groups = fct_relevel(groups, c("1-10","10-20","20-30","30-100","100-200", ">200")))

# Since im more concerned of how well my model captures trends, i'm not exponentiating the predictions and observations for the eval metrics
# And instead I compare the raw outputs
  df_metrics_perf <- df_cv_abundance2  %>% 
  mutate(year = year(as.Date(DATE))) %>%
  group_by(year) %>% #ECO_CLI #or year
  summarise(mae = round(MLmetrics::MAE(y_true = obs ,y_pred = pred),2),
            mse =  round(MLmetrics::MSE(y_true = obs ,y_pred = pred),2),
            rmse =  round(MLmetrics::RMSE(y_true = obs ,y_pred = pred),2),
            mape =  round(MLmetrics::MAPE(y_true = obs ,y_pred = pred),2),
            r2 =  round(MLmetrics::R2_Score(y_true = obs, y_pred = pred),2),
            spearman = round(cor(obs, pred, method="spearman"),2),
            pearson = round(cor(obs, pred, method ="pearson"),2),
            n=n()) %>%
  as_tibble()

  
  
# ## Violin plots with Rsquared
# plot_validation_abundance <- ggplot() + 
#   geom_violin(data = df_cv_abundance2, aes(x=factor(year) , y=residuals)) + 
#   stat_summary(data = df_cv_abundance2, aes(x=factor(year) , y=residuals), fun=median, geom="line", size=2, color="red") +
#   theme_minimal() + 
#   #facet_wrap(~year, scales="free_y")+
#   xlab("Year") + 
#   ylab("Residuals (obs - pred) in logscale ") + 
#   geom_label(data = df_metrics_perf,
#              size = 2.5,
#              mapping = aes(x = factor(year), y = max(df_cv_abundance2$residuals,na.rm = T), label = paste0('R-squared = ',r2,'\nn = ',n),
#                            vjust = 1)) +
#   scale_y_log10() +
#   ggtitle("Abundance model: R - squared by year (LLO CV)") + 
#   geom_hline(yintercept=0, linetype="dashed") + 
#   theme(axis.title.x = element_text(size = 10, face="bold"),
#         axis.title.y = element_text(size = 10, face="bold"),
#         legend.position="bottom",
#         plot.title = element_text(hjust = 0.5, size= 12, face='bold'))
# #########################


#Plots for pred vs obs (Paul's idea :) )

eval_plot_LLO <- df_cv_abundance_LLO %>% 
  mutate(year=year(as.Date(DATE)))%>%
  ggplot(aes(x=exp(obs), y=exp(pred)))+
  geom_point(color="grey70", size=1.2) + #aes(color=ECO_CLI))+
  theme_bw() +
  facet_grid(.~year, scales="fixed")+ #ECO_CLI
  theme(strip.text.x = element_text(size = 14, colour = "black", face="bold"))+
  #geom_smooth(color="maroon2", linewidth=1, se=T, fill="lightpink")+ #for LTO
  geom_smooth(color="deepskyblue3", linewidth=1, se=T, fill="lightblue")+ #for LLO
  scale_y_log10() +
  scale_x_log10(labels = scales::number_format(scale = 1, suffix = "", accuracy = 1)) +
  geom_label(data = df_metrics_perf,size = 4,
             aes(x = mean(exp(df_cv_abundance_LTO$obs), na.rm = TRUE),  
                 y = max(exp(df_cv_abundance_LTO$pred), na.rm = TRUE),
                label = paste0("Spearman's ρ = ", spearman, '\n',"MAE = ", mae,'\n', "n = ", n), vjust = 1, hjust = 0.5))+ #"R2 = ", r2,'\n',
  xlab("Observations") + 
  ylab("Predictions") +
  ggtitle("Abundance LLO model evaluation: predicted vs. observed value")+
  theme(axis.title.x = element_text(size = 12, face="bold"),
        axis.text.x = element_text(size = 12, face="bold"),
        axis.title.y = element_text(size = 12, face="bold"),
        axis.text.y = element_text(size = 12, face="bold"),
        plot.title = element_text(hjust = 0.5, size= 14, face='bold'))

##finished here
#### Third step: VIP  

model = multiv_model_abundance_LTO$model
df = multiv_model_abundance_LTO$df_mod
df_cv <-  multiv_model_abundance_LTO$df_cv

## To select the importance of each variable from the model and to transform in data frame
imp <- model$finalModel$variable.importance
imp <- as.data.frame(imp)
imp$var <- rownames(imp)

################
library(caret)
importance <- varImp(model, scale=F)
plot(importance) 



## To arrange by order of importance and to categorize by type of variable
imp <- imp %>%
  dplyr::rename(importance = imp) %>%
  mutate(label = forcats::fct_reorder(var, importance)) %>%
  arrange(-importance) %>% 
  mutate(Type = case_when(var %in% c("UM_0_0", "TEMPMINI", "EVI_1_5") ~ "Meteorological",
                          var %in% c("VENTDEBUT", "PLUIEDEBUT") ~ "Micro-climatic",
                          var %in% c("ALT","CLC_level2") ~ "Landscape",
                          var %in% ("ELEV_OVIN") ~ "Livestock"))%>%
  mutate(var = case_when(var == "UM_0_0" ~ "Relative Humidity",
                         var == "TEMPMINI" ~ "Minimum Temperature",
                         var == "VENTDEBUT" ~ "Wind strength before collection",
                         var == "PLUIEDEBUT" ~"Rain before collection",
                         var == "ALT" ~ "Altitude",
                         var == "ELEV_OVIN" ~ "Cattle farm",
                         var == "CLC_level2" ~ "Corine land cover type ",
                         var == "EVI_1_5" ~ "Enhanced Vegetation Index 2 - 6 weeks before collection"))


## To plot the importance of the variables
plot_imp_abundance_LTO <- ggplot(imp, aes(x = importance , y = label, label = var, fill = Type)) +
  geom_bar(position = 'dodge', stat="identity", width = 0.6) + 
  theme_bw() + 
  geom_text(size=3,position = position_dodge(0.9),hjust=-0.1) +
  theme(plot.title = element_text(size = 12, face="bold", hjust=0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 7),
        legend.position="bottom") +
  ylab("") + 
  xlab("") +
  xlim(NA,max(imp$importance, na.rm = T) + max(imp$importance, na.rm = T)) +
  labs(title = " Presence model : Variable Importance Plot")
#### Last step: PDP  

## To create a function which predicts the abundance according different variables

pred_wrapper_reg <- function(object, newdata) {
  p <- predict(object, newdata = newdata)
  c("avg" = mean(p))
  #c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
}

pdps <- list()
for(i in 1:length(imp$var)){
  pd <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_reg, train = df)
  pd$yhat[which(pd$yhat<0)] <-0 
  p <- autoplot(pd, smooth = T)  
  dat1 <- ggplot_build(p)$data[[1]]
  dat2 <- ggplot_build(p)$data[[2]]
  pdps[[i]] <- ggplot() + 
    geom_line(data = dat1, aes(x = x, y = y), size = 0.3, colour = "black", alpha = 0.4) +   ## smooth the observed data
    geom_line(data = dat2, aes(x = x, y = y), size = 0.5, colour = "#009E73") +  # smooth the prediction data
    geom_rug(data = df, aes_string(x = imp$var[i]), sides="b", length = unit(0.05, "npc")) + 
    theme_bw() + 
    xlab(imp$var[i]) + 
    ylab("") + 
    ylim(c(4,11))
  
}

plot_pdps_abundance <- patchwork::wrap_plots(pdps) + plot_annotation(title = "Abundance model : PDP") ## To put all variables together

