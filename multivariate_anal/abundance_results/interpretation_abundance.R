library(tidyverse) 
library(iml) 
library(patchwork) 
library(precrec) 
library(lubridate)

#install.packages("hrbrthemes")
library(hrbrthemes)
#forgot to add eco_cli column, doing it now

grid <- read.csv("../../data/culicoides_point_locations_grid_150_150.csv")%>%
  select(ID_SITE, ECO_CLI)

#cv with location LLO
multiv_model_abundance_LLO <- readRDS("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/abundance_results/res_multiv_model_abundance_LLO.rds")


#CV with LTO
multiv_model_abundance_LTO <- readRDS("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/abundance_results/res_multiv_model_abundance_LTO.rds")


#results of LLO
model_abundance_LLO <- multiv_model_abundance_LLO[[1]] #### sum up of presence model
df_cv_abundance_LLO <- multiv_model_abundance_LLO[[2]] %>% #### data frame with prediction 
  left_join(grid)
df_mod_abundance_LLO<- multiv_model_abundance_LLO[[3]] #### data frame which was used to build the model

##results of LTO
df_cv_abundance_LTO <- multiv_model_abundance_LTO[[2]] %>% #### data frame with prediction 
  left_join(grid)

# ploting with LLO CV
plot_eval_abundance_model <- df_cv_abundance_LLO %>%
  mutate(obs=exp(obs),pred=exp(pred)) %>%
  mutate(year=year(as.Date(DATE)), week=week(as.Date(DATE)))%>%
  dplyr::group_by(ECO_CLI, week, year) %>%    ## to sum up, grouping by trap, location and num session 
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
  ggplot(aes(x=week, y = value, color = name)) +
  #geom_point() + 
  geom_line(linewidth=1) + 
  facet_grid(.~ECO_CLI~year, scales = "free_y") + 
  theme_bw() + 
  scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F) + 
  scale_y_log10() +
  scale_x_continuous(breaks = seq(4,52,4)) +
  xlab("week") +
  ylab("mean abundance (log scale)") + 
  labs(color='Number of C. Obsoletux/Scoticus') + 
  theme(legend.position="bottom") + 
  ggtitle('Abundance models : observed vs. predicted values (LLO CV)')

#ggsave(filename = "C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/abundance_results/abundance_evaluation_LLO.pdf",plot =plot_eval_abundance_model, device = "pdf", width = 11, height = 8) ## to save
####################### raw plots below




## With site and session cross validation: plot with observation and prediction for the different site, trap and numero session

df_LTO_abund<-df_cv_abundance_LTO|> ## grouping in a same dataframe both predictions with site cross validation and session cross validation 
  select(idpointdecapture, pred)|>
  rename(pred_year=pred)

df_fin_cv<-merge(df_cv_abundance_LLO, df_LTO_abund, by="idpointdecapture")


plot_eval_abundance_model <- df_fin_cv %>%
  mutate(week=week(as.Date(DATE)), year=year(as.Date(DATE)))|>
  mutate(obs=exp(obs),pred=exp(pred), pred_year=exp(pred_year)) %>%
  dplyr::group_by(ECO_CLI, year, week) %>%    ## to sum up, grouping by trap, location and num session 
  dplyr::summarise(pred = mean(pred), obs = mean(obs), pred_year=mean(pred_year)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs', 'pred_year')) %>%
  mutate(name = case_when(name=="pred"~"Predicted with LLO",name=='pred_year'~"Predicted with LTO", name=="obs"~"Observed")) %>%
  ggplot(aes(x=week, y = value, color = name)) +
  #geom_point() + 
  geom_line(linewidth=1) + 
  facet_grid(ECO_CLI~year, scales = "free_y") + 
  theme_bw()+
  scale_colour_manual(values=c("#009E73","blue","#E69F00"),na.translate = F) + 
  scale_x_continuous(breaks = seq(4,52,4)) +
  scale_y_log10() +
  xlab("week") +
  ylab("mean abundance (log scale)") + 
  labs(color='Number of Ae. Albopictus') + 
  theme(legend.position="bottom") + 
  ggtitle('Abundance models : observed vs. predicted values with LLO or LTO CV')

####################### raw plots below
#### Second step: Model validation plots: visually with the RMSE

df_cv_abundance2 <- df_cv_abundance_LLO %>% ## separation of prediction in different groups according to the number predicted to better wizualisation
   mutate(obs = exp(obs)) %>%
   mutate(pred = exp(pred)) %>%
  mutate(rep = abs(obs - pred)/obs) %>%
  mutate(residuals = obs - pred) %>% 
  mutate(year = year(as.Date(DATE)))
  # mutate(groups = case_when(
  #   obs>=1 & obs<=10 ~ "1-10",
  #   obs>10 & obs<=20 ~ "10-20",
  #   obs>20 & obs<=30 ~ "20-30",
  #   obs>30 & obs<=100 ~ "30-100",
  #   obs>100 & obs<=200~ "100-200",
  #   obs>200 ~ ">200")) %>%
  # mutate(groups = fct_relevel(groups, c("1-10","10-20","20-30","30-100","100-200", ">200")))

df_metrics_perf <- df_cv_abundance2 %>% ## to evaluate different type of metrics
  group_by(year, ECO_CLI) %>%
  summarise(mae = round(MLmetrics::MAE(y_true = obs ,y_pred = pred),2),
            mse =  round(MLmetrics::MSE(y_true = obs ,y_pred = pred),2),
            rmse =  round(MLmetrics::RMSE(y_true = obs ,y_pred = pred),2),
            mape =  round(MLmetrics::MAPE(y_true = obs ,y_pred = pred),2),
            r2 =  round(MLmetrics::R2_Score(y_pred = pred,y_true = obs),2),
            n=n()) %>%
  as_tibble()

## To represent visually the RMSE according to the different category of prediction 
plot_validation_abundance <- ggplot() + 
  geom_violin(data = df_cv_abundance2, aes(x=ECO_CLI , y=residuals)) + 
  stat_summary(data = df_cv_abundance2, aes(x=ECO_CLI , y=residuals), fun=median, geom="point", size=2, color="black") +
  theme_minimal() + 
  facet_wrap(~year, scales="free_y")+
  xlab("Observed counts") + 
  ylab("Residuals (obs - pred) in log10 ") + 
  geom_label(data = df_metrics_perf,
             size = 2.5,
             mapping = aes(x = ECO_CLI, y = max(df_cv_abundance2$residuals,na.rm = T), label = paste0('RMSE = ',rmse,'\nn = ',n),
                           vjust = 1)) +
  scale_y_log10() +
  ggtitle("Abundance model: RMSE by year (LLO CV)") + 
  geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

#ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/abundance_validation.pdf",plot =plot_validation_abundance, device = "pdf", width = 11, height = 8) ## predictions

#### Third step: VIP  

model = multiv_model_abundance_LLO$model
df = multiv_model_abundance_LLO$df_mod
df_cv <-  multiv_model_abundance_LLO$df_cv

## To select the importance of each variable from the model and to transform in data frame
imp <- model$finalModel$variable.importance
imp <- as.data.frame(imp)
imp$var <- rownames(imp)

## To arrange by order of importance and to categorize by type of variable
imp <- imp %>%
  dplyr::rename(importance = imp) %>%
  mutate(label = forcats::fct_reorder(var, importance)) %>%
  arrange(-importance)



## To plot the importance of variables
plot_imp_abundance <- ggplot(imp, aes(x = importance , y = label, label = label, fill = var)) +
  geom_bar(position = 'dodge', stat="identity", width = 0.6) + 
  theme_bw() + 
  geom_text(size=3,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines")) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 7),
        plot.subtitle = element_text(size = 7, face="bold")
  ) +
  ylab("") + 
  xlab("") +
  xlim(NA,max(imp$importance, na.rm = T) + max(imp$importance, na.rm = T)*2.5) +
  labs(title = "Abundance model LLO CV: VIP")

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/abundance_VIP.pdf",plot =plot_imp_abundance, device = "pdf", width = 11, height = 8) ## To save

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

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/abundance_PDP.pdf",plot =plot_pdps_abundance, device = "pdf", width = 11, height = 8) ## To save
