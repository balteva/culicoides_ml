library(tidyverse) 
library(iml) 
library(patchwork) 
library(precrec) 
library(lubridate)


#forgot to add eco_cli column, doing it now

grid <- read.csv("../../data/culicoides_point_locations_grid_150_150.csv")%>%
  select(ID_SITE, ECO_CLI)

#cv with location LLO
multiv_model_presence_LLO <- readRDS("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/res_multiv_model_presence_LLO.rds")


#CV with LTO
multiv_model_presence_LTO <- readRDS("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/res_multiv_model_presence_LTO.rds")


#results of LLO
model_presence_LLO <- multiv_model_presence_LLO[[1]] #### sum up of presence model
df_cv_presence_LLO <- multiv_model_presence_LLO[[2]] %>% #### data frame with prediction 
  left_join(grid)
df_mod_presence_LLO<- multiv_model_presence_LLO[[3]] #### data frame which was used to build the model

##results of LTO
df_cv_presence_LTO <- multiv_model_presence_LTO[[2]] %>% #### data frame with prediction 
  left_join(grid)
###
plot_eval_presence_model_LLO <- df_cv_presence_LLO %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE), week=lubridate::week(DATE))%>%
  #filter(year==2009)%>%
  dplyr::group_by(ECO_CLI,year,week) %>%   
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>% ## to sum up, grouping by trap, location and num session 
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
  ggplot(aes(x=week, y = value, color = name)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(year~ECO_CLI, scales = "free") + 
  theme_bw() + 
  scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F) + 
  #scale_x_continuous(breaks = seq(1,52,4)) +
  xlab("week") +
  ylab("∑ Presence probability") + 
  labs(color='Probability of presence of Culicoides Obsoletus/Scoticus by ecoclimatic zone (LLO CV)') + 
  theme(legend.position="bottom") + 
  ggtitle('Presence models : observed vs. predicted values')

ggsave(filename = "C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/presence_LLO_ECO_CLI.pdf",plot =plot_eval_presence_model_LLO, device = "pdf", width = 11, height = 8)
################""
### binary plot 
df_cv_presence_LLO %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE), week=lubridate::week(DATE))%>%
  dplyr::group_by(ECO_CLI,year,week) %>% 
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>% ## to sum up, grouping by trap, location and num session 
  mutate(Prediction = if_else(pred >= 0.5, "1", "0"), Observation = if_else(obs >= 0.5, "1", "0"))%>%
  as_tibble() %>%
  pivot_longer(cols=c('Prediction','Observation'), names_to="Type", values_to = "PresenceStatus") %>%
  ggplot(aes(x=week, y = Type, fill = PresenceStatus)) +
  geom_tile()+
  coord_fixed(ratio =4) +
  facet_grid(~ECO_CLI~year, scales="fixed") + 
  theme_minimal() + 
  scale_x_continuous(breaks = seq(2, 52, 4)) +
  scale_fill_manual(values = c("0" = "#4682B4", "1" = "#FF6347"), labels =c("Absence", "Presence"))+
  xlab("week") +
  labs(fill="Status") + 
  theme(legend.position="bottom") + 
  ggtitle("Predictions of Culicoides Obsoletus/Scoticus presence by ecoclimatic zone (LLO CV)")




########
plot_eval_presence_model_LTO <- df_cv_presence_LTO %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(week=lubridate::week(DATE))%>%
  #filter(year==2009)%>%
  dplyr::group_by(ECO_CLI,year,week) %>%   
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>% ## to sum up, grouping by trap, location and num session 
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
  ggplot(aes(x=week, y = value, color = name)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(year~ECO_CLI, scales = "free") + 
  theme_bw() + 
  scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F) + 
  #scale_x_continuous(breaks = seq(1,52,4)) +
  xlab("week") +
  ylab("∑ Presence probability") + 
  labs(color='Probability of presence of Culicoides Obsoletus/Scoticus by ecoclimatic zone (LTO CV)') + 
  theme(legend.position="bottom") + 
  ggtitle('Presence models : observed vs. predicted values')

ggsave(filename = "C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/presence_LTO_ECO_CLI.pdf",plot =plot_eval_presence_model_LTO, device = "pdf", width = 11, height = 8)






#####
df_LTO_presence <- df_cv_presence_LTO|> ## grouping in a same dataframe both predictions with LLO and LTO cross validation 
  select(idpointdecapture, pred)|>
  rename(pred_year=pred) #renaming LTO pred to indicate the cv was by year

df_fin_cv<-merge(df_cv_presence_LLO, df_LTO_presence, by="idpointdecapture")%>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=year(DATE), week=week(DATE))

plot_eval_presence_model <- df_fin_cv %>%
  dplyr::group_by(ECO_CLI,year,week) %>%   
  dplyr::summarise(pred = mean(pred), obs = mean(obs), pred_year=mean(pred_year)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs', 'pred_year')) %>%
  mutate(name = case_when(name=="pred"~"Predicted with LLO",name=='pred_year'~"Predicted with LTO", name=="obs"~"Observed")) %>% ## to sum up, grouping by ECO CLI, week and year 
  ggplot(aes(x=week, y = value, color = name)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(ECO_CLI~year, scales = "free") + 
  theme_bw() + 
  scale_colour_manual(values=c("#009E73","blue","#E69F00"),na.translate = F) + 
  #scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  xlab("week") +
  ylab("∑ Presence probability") + 
  labs(color='Probability of presence of Culicoides Obsoletus/Scoticus complex by ecoclimatic zone') + 
  theme(legend.position="bottom") + 
  ggtitle('Presence models : observed vs. predicted values')


#### Second step: Model validation plots: ROC 
library(MLmetrics)
AUC = MLmetrics::AUC(df_cv_presence_LTO$pred, df_cv_presence_LTO$obs) ## To calculate the AUC

precrec_obj <- precrec::evalmod(scores = df_cv_presence_LTO$pred, labels = df_cv_presence_LTO$obs)

plot_validation_presence_LTO <- autoplot(precrec_obj,curvetype = c("ROC")) + 
  ggtitle(paste0("Presence model with LTO CV: ROC curve (AUC = ",round(AUC,2),")")) +     
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

#ggsave(filename = "C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/presence_validation_LTO.pdf",plot =plot_validation_presence_LTO, device = "pdf", width = 11, height = 8) ## to save

#### Third step: VIP 

model = multiv_model_presence_LLO$model
df = multiv_model_presence_LLO$df_mod
df_cv <-  multiv_model_presence_LLO$df_cv

## To select the importance of each variable from the model and to transform in data frame
imp <- model$finalModel$variable.importance 
imp <- as.data.frame(imp)
imp$var <- rownames(imp)

## To arrange by order of importance and to categorize by type of variable
imp <- imp %>%
  dplyr::rename(importance = imp) %>%
  mutate(label = forcats::fct_reorder(var, importance)) %>%
  arrange(-importance) #%>% 
  # mutate(type = case_when(var %in% c("GDDjour_1_1","WINDmf_0_5","RFDode_5_6") ~ "Meteorological",
  #                         var %in% c("RFSUM_collection","RHMAX_24h_prec","RHMIN_24h_prec") ~ "Micro-climatic",
  #                         var=="lsm_c_pland_LCG_20_13" ~ "Landscape - vegetation",
  #                         var %in% c("lsm_l_shdi_LCG_100_NA","lsm_c_pland_LCG_20_11","lsm_c_pland_LCG_50_10","BATH_max_250") ~ "Landscape - others",
  #                         var=="NO_3_3" ~ "polluants",
  #                         var %in% c("POP_250_sum") ~ "Socio-demographics"))


## To plot the importance of the variables
plot_imp_presence <- ggplot(imp, aes(x = importance , y = label, label = label, fill = var)) +
  geom_bar(position = 'dodge', stat="identity", width = 0.6) + 
  theme_bw() + 
  geom_text(size=3,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines")) + #,aes(fontface=2)
  #   geom_label(size=2, aes(fontface=2), label.padding = unit(0.15, "lines"), x = 0.05, alpha = 0.5) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 7),
        plot.subtitle = element_text(size = 7, face="bold")
  ) +
  ylab("") + 
  xlab("") +
  xlim(NA,max(imp$importance, na.rm = T) + max(imp$importance, na.rm = T)*2.5) +
  labs(title = "LLO CV Presence model : VIP")

ggsave(filename = "C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/presence_VIP_LLO.pdf",plot =plot_imp_presence, device = "pdf", width = 11, height = 8) ## to save

#### Last step: PDP 
library(pdp)
## To create a function which predicts the probability of presence according different variabes
pred_wrapper_classif <- function(object, newdata) { 
  p <- predict(object, newdata = newdata, type ="prob")[,"Presence"]
  c("avg" = mean(p))
}

pdps <- list()

for(i in 1:length(imp$var)){
  
  pd <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_classif, train = df) ## array that returns predictions of a variable in a model
  pd$yhat[which(pd$yhat<0)] <-0 
  p <- autoplot(pd, smooth = T)  
  dat1 <- ggplot_build(p)$data[[1]]
  dat2 <- ggplot_build(p)$data[[2]]
  
  if(imp$var[i]!="RFSUM_collection"){ ## for every variable which is not The resence of rainfall
    pdps[[i]] <- ggplot() + 
      geom_line(data = dat1, aes(x = x, y = exp(y)), size = 0.3, colour = "black", alpha = 0.4) +  ## smooth the observed data
      geom_line(data = dat2, aes(x = x, y = y), size = 0.5, colour = "#009E73") +  ## smooth the prediction data
      geom_rug(data = df, aes_string(x = imp$var[i]), sides="b", length = unit(0.05, "npc")) + 
      ylim(c(0,1)) + 
      theme_bw() + 
      xlab(imp$var[i]) + 
      ylab("")
  } else {
    dat1$x <- c("Absence","Presence")
    df[,imp$var[i]][which(df[,imp$var[i]]==1)] <- "Presence"
    df[,imp$var[i]][which(df[,imp$var[i]]==0)] <- "Absence"
    
    pdps[[i]] <- ggplot() + 
      geom_bar(data = dat1, aes(x = x, y = y), size = 0.5, fill = "#009E73", stat = "identity") +  ## indicate the different value of the variable present on the data frame
      geom_rug(data = df, aes_string(x = imp$var[i]), sides="b", length = unit(0.05, "npc")) + 
      ylim(c(0,1)) + 
      theme_bw() + 
      xlab(imp$var[i]) + 
      ylab("")
    
    df[,imp$var[i]][which(df[,imp$var[i]]=="Presence")] <- "1"
    df[,imp$var[i]][which(df[,imp$var[i]]=="Absence")] <- "0"
    df[,imp$var[i]] <- as.numeric(df[,imp$var[i]])
  }
  
}

plot_pdps_presence <- patchwork::wrap_plots(pdps) + plot_annotation(title = "LLO Presence model : PDP") ## put all the plots together

ggsave(filename = "C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/LLO_presence_PDP.pdf",plot =plot_pdps_presence, device = "pdf", width = 11, height = 8) ## To save




