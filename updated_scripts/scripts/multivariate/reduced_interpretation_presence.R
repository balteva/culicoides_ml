library(tidyverse) 
library(iml) 
library(patchwork) 
library(precrec) 
library(lubridate)



# grid <- read.csv("../../qgis/culicoides_point_locations_grid_150_150.csv")%>%
#   select(ID_SITE, ECO_CLI, Cell) #dont need it now because added in model prep

#cv with location LLO
multiv_model_presence_LLO <- readRDS("./updated_scripts/models/reduced_RF_model_presence_LLO_interpretation.rds")


#CV with LTO
multiv_model_presence_LTO <- readRDS("./updated_scripts/models/reduced_RF_model_presence_LTO_interpretation.rds")


#results of LLO
model_presence_LLO <- multiv_model_presence_LLO[[1]] #### sum up of presence model 
df_cv_presence_LLO <- multiv_model_presence_LLO[[2]]  #### data frame with prediction # %>% #  left_join(grid)
df_mod_presence_LLO<- multiv_model_presence_LLO[[3]] #### data frame which was used to build the model

##results of LTO
df_cv_presence_LTO <- multiv_model_presence_LTO[[2]]  #### data frame with prediction #  left_join(grid)



eco_cli_colors <- c("Alpine" = "#faa09b", "Atlantic" = "#c8e18d", "Continental" = "#c5f1f2", "Mediterranean" = "#d7bdeb") 


# LTO == "maroon2"
# LLO == "deepskyblue3"
plot_eval_presence_model_LTO <- df_cv_presence_LLO %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE), week=lubridate::week(DATE))%>%
  dplyr::group_by(year,week) %>% #ECO_CLI,   
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>% ## to sum up, grouping by week, year and ecoclimatic zone
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
  ggplot(aes(x=week, y = value, color = name)) +
  #geom_point() + 
  geom_line(linewidth=0.8) + 
  facet_wrap(~year, scales = "free") + #~ECO_CLI
  theme_bw() + 
  scale_colour_manual(values=c("grey70","deepskyblue3"),na.translate = F) + 
  scale_x_continuous(breaks = seq(4,52,4)) +
  xlab("week") +
  ylab("∑ Presence probability") + 
  labs(color = expression("Probability of presence of " * italic("C. obsoletus") * " and " * italic("C. scoticus") * " by year (LLO CV)")) + 
  ggtitle('Presence models : observed vs. predicted values')+
  theme(axis.title.x = element_text(size = 12, face="bold"),
      axis.title.y = element_text(size = 12, face="bold"),
      legend.position="bottom",
      plot.title = element_text(hjust = 0.5, size= 14, face='bold'))


################
### binary plot 
binary_plot_LLO <- df_cv_presence_LLO %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE), week=lubridate::week(DATE))%>%
  dplyr::group_by(year,week) %>%#ECO_CLI,
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>% ## to sum up, grouping week, year and ecoclimatic zone 
  mutate(Prediction = if_else(pred >= 0.5, "1", "0"), Observation = if_else(obs >= 0.5, "1", "0"))%>%
  # mutate(ECO_CLI = case_when(ECO_CLI == "Atlantic" ~ "Atlan.",
  #                            ECO_CLI == "Mediterranean" ~ "Med.",
  #                            ECO_CLI == "Continental" ~ "Cont.",
  #                            ECO_CLI == "Alpine" ~ "Alp."))%>%
  as_tibble() %>%
  pivot_longer(cols=c('Prediction','Observation'), names_to="Type", values_to = "PresenceStatus") %>%
  ggplot(aes(x=week, y = Type, fill = PresenceStatus)) +
  geom_tile()+
  coord_fixed(ratio =4) +
  facet_wrap(~year, scales="fixed", ncol=1) +#~ECO_CLI
  theme_minimal() + 
  scale_x_continuous(breaks = seq(2, 52, 4)) +
  scale_fill_manual(values = c("0" = "#4682B4", "1" = "#FF6347"), labels =c("Absence", "Presence"))+
  xlab("week") +
  labs(fill="Status") + 
  theme(legend.position="bottom", strip.text=element_text(face="bold", size=10),
        plot.title = element_text(hjust = 0.5, size= 15, face='bold'),
        axis.title.x = element_text(size=12, face="bold"),  axis.title.y = element_text(size=12, face="bold")) + 
  ggtitle(expression("Predictions of " * italic("C. obsoletus") * " and " * italic("C. scoticus") * " presence by year (LLO CV)"))


#####
df_LTO_presence <- df_cv_presence_LTO %>% ## grouping in a same dataframe both predictions with LLO and LTO cross validation 
  select(idpointdecapture, pred) %>%
  rename(pred_year=pred) #renaming LTO pred to indicate the cv was by year

df_fin_cv<-merge(df_cv_presence_LLO, df_LTO_presence, by="idpointdecapture")%>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=year(DATE), week=week(DATE))

# LTO == "maroon2"
# LLO == "deepskyblue3"
plot_eval_presence_model <- df_fin_cv %>%
  dplyr::group_by(ECO_CLI,year, week) %>%   #ECO_CLI,
  dplyr::summarise(pred = mean(pred), obs = mean(obs), pred_year=mean(pred_year)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs', 'pred_year')) %>%
  mutate(name = case_when(name=="pred"~"Predicted with LLO",name=='pred_year'~"Predicted with LTO", name=="obs"~"Observed")) %>% ## to sum up, grouping by ECO CLI, week and year 
  ggplot(aes(x=week, y = value, color = name)) +
  #geom_point() + 
  geom_line(linewidth=1) + 
  facet_wrap(~ECO_CLI~year, scales = "free") + #~ECO_CLI
  theme_bw() + 
  scale_colour_manual(values=c("grey70","deepskyblue3","maroon2"),na.translate = F) + 
  scale_x_continuous(breaks = seq(2,52,4)) +
  xlab("week") +
  ylab("∑ Presence probability") + 
  labs(color='Data Type') + 
  theme(axis.title.x = element_text(size = 12, face="bold"),
        axis.title.y = element_text(size = 12, face="bold"),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5, size= 14, face='bold'))+
  ggtitle(expression("Probability of presence of " *italic("C. obsoletus")* " and " *italic( "C. scoticus ")* "by ecoclimatic zone and year"))



#### Second step: Model validation plots: ROC 

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
roc_plots_all <- patchwork::wrap_plots(roc_plots, ncol = 3) #+ plot_annotation(title = "Presence model evaluation per ecoclimatic zone")

###########################################################Third step: VIP 
library(caret)
model <- multiv_model_presence_LTO$model #model details (mtrees, specificity, sensitivity)
df <- multiv_model_presence_LTO$df_mod #og dataframe
df_cv <-  multiv_model_presence_LTO$df_cv #og data frame with predictions

##easier to understand importace measure
importance <- varImp(model, scale=F)
plot(importance) 

##################



## To select the importance of each variable from the model and to transform in data frame
imp <- model$finalModel$variable.importance #ii think here it stored it as a vector
imp <- as.data.frame(imp) #converted to df
imp$var <- rownames(imp)



## To arrange by order of importance and to categorize by type of variable
imp <- imp %>%
  dplyr::rename(importance = imp) %>%
  mutate(label = forcats::fct_reorder(var, importance)) %>%
  arrange(-importance) %>% 
  mutate(Type = case_when(var %in% c("TN_0_0", "FG_4_6", "PLUIEDEBUT") ~ "Meteorological",
                          var %in% c("ALT", "OUVERTURE_BAT") ~ "Landscape",
                          var %in% c("SWV_2_5", "NDVI_0_1") ~ "Environmental",
                          var %in% c("OVI", "BETAIL") ~ "Livestock"))%>%
  mutate(var = case_when(var == "TN_0_0" ~ "Minimum temperature one week before collection",
                         var == "SWV_2_5" ~ "Soil moisture 3-6 weeks before collection",
                         var == "FG_4_6" ~ "Wind strength 5-7 weeks before collection",
                         var == "NDVI_0_1" ~ "Vegetation Index 1-2 weeks before collection",
                         var == "ALT" ~ "Altitude",
                         var == "OVI" ~ "Nb. of sheeps/hectare in canton",
                         var == "BETAIL" ~ "Nb. of livestock animals/hectare in canton",
                         var == "OUVERTURE_BAT" ~ "Degree of building openness",
                         var == "PLUIEDEBUT" ~ "Rain status during collection"))


## To plot the importance of the variables
plot_imp_presence_LTO <- ggplot(imp, aes(x = importance , y = label, label = var, fill = Type)) +
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
  labs(title = "Variable importance plot (LTO Presence model)")


#### Last step: PDP 

categorical_vars <- c("OUVERTURE_BAT", "PLUIEDEBUT")


library(pdp)



## To create a function which predicts the probability of presence according different variable values
pred_wrapper_classif <- function(object, newdata) { 
  p <- predict(object, newdata = newdata, type ="prob")[,"Presence"] #object is fitted model, newdata is new predictor values (generated value range for feature)
  return(c("avg" = mean(p)))
}

pdps <- list()

for(i in 1:length(imp$var)){ #looping over each variable
  if (!(imp$var[i] %in% categorical_vars)){
    pd <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_classif, train = df) ## array that returns predictions of a variable in a model #train extracts the original training data
    pd$yhat[which(pd$yhat<0)] <-0 #any neg values in probability to 0 because they range [0;1]
    p <- autoplot(pd, smooth = T)  
    dat1 <- ggplot_build(p)$data[[1]]#ggplot_build reverse engineers data from a plot and give all info including data. here im accessing the first layer  which is original feature values and original observatins
    dat2 <- ggplot_build(p)$data[[2]] #same concept but the predicted data after replacement with fixed feature values (with feature replacement)
    pdps[[i]] <- ggplot() + 
      geom_point(data = dat1, aes(x = x, y = y), size = 1, colour = "black", alpha = 1) +  ## plotting the observed data
      geom_line(data = dat2, aes(x = x, y = y), size = 0.9, colour = "red") +  ## plotting the predicted data
      geom_rug(data = df, aes_string(x = imp$var[i]), sides="b", length = unit(0.05, "npc")) + 
      ylim(c(0,1)) + 
      theme_bw() + 
      xlab(imp$var[i]) + 
      ylab("Probability of Presence") }
  else if (imp$var[i] %in% categorical_vars){#this is for plotting categorical vars, simple pdp without any modification like done for numerical
    pdp_cat <- pdp::partial(model, pred.var = imp$var[i] , plot = FALSE, which.class = "Presence")
    pdps[[i]] <- ggplot(pdp_cat, aes(x=factor(!!sym(imp$var[i])), y = yhat)) +  # factor(x) for categorical data on x axis
      geom_col(fill = "deepskyblue4", width=0.5) +  #barchart
      ylim(c(0, 1)) +  
      theme_bw() + 
      xlab(imp$var[i]) + 
      ylab("Probability of Presence")
  }
}
plot_pdps_presence <- patchwork::wrap_plots(pdps) + plot_annotation(title = "Partial dependence plots (Presence model)") ## put all the plots together




###############Individual Conditional Expectation plots (centered)

pred_ice<- function(object, newdata) { 
      p <- predict(object, newdata = newdata, type ="prob")[,"Presence"] #object is fitted model, newdata is new predictor values (generated value range for feature)
    }
    
grid <- read.csv("../../qgis/culicoides_point_locations_grid_150_150.csv")%>%
  select(ID_SITE, ECO_CLI, Cell)
eco_cli_colors <- c("Alpine" = "#D81B60",  "Atlantic" = "#1E88E5","Continental" = "#004D40",  "Mediterranean" = "#FFC107") 

df_ids <- df %>%
  rename(yhat.id = rowIndex)%>%
  left_join(grid)%>%
  select(c("yhat.id", "ECO_CLI", "year"))

pdps_ice <- list()

#functioning the function
for(i in 1:length(imp$var)){
  
  pd <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_ice, train = df) ## array that returns predictions of a variable in a model #train extracts the original training data
  pd$yhat[which(pd$yhat<0)] <-0 #any neg values in pro st to 0 because they range [0;1]
  pd <- pd %>%
    group_by(yhat.id)%>%
    mutate(yhat.centered = yhat-first(yhat)) #same concept but the predicted data after replacement with fixed feature values (with feature replacement)
  
  pd <- pd %>%
    left_join(df_ids)
  
  if (!(imp$var[i] %in% categorical_vars)){
    pdps_ice[[i]] <- ggplot(pd, aes_string(x=imp$var[i], y="yhat.centered", color= "ECO_CLI")) +
      geom_line(aes(group = yhat.id), alpha = 0.6, linewidth=0.4) +
      stat_summary(fun.y = mean, geom = "line", col = "red", size = 0.9) +
      ylab("Change in Presence Prediction")+
      theme_classic() +
      scale_color_manual(values=eco_cli_colors) +
      labs(color = "Ecoclimatic Zone") +  
      guides(color = guide_legend(override.aes = list(linewidth = 1, alpha = 1)))  
    
  }}

plot_ice_presence <- patchwork::wrap_plots(pdps_ice) + 
  plot_annotation(title = "Presence model : Individual Conditional Expectation (ICE) plots") +
  plot_layout(guides = "collect")
######### MEASURING FEATURE INTERACTIONS
library(yaImpute)
library(caret)
library(data.table)
## just a reminder of where i'm getting the data:
# model <- multiv_model_presence_LTO$model #model details (mtrees, specificity, sensitivity)
# df <- multiv_model_presence_LTO$df_mod #og dataframe
# df_cv <-  multiv_model_presence_LTO$df_cv #og data frame with predictions


data_only <- df %>% #making a df with only the predictors used in RF
  select(-c("idpointdecapture", "ECO_CLI","ID_SITE", "DATE", "Cell", "year", "NBINDIV", "PRES_CUL", "rowIndex"))

options(future.globals.maxSize = 2 * 1024 * 1024 * 1024)  # 2GB, computing interaction needs extra power

mod <- Predictor$new(model, data = data_only, type = "prob", class = "Presence")#creates an object  holding the machine learning model and the data

ia <- Interaction$new(mod) #interaction evaluation
plot(ia) #visualising

#calculating var1 x other vars interaction strength based on previous output

# SWV_int<-Interaction$new(mod, feature = "SWV_0_4", grid.size = 30)
# 
# plot(SWV_int)
# 
# EVI_int<-Interaction$new(mod, feature = "EVI_0_3", grid.size = 30)
# 
# plot(EVI_int)


TN_int<-Interaction$new(mod, feature = "TN_0_0", grid.size = 30)

plot(TN_int)


## which var pair im analysing
varpair <- c("TN_0_0", "ALT")  

##function to predict presence probability
pred_wrapper_classif <- function(object, newdata) { 
  p <- predict(object, newdata = newdata, type = "prob")[, "Presence"]  
  return(c("avg" = mean(p)))  
}

## 2D DPD
pd <- pdp::partial(model, pred.var = varpair, pred.fun = pred_wrapper_classif, train = df) 

pd$yhat[pd$yhat < 0] <- 0

plot_pdp_2D <- ggplot(pd, aes(x = TN_0_0, y = ALT, fill = yhat)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal() +
  labs(title = expression("Partial dependence plot of " * italic ("C. obsoletus")* " and " *italic("C. scoticus")* " presence probability"),
    x = "Minimum temperature 1 week before collection",
    y = "Altitude",
    fill = "Probability") +
  theme(plot.title = element_text(hjust = 0.5, size= 14, face='bold')) 

  