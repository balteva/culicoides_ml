


library(yaImpute)
library(iml)
library(caret)
library(tidyverse)
library(data.table)

model <- multiv_model_presence_LLO$model #model details (mtrees, specificity, sensitivity)
df <- multiv_model_presence_LLO$df_mod #og dataframe
df_cv <-  multiv_model_presence_LLO$df_cv #og data frame with predictions


## To select the importance of each variable from the model and to transform in data frame
imp <- model$finalModel$variable.importance #ii think here it stored it as a vector
imp <- as.data.frame(imp) #converted to df
imp$var <- rownames(imp)




data_only <- df %>%
  select(-c("idpointdecapture", "ID_SITE", "DATE", "Cell", "year", "NBINDIV", "PRES_CUL", "rowIndex"))


options(future.globals.maxSize = 2 * 1024 * 1024 * 1024)  # 2GB
#ia <- Interaction$new(mod)


mod <- Predictor$new(model, data = data_only, type = "prob", class = "Presence")
ia <- Interaction$new(mod)
## End(Not run)
plot(ia)

Interaction$new(mod, feature = "ALT", grid.size = 30)

ALT_int<-Interaction$new(mod, feature = "ALT", grid.size = 30)

plot(ALT_int)












##easier to understand importace measure
importance <- varImp(model, scale=F)
plot(importance)

################## 
df_binary <- df %>%
  mutate(PRES_CUL = ifelse(PRES_CUL == "Presence", 1, 0))

predictor <- Predictor$new(model, data = df_binary, y = "PRES_CUL") 



ale_effect <- FeatureEffect$new(predictor, feature = "UM_0_0", method = "ale", grid.size = 20)


plot(ale_effect, rug= TRUE)


Interaction$new(predictor, grid.size = 30)



#################ice


#trying to plot ICE for ECO  climatic zones
#idea is to try per species

model <- multiv_model_presence_LLO$model #model details (mtrees, specificity, sensitivity)
df <- multiv_model_presence_LLO$df_mod #og dataframe
df_cv <-  multiv_model_presence_LLO$df_cv #og data frame with predictions


## To select the importance of each variable from the model and to transform in data frame
imp <- model$finalModel$variable.importance #ii think here it stored it as a vector
imp <- as.data.frame(imp) #converted to df
imp$var <- rownames(imp)


pred_ice<- function(object, newdata) { 
  p <- predict(object, newdata = newdata, type ="prob")[,"Presence"] #object is fitted model, newdata is new predictor values (generated value range for feature)
}

pdps_ice <- list()

for(i in 1:length(imp$var)){#looping over each variable
  
  pd <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_ice, train = df) ## array that returns predictions of a variable in a model #train extracts the original training data
  pd$yhat[which(pd$yhat<0)] <-0 #any neg values in pro st to 0 because they range [0;1]
  pd <- pd %>%
    group_by(yhat.id)%>%
    mutate(yhat.centered = yhat-first(yhat)) #same concept but the predicted data after replacement with fixed feature values (with feature replacement)
  
  if (!(imp$var[i] %in% categorical_vars)){ ## doing this only for continuous variables
    pdps_ice[[i]] <- ggplot(pd, aes_string(x=imp$var[i], y="yhat")) +#change to yhat.centered to center
      geom_line(aes(group = yhat.id), alpha = 0.2) +
      stat_summary(fun.y = mean, geom = "line", col = "red", size = 1)+
      ylab("Change in Presence Prediction")+
      theme_classic()
  }}
#### unfunctioning the function
eco_cli_colors <- c("Alpine" = "#D81B60",  "Atlantic" = "#1E88E5","Continental" = "#004D40",  "Mediterranean" = "#FFC107") # Vivid amber

grid <- read.csv("../../qgis/culicoides_point_locations_grid_150_150.csv")%>%
  select(ID_SITE, ECO_CLI, Cell)

df_ids <- df %>%
  rename(yhat.id = rowIndex)%>%
  left_join(grid)%>%
  select(c("yhat.id", "ECO_CLI", "year"))

pdps_ice <- list()

  for(i in 1:length(imp$var)){#looping over each variable
  
  pd <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_ice, train = df) ## array that returns predictions of a variable in a model #train extracts the original training data
  pd$yhat[which(pd$yhat<0)] <-0 #any neg values in pro st to 0 because they range [0;1]
pd <- pd %>%
  group_by(yhat.id)%>%
  mutate(yhat.centered = yhat-first(yhat)) #same concept but the predicted data after replacement with fixed feature values (with feature replacement)


pd <- pd %>%
  left_join(df_ids)


 ## doing this only for continuous variables
  pdps_ice_UM <- ggplot(pd, aes_string(x="ALT", y="yhat.centered", color= "ECO_CLI")) +
    geom_line(aes(group = yhat.id), alpha = 0.7, linewidth=0.6) +
    stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
    ylab("Change in Presence Prediction")+
    theme_classic() +
    scale_color_manual(values=eco_cli_colors) +
    labs(color = "Ecoclimatic Zone") +  
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1)))  



###

  grid <- read.csv("../../qgis/culicoides_point_locations_grid_150_150.csv")%>%
    select(ID_SITE, ECO_CLI, Cell)
  eco_cli_colors <- c("Alpine" = "#D81B60",  "Atlantic" = "#1E88E5","Continental" = "#004D40",  "Mediterranean" = "#FFC107") # Vivid amber
  
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
    geom_line(aes(group = yhat.id), alpha = 0.7, linewidth=0.6) +
    stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
    ylab("Change in Presence Prediction")+
    theme_classic() +
    scale_color_manual(values=eco_cli_colors) +
    labs(color = "Ecoclimatic Zone") +  
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1)))  
  
}}
  library(patchwork)
  plot_ice_presence <- patchwork::wrap_plots(pdps_ice) + 
    plot_annotation(title = "Presence models : Individual Conditional Expectation (ICE) plots") +
    plot_layout(guides = "collect")
  