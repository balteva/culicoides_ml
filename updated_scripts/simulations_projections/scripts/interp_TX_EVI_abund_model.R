library(tidyverse) 
library(iml) 
library(patchwork) 
library(precrec) 
library(lubridate)
library(pdp)


setwd("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml")


# grid <- read.csv("../../qgis/culicoides_point_locations_grid_150_150.csv")%>%
#   select(ID_SITE, Cell, ECO_CLI)

#cv with location LLO
multiv_model_abundance_LLO <- readRDS("./updated_scripts/simulations_projections/models/EVI/abundance/TX_EVI/1NBINDIV_abundance_LLO_TX_EVI.rds")


#CV with LTO
multiv_model_abundance_LTO <- readRDS("./updated_scripts/simulations_projections/models/EVI/abundance/TX_EVI/1NBINDIV_abundance_LTO_TX_EVI.rds")


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
  facet_wrap(~year) + #~ECO_CLI #ncol=2 if only by year
  theme_bw() + 
  scale_colour_manual(values=c("grey70","maroon2"),na.translate = F) + 
  scale_y_log10() +
  scale_x_continuous(breaks = seq(4,52,4)) +
  xlab("week") +
  ylab("Mean Abundance (log scale)") + 
  labs(color = expression("Number of " * italic("C. obsoletus") * " and " * italic("C. scoticus") * " (LTO CV)")) + 
  ggtitle('Presence models : observed vs. predicted values')+
  theme(legend.position="bottom") + 
  ggtitle('Abundance models : observed vs. predicted values (LTO CV)')+
  theme(axis.title.x = element_text(size = 12, face="bold"),
        axis.title.y = element_text(size = 12, face="bold"),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5, size= 14, face='bold'))


####################### LLO vs LTO in one (abundance)

df_temp <- df_cv_abundance_LTO %>% 
  select(pred,ID_SITE, DATE) %>%
  rename(pred_year=pred) #marking the predictions of LTO differently to distinguish in joint df

df_fin_cv<-merge(df_cv_abundance_LLO, df_temp, by=c("ID_SITE", "DATE"))
# LTO == "maroon2"
# LLO == "deepskyblue3"
# Observed = "grey70"

plot_eval_abundance_model <- df_fin_cv %>%
  mutate(week=week(as.Date(DATE)), year=year(as.Date(DATE))) %>%
  mutate(obs=exp(obs),pred=exp(pred), pred_year=exp(pred_year)) %>%
  dplyr::group_by(year, week, ECO_CLI) %>%    #ECO_CLI
  dplyr::summarise(pred = mean(pred), obs = mean(obs), pred_year=mean(pred_year)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs', 'pred_year')) %>%
  mutate(name = case_when(name=="pred"~"with LLO",name=='pred_year'~"with LTO", name=="obs"~"Observed")) %>%
  ggplot(aes(x=week, y = value, color = name)) +
  #geom_point() + 
  geom_line(linewidth = 0.8) + 
  facet_wrap(~year~ECO_CLI) + # ncol=2 #~ECO_CLI
  theme_bw()+
  scale_colour_manual(values=c("grey70","deepskyblue3","maroon2"),na.translate = F) + 
  scale_x_continuous(breaks = seq(4,52,4)) +
  scale_y_log10() +
  xlab("week") +
  ylab("Mean Abundance (log scale)") + 
  labs(color= expression("Predicted counts of " *italic("C. obsoletus")* " and " *italic(" C. scoticus")* " by year and ecoclimatic zone")) + 
  theme(legend.position="bottom") + 
  ggtitle('Abundance models: observed vs. predicted values with LLO or LTO CV') +
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
  group_by(ECO_CLI) %>% #ECO_CLI #or year
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

eval_plot_LTO <- df_cv_abundance_LLO %>% 
  mutate(year=year(as.Date(DATE)))%>%
  ggplot(aes(x=exp(obs), y=exp(pred)))+
  geom_point(color="grey70", size=1.2) + #aes(color=ECO_CLI))+
  theme_bw() +
  facet_grid(.~ECO_CLI, scales="fixed")+ #ECO_CLI #or year
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
  ggtitle("Abundance LLO model evaluation: predicted vs. observed value")+ #change this depending on LLO or LTO
  theme(axis.title.x = element_text(size = 12, face="bold"),
        axis.text.x = element_text(size = 12, face="bold"),
        axis.title.y = element_text(size = 12, face="bold"),
        axis.text.y = element_text(size = 12, face="bold"),
        plot.title = element_text(hjust = 0.5, size= 14, face='bold'))

##finished here
#### Third step: VIP  

model <- multiv_model_abundance_LTO$model

df_cv <-  multiv_model_abundance_LTO$df_cv
df <- multiv_model_abundance_LTO$df_mod #%>%
  # mutate(CLC_level2 = fct_relevel(CLC_level2,"Forests", "Scrub and/or herbaceous associations", "Pastures", "Permanent crops",
  #                                 "Heterogeneous agricultural areas", "Arable land", " Artificial, non-agricultural vegetated areas", "Urban fabric",
  #                                  "Industrial, commercial and transport units", "Mine, dump and construction sites")) #%>%
  # # dplyr::rename("Average max temperature" = TX_0_0,
  # #        "Land use" = CLC_level2,
  # #        "Soil moisture" = SWV_0_3,
  # #        "Wind speed"= FG_0_1,
  # #        "Vegetation index" = NDVI_0_5)





## To select the importance of each variable from the model and to transform in data frame
imp <- model$finalModel$variable.importance
imp <- as.data.frame(imp)
imp$var <- rownames(imp)
################
library(caret)
importance <- varImp(model, scale=T)
plot(importance) 




## To arrange by order of importance and to categorize by type of variable
imp1 <- imp %>%
  dplyr::rename(importance = imp) %>%
  mutate(label = forcats::fct_reorder(var, importance)) %>%
  arrange(-importance) %>% 
  mutate(Type = case_when(var %in% c("TX_0_0", "FG_0_5") ~ "Meteorological",
                          var %in% c("SWV_0_2", "EVI_0_5") ~ "Environmental",
                          var %in% ("ALT") ~ "Landscape",
                          var %in% c("OVI", "BETAIL") ~ "Livestock")) %>%
  mutate(var = case_when(var == "TX_0_0" ~ "Average Max Temperature 1 week before collection",
                         var == "SWV_0_2" ~ "Soil moisture 3 week average before collection",
                         var == "EVI_0_5" ~ "Vegetation Index 6 week average before collection",
                         var == "FG_0_5" ~ "Wind strength 6 week average before collection",
                         var == "BETAIL" ~ "Livestock density in canton",
                         var == "OVI" ~ "Sheep density in canton",
                         var == "ALT" ~ "Altitude"))


## To plot the importance of the variables
plot_imp_abundance_LTO <- ggplot(imp1, aes(x = importance , y = label, label = var, fill = Type)) +
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
  xlim(NA,max(imp1$importance, na.rm = T) + max(imp1$importance, na.rm = T)) +
  labs(title = "Variable importance plot (Abundance model)")
#### Last step: PDP  

## To create a function which predicts the abundance according different variables
categorical_vars <- ("CLC_level2")
exclusive <- ("TX_0_0")

###
imp <- model$finalModel$variable.importance ##need to run this again
imp <- as.data.frame(imp)
imp$var <- rownames(imp)
# imp<- imp%>%
#   mutate(var = recode(var,
#                       TX_0_0="Average max temperature",
#                       CLC_level2= "Land use",
#                       SWV_0_3= "Soil moisture",
#                       FG_0_1="Wind speed",
#                       NDVI_0_5="Vegetation index"))


fold_results <- model$resample
mean_accuracy <- mean(fold_results$MAE)
std_accuracy <- sd(fold_results$MAE)


pred_wrapper_reg <- function(object, newdata) {
  p <- predict(object, newdata = newdata)
  c("avg" = mean(p))
}

pdps <- list()
i<-1
for(i in 1:length(imp$var)){
  if (!(imp$var[i] %in% categorical_vars)){
    pd <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_reg, train = df)
    pd$yhat[which(pd$yhat < 0)] <- 0  # Correction des valeurs négatives
    
    p <- autoplot(pd, smooth = TRUE)  
    dat1 <- ggplot_build(p)$data[[1]]
    dat2 <- ggplot_build(p)$data[[2]]
    
    
    dens <- density(df[[imp$var[i]]])  
    dens_fun <- approxfun(dens$x, dens$y)  
    density_values <- dens_fun(dat1$x)  
    
    density_values <- density_values / max(density_values, na.rm = TRUE)  
    
    std_adjusted <- std_accuracy / sqrt(density_values + 1)  #
    
    dat1$conf_interval_lower <- dat1$y - 1.96 * std_adjusted
    dat1$conf_interval_upper <- dat1$y + 1.96 * std_adjusted
    if (imp$var[i] %in% exclusive) {
    pdps[[i]] <- ggplot() + 
      geom_point(data = dat1, aes(x = x, y = exp(y)), size = 1, fill = "black", alpha = 1) +   ## smooth the observed data
      geom_line(data = dat2, aes(x = x, y = exp(y)), size = 0.9, colour = "red") +  # smooth the prediction data
      geom_ribbon(data = dat1, aes(x = x, ymin = exp(conf_interval_lower), ymax = exp(conf_interval_upper)), 
                  alpha = 0.3, fill = "salmon1") +  # Affichage des intervalles de confiance sans contraintes
      geom_rug(data = df, aes_string(x = imp$var[i]), sides = "b", length = unit(0.05, "npc")) +
      #ylim(c(0, )) + 
      theme_bw() + 
      xlab(imp$var[i]) + 
      ylab("Number of C. obsoletus and C. scoticus")
    } 
    else {
      pdps[[i]] <- ggplot() + 
        geom_point(data = dat1, aes(x = x, y = exp(y)), size = 1, fill = "black", alpha = 1) +   ## smooth the observed data
        geom_line(data = dat2, aes(x = x, y = exp(y)), size = 0.9, colour = "red") +  # smooth the prediction data
        geom_ribbon(data = dat1, aes(x = x, ymin = exp(conf_interval_lower), ymax = exp(conf_interval_upper)), 
                    alpha = 0.3, fill = "salmon1") +  # Affichage des intervalles de confiance sans contraintes
        geom_rug(data = df, aes_string(x = imp$var[i]), sides = "b", length = unit(0.05, "npc")) +
        ylim(c(0,70)) + 
        theme_bw() + 
        xlab(imp$var[i]) + 
        ylab("Number of C. obsoletus and C. scoticus")
    }
  }
  else if (imp$var[i] %in% categorical_vars){
    
    counts <- df %>% 
      count(!!sym(imp$var[i]))
    
    pdp_cat <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_reg, train = df)
    pdps[[i]] <- ggplot(pdp_cat, aes(x=factor(!!sym(imp$var[i])), y = exp(yhat))) +  # factor(x) for categorical data on x axis
      geom_col(fill = "deepskyblue4", width=0.5) +  #barchart
      geom_text(data = counts, aes(x = factor(!!sym(imp$var[i])), y = 45, label = paste0("n = ", n)),  # Adjust y for text placement
                vjust = -0.5, size = 3, fontface="bold") +
      theme_bw() + 
      ylim(c(0,60)) +
      #scale_y_log10() +
      xlab(imp$var[i]) + 
      ylab("Number of C. obsoletus and C. scoticus")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))   
  
  }
}

plot_pdps_abundance <- patchwork::wrap_plots(pdps) + plot_annotation(title = "Abundance model: PDP") ## To put all variables together

#####

pred_ice <- function(object, newdata) {
  p <- predict(object, newdata = newdata)}


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
  pd$yhat[which(pd$yhat<0)] <-0
  pd <- pd %>%
    group_by(yhat.id)%>%
    mutate(yhat.centered = yhat-first(yhat)) #same concept but the predicted data after replacement with fixed feature values (with feature replacement)

  pd <- pd %>%
    left_join(df_ids) %>%
    mutate(yhat.exp = exp(yhat.centered))
  

  if (!(imp$var[i] %in% categorical_vars)){
    pdps_ice[[i]] <- ggplot(pd, aes_string(x=imp$var[i], y="yhat.exp", color= "ECO_CLI")) +
      geom_line(aes(group = yhat.id), alpha = 0.6, linewidth=0.4) +
      ylab("Change in caught individuals")+
      theme_classic() +
      scale_color_manual(values=eco_cli_colors) +
      labs(color = "Ecoclimatic Zone") +  
      guides(color = guide_legend(override.aes = list(linewidth = 1, alpha = 1)))  
    
  }}

plot_ice_abundance <- patchwork::wrap_plots(pdps_ice) + 
  plot_annotation(title = "Abundance model: Individual Conditional Expectation (ICE) plots") +
  plot_layout(guides = "collect")
#################################



data_only <- df %>% #making a df with only the predictors used in RF
  select(-c("ECO_CLI","ID_SITE", "DATE", "Cell", "year", "NBINDIV", "PRES_OBSCOT", "rowIndex"))

options(future.globals.maxSize = 2 * 1024 * 1024 * 1024)  # 2GB, computing interaction needs extra power

#creates an object  holding the machine learning model and the data
mod <- Predictor$new(model = model,data = data_only, y = log(df$NBINDIV))
ia <- Interaction$new(mod) #interaction evaluation
plot(ia) #visualising

TX_int<-Interaction$new(mod, feature = "TX_0_0", grid.size = 30)

plot(TX_int)

SWV_int<-Interaction$new(mod, feature = "SWV_0_2", grid.size = 30)


plot(SWV_int)



varpair <- c("SWV_0_2", "OVI")  

##function to predict presence probability
pred_wrapper_reg <- function(object, newdata) {
  p <- predict(object, newdata = newdata)
  c("avg" = mean(p))
}

## 2D DPD
pd <- pdp::partial(model, pred.var = varpair, pred.fun = pred_wrapper_reg, train = df) 

pd$yhat[which(pd$yhat < 0)] <- 0  # Correction des valeurs négatives


plot_pdp_2D <- ggplot(pd, aes(x = SWV_0_2, y = OVI, fill = exp(yhat))) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal() +
  labs(title = "Partial Dependence Plot for C. obsoletus/ C. scoticus\n abundance",
       x = "Water Volume in Soil",
       y = "Sheep density in canton",
       fill = "nbindiv") +
  theme(plot.title = element_text(hjust = 0.5, size= 14, face='bold')) 




