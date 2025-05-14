library(tidyverse) 
library(caret) 
library(CAST) 
library(ranger) 
library(correlation) 
library(fuzzySim) 
library(fst)
library(precrec)
dictionary <- read_fst("../../data/covariates_dict.fst")

# loading landscape data and cleaning the df for NA/empty values

micro_landscape_df<- read.csv('./data//glmm/dataframes/signif_landscape_vars_femelles_pares_nbindiv.csv')%>%
  select(!(c(num_releve, idpointdecapture, longitude, latitude, SURF_CANT, ANIMAUXFIN, ANIMAUXDEBUT, NUAGEDEBUT, NUAGEFIN, TEMPERATUREDEBUT, TEMPERATUREFIN, TEMPMINI, TEMPMAX, ACTIV_LAIT,EQU)))%>%
  mutate(NBINDIV = as.numeric(NBINDIV), PRES_OBSCOT= as.numeric(PRES_OBSCOT), PRES_FEMELLES= as.numeric(PRES_FEMELLES), PRES_PARS= as.numeric(PRES_PARES))%>%
  mutate(PLUIEDEBUT=factor(fct_relevel(PLUIEDEBUT, "Aucune","Brouillard / Bruine", "Pluie fine", "Pluie forte"), ordered = T),
         PLUIEFIN=factor(fct_relevel(PLUIEFIN, "Aucune","Brouillard / Bruine", "Pluie fine", "Pluie forte"), ordered = T),
         VENTDEBUT= factor(fct_relevel(VENTDEBUT, "Vent nul", "Légère brise à vent faible", "Vent moyen","Vent fort à grand vent"), ordered = T),
         VENTFIN= factor(fct_relevel(VENTFIN, "Vent nul", "Légère brise à vent faible", "Vent moyen","Vent fort à grand vent"), ordered = T),
         OUVERTURE_BAT= factor(fct_relevel(OUVERTURE_BAT, "0 - 5 %","5 - 25 %", "25 - 75 %","75 - 100 %"), ordered = T))




ccm_meteo <- read.csv("./data/pre_post_ccm/ccm_vars_obscot_femelles_pares_indiv.csv")%>%
  select(c(ID_SITE, DATE,
#presence 1NBINDIV vars
"TM_0_0",
"TN_0_0",
"TX_0_4",
"UM_0_0",
"TPS_0_0",
"QQ_1_6",
"EVI_0_3",
"PP_5_5",
"SWV_0_4",
"FG_4_6",
"NDVI_0_1",
#presence 5FEMELLES vars
"TM_0_0",
"TN_0_0",
"TX_0_4",
"UM_0_0",
"TPS_0_0",
"QQ_1_6",
"PP_1_6",
"SWV_0_2",
"FG_0_5",
"EVI_0_5",
"NDVI_0_1",
#presence 5 PARES
"TM_0_0",
"TN_0_0",
"TX_0_0",
"UM_0_0",
"TPS_0_0",
"QQ_1_6",
"PP_1_6",
"SWV_0_2",
"FG_0_5",
"EVI_0_0",
"NDVI_0_0",
#abundance 1NBINDIV
"TM_0_0",
"TN_0_0",
"TX_0_0",
"UM_0_0",
"TPS_0_0",
"QQ_0_6",
"PP_1_5",
"EVI_0_6",
"SWV_0_2",
"FG_0_5",
"EVI_0_5",
"NDVI_0_1"))



# filtered_meteo <- ccm_meteo %>% filter(is.na(TX_0_4)) #check foor errors

df_combined <-micro_landscape_df %>%
  left_join(ccm_meteo)%>%
  relocate(NBINDIV,NBFEMELLES, NBPARE, .after = DATE)%>%
  mutate(CLC_level2=as.factor(CLC_level2))


#filtered_combined <- df_combined %>% filter(is.na(TX_0_4)) #check for errors

# duplicates <- df_combined %>% #check for errors
#   group_by(ID_SITE, DATE) %>%
#   filter(n() > 1)%>%
#   as.data.frame()


#creating clumns for presence/absence for the presence model
df_model <- df_combined %>% 
  mutate(PRES_OBSCOT = ifelse(NBINDIV >= 1,"Presence","Absence"),
         PRES_PARES = ifelse(NBPARE > 5,"Presence","Absence"),
         PRES_FEMELLES = ifelse(NBFEMELLES > 5,"Presence","Absence")) %>% ## to create a "character" variable for presence or absence 
  mutate(PRES_OBSCOT = fct_relevel(PRES_OBSCOT,c("Presence","Absence")),
         PRES_PARES = fct_relevel(PRES_PARES,c("Presence","Absence")),
         PRES_FEMELLES = fct_relevel(PRES_FEMELLES,c("Presence","Absence"))) %>%
  mutate(PRES_OBSCOT_NUMERIC = ifelse(NBINDIV >= 1, 1, 0),
         PRES_PARES_NUMERIC = ifelse(NBPARE > 5, 1, 0),
         PRES_FEMELLES_NUMERIC = ifelse(NBFEMELLES > 5, 1, 0))%>%
  relocate(PRES_OBSCOT_NUMERIC:PRES_FEMELLES_NUMERIC, .after = NBPARE) %>%
  mutate(across(ELEV_OVIN:PLUIEFIN, as.factor), across(ELEV_OVIN:OUVERTURE_BAT, as.factor))%>%
  mutate(across(ALT:BOV, as.numeric), across(TM_0_0:EVI_0_6, as.numeric))%>%
  mutate(CLC_level2=as.factor(CLC_level2))

################################## this was the preparatory step
# grid <- read.csv("../../qgis/culicoides_point_locations_grid_150_150.csv") %>%
#   select(ID_SITE, Cell)
# 
# df_model<- df_model %>%
#   left_join(grid)
# write.csv(df_model, "./data/predictive_femelles_pares_obscot_RF.csv", row.names = F)

################################# LOAD DATA HERE : 
df_model <- read.csv("./data/predictive_femelles_pares_obscot_RF.csv")



#######################################################################################################
#                                       PREDICTIVE MODEL VARS ONLY
#######################################################################################################
vars_presence_nbindiv <- c("TM_0_0",
                           "TN_0_0",
                           "TX_0_4",
                           "UM_0_0",###big issue with humidity!!!! cannot calculate it because this isdewpoint temperature and not relative hum!
                           "TPS_0_0",
                           #"QQ_1_6",
                           "EVI_0_3",
                           #"NDVI_0_1",
                           #"PP_5_5", #something funky wtih PP, lots of NA values
                           "SWV_0_4",
                           "FG_4_6",
                           "ALT", 
                           "OVI",
                           "BETAIL")



vars_presence_femelles <- c("TM_0_0",
                            "TN_0_0",
                            "TX_0_4",
                            "UM_0_0",
                            "TPS_0_0",
                            #"QQ_1_6",
                            #"PP_1_6",
                            "SWV_0_2",
                            "FG_0_5",
                            "EVI_0_5",
                            #"NDVI_0_1",
                            "ALT",
                            "OVI",
                            "BETAIL")


vars_presence_pares <-    c("TM_0_0",
                            "TN_0_0",
                            "TX_0_0",
                            #"UM_0_0",
                            "TPS_0_0",
                            #"QQ_1_6",
                            #"PP_1_6",
                            "SWV_0_2",
                            "FG_0_5",
                            "EVI_0_0",
                            #"NDVI_0_0",
                            "ALT",
                            "OVI",
                            "BETAIL")

df_model_numerical <- df_model %>%
#   select(all_of(vars_presence_nbindiv))%>%
# filter(!is.na(OVI), !is.na(BETAIL), !is.na(EVI_0_3), !is.na(FG_4_6))
  # select(all_of(vars_presence_femelles))%>%
  # filter(!is.na(OVI), !is.na(BETAIL), !is.na(EVI_0_5), !is.na(FG_0_5))
select(all_of(vars_presence_pares))%>%
filter(!is.na(OVI), !is.na(BETAIL), !is.na(EVI_0_0), !is.na(FG_0_5))

############################################### Checking for issues
# test <- df_model_numerical %>%
#   filter(if_any(everything(), is.na))





##### Pearson correlation coefficient
m <- cor(df_model_numerical[,vars_presence_pares], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > 0.7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]]) # p prints highly correlated pairs

#caret package, using pearsons corr coef matrix to get the least corr variables
highCorrVars <- findCorrelation(m, cutoff = 0.7, names = TRUE, exact= TRUE, verbose=F)

# only uncorrelated variables
filtered_vars_presence <- setdiff(colnames(m), highCorrVars) #this will print the selected vars according to pearsons 0.7
# "UM_0_0"  "QQ_1_6"  "EVI_0_3" "SWV_0_4" "FG_4_6"  "ALT"     "OVI"     "BETAIL" #presence NBINDIV

#for predicting on future projected data, need to remove sxv and evi (replace evi with ndvi).



# "PP_1_6"  "SWV_0_2" "FG_0_5"  "EVI_0_5" "ALT"     "OVI"     "BETAIL"      #presence 5 FEMELLES    #VIF proposed UM instead of PP. Will stay with PP
# "PP_1_6"  "SWV_0_2" "FG_0_5"  "EVI_0_0" "ALT"     "OVI"     "BETAIL"      #presenc 5 pares
#### 2nd method via VIF 
variables_pres_corselect<-fuzzySim::corSelect(df_model_numerical, sp.cols="PRES_PARES_NUMERIC", var.cols=vars_presence_pares, coeff = TRUE,
                                              cor.thresh =  0.7,
                                              select =  "VIF", family = "binomial",
                                              use = "pairwise.complete.obs", method = "pearson", verbosity = 1)
variables_pres_corselect_fin<-variables_pres_corselect$selected.vars





### PSELECTED RESENCE MODEL NUMERICAL VARS ARE:

#filtered_numer_presence_vars <- c("TM_0_0","SWV_0_4", "EVI_0_3", "ALT", "OVI", "BETAIL") ##EVI_0_3 is more corr in CCM than NDVI
#filtered_numer_presence_vars2 <- c("TM_0_0","SWV_2_6","EVI_0_2","FG_5_6","ALT","OVI","BETAIL") #lets try with this


filtered_numer_presence_vars <-c("UM_0_0",  "QQ_1_6",  "EVI_0_3", "SWV_0_4", "FG_4_6",  "ALT", "OVI","BETAIL")

#############################       VISUALISATION
##### Plot the bivariate relationship between presence and each selected predictor 
library(ggpubr)
p_pres <- df_model %>% 
  dplyr::select(PRES_CUL_NUMERIC,vars_presence_numerical) %>%
  pivot_longer(-PRES_CUL_NUMERIC) %>%
  ggplot(aes(y = PRES_CUL_NUMERIC, x = value))+
  geom_point() + 
  ylim(c(0,1)) +
  geom_smooth() +
  facet_wrap(.~name, scales = "free") + 
  theme_bw() + 
  ggtitle("Presence Obsoletus/Scoticus ~ potential variables")+
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 4) 


###ths is just to visualise the correlations via heatmap
library(reshape2)
melted_m <- melt(m)
breaks <- c(-1, -0.7, 0.7, 1)  # breakpoints from high to low
labels <- c("-1 (High negative)", "-0.7 (Negative limit)", "0.7 (Positive limit)", "1 (High positive)")  # values in between positive and negative limit are ok
colors <- c("blue", "green", "yellow", "red")  #Blue for r < -0.7, yellow and green for |r| < 0.7, red for r > 0.7

ggplot(melted_m, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = colors,           
    breaks = breaks,  
    labels = labels,               
    name = "Correlation",
    limits = c(-1, 1))+             
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  labs(title = "Correlation Heatmap for Numerical Variables in Obs/Scot Presence model")
#################################################### END OF VIZ
############finished here



#########'Second stage of analysis: multivariate anaylsis using a leave-one-site-out cross validation and a leave-one-session-out cross validation (but juste to valdiate and evaluate the model)
#########' For presence models
# df_model <- df_combined %>% 
#   mutate(PRES_CUL = ifelse(NBINDIV>0,"Presence","Absence")) %>% ## to create a "character" variable for presence or absence 
#   mutate(PRES_CUL = fct_relevel(PRES_CUL,c("Presence","Absence"))) %>%
#   mutate(PRES_CUL_NUMERIC = ifelse(NBINDIV>0,1,0)) %>% ## to create a numeric variable for presence or absence for culicoides
#   relocate(PRES_CUL:PRES_CUL_NUMERIC, .after = NBINDIV)

###### categorical and numerical vars for presence 

#predictors_presence <-c("UM_0_0",  "QQ_1_6",  "EVI_0_3", "SWV_0_4", "FG_4_6",  "ALT", "OVI","BETAIL") #nbindiv
predictors_presence <- c("TX_0_4",  "EVI_0_3" ,"SWV_0_4", "FG_4_6" , "ALT", "OVI","BETAIL" ) #nbindiv new as of 26/04

#predictors_presence <-c("PP_1_6",  "SWV_0_2", "FG_0_5",  "EVI_0_5", "ALT",     "OVI",     "BETAIL") #femelles
predictors_presence <- c("TX_0_4",  "SWV_0_2", "FG_0_5", "EVI_0_5" ,"ALT","OVI", "BETAIL" ) #femelles new as of 27/04

predictors_presence <-c("PP_1_6",  "SWV_0_2", "FG_0_5",  "EVI_0_0", "ALT",     "OVI",     "BETAIL") #pares

predictors_presence <-c( "TN_0_0",  "SWV_0_2", "FG_0_5",  "EVI_0_0", "ALT" ,    "OVI" ,    "BETAIL" )#pares new as of 27/04

df_model_presence <- df_model %>%
  mutate(year=year(as.Date(DATE)))%>%
  # dplyr::select("ID_SITE", "ECO_CLI","DATE", "Cell", "year",  "NBINDIV", "PRES_FEMELLES", predictors_presence) %>%
  # filter(!is.na(EVI_0_5), !is.na(FG_0_5))
  dplyr::select("ID_SITE", "ECO_CLI","DATE", "Cell", "year",  "NBINDIV", "PRES_PARES", predictors_presence) %>%
  filter(!is.na(EVI_0_0), !is.na(FG_0_5))


#Why EVI_0_3 over NDVI? CCM's show EVI to be more highly correlated, and both are good indicators of vegetation biomass


cv_col <- "Cell"


#training model using LLO
indices_cv <- CAST::CreateSpacetimeFolds(df_model_presence, spacevar = cv_col, k = length(unique(unlist(df_model_presence[,cv_col])))) #### Take into acocunt spatil avariability

## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
tr = trainControl(method="cv", ## Definition of method sampling: cross validation
                  index = indices_cv$index,  ##  list of elements to sampling
                  indexOut = indices_cv$indexOut,##  list of items to be set aside for each resampling
                  summaryFunction = twoClassSummary,#comboSummary, ## Calcul of ROC and AUC
                  classProbs = TRUE,
                  savePredictions = 'final',
                  verboseIter = FALSE
)


#### Third step: realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the ROC
mod_presence_LLO <- caret::train(x = df_model_presence[,predictors_presence], y = df_model_presence$PRES_PARES, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE") 

#### Last step: to put predictions on same data frame
df_model_presence$rowIndex <- seq(1,nrow(df_model_presence),1)
df_cv_presence_LLO <- mod_presence_LLO$pred %>%
  left_join(df_model_presence) %>%
  dplyr::select(ID_SITE, ECO_CLI, DATE,year, Cell, Presence ,pred, obs) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)

res_multiv_model_presence_LLO <- list(model = mod_presence_LLO, df_cv = df_cv_presence_LLO, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence_LLO,"./PARES_presence_LLO_TX_EVI.rds")




#########################################################

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
mod_presence_LTO <- caret::train(x = df_model_presence[,predictors_presence], y = df_model_presence$PRES_PARES, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE") 


## Adding prediction with session cross validation 
df_model_presence$rowIndex <- seq(1,nrow(df_model_presence),1)
df_cv_presence_LTO<-mod_presence_LTO$pred %>%
  left_join(df_model_presence)%>% 
  dplyr::select(ID_SITE, ECO_CLI, DATE, Cell, year, Presence , pred, obs) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)

res_multiv_model_presence_LTO <- list(model = mod_presence_LTO, df_cv = df_cv_presence_LTO, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence_LTO,"./5PARES_presence_LTO_TX_EVI.rds")



#####################                     ABUNDANCE MODEL
######################################################################
##################################################################
####################################################
###########################################
### Numerical var selection
numeric_abundance_vars <- c("TM_0_0",
                            "TN_0_0",
                            "TX_0_0",
                            #"UM_0_0",
                            "TPS_0_0",
                            #"QQ_0_6",
                            #"PP_1_5",
                            "EVI_0_6",
                            "SWV_0_2",
                            "FG_0_5",
                            "EVI_0_5",
                            #"NDVI_0_1",
                            "ALT",
                            "OVI",
                            "BETAIL")





df_model_numerical <- df_model %>%
  select(all_of(numeric_abundance_vars))%>%
  filter(!is.na(ALT), !is.na(EVI_0_5), !is.na(FG_0_5))

m <- cor(df_model_numerical[,numeric_abundance_vars], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > 0.7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]]) # p prints highly correlated pairs

highCorrVars <- findCorrelation(m, cutoff = 0.7, names = TRUE, exact= TRUE, verbose=F)
filtered_vars_abundance <- setdiff(colnames(m), highCorrVars) #this will print the selected vars according to pearsons 0.7
#  "UM_0_0"  "PP_1_5"  "EVI_0_6" "SWV_0_2" "FG_0_5"  "ALT"     "OVI"     "BETAIL"  #ABUNDANCE 1NBINDIV

#"TX_0_0"  "SWV_0_2" "FG_0_5"  "EVI_0_5" "ALT"     "OVI"     "BETAIL" var selection for abundance 1NBINDIV 27/04
#### Third select: to select final variables using the VIF 
variables_abond_corselect<-fuzzySim::corSelect(df_model_numerical, sp.cols="NBINDIV", var.cols=numeric_abundance_vars, coeff = TRUE,
                                               cor.thresh =  0.7,
                                               select =  "VIF", family = "truncated_nbinom2",
                                               use = "pairwise.complete.obs", method = "pearson", verbosity = 1)
variables_abond_corselect_fin<-variables_abond_corselect$selected.vars


#c("UM_0_0",   "PP_1_5",   "SWV_0_2",  "FG_0_5" ,  "EVI_0_6", "ALT", "OVI", "BETAIL") 24/4/2025
predictors_abundance <- c("UM_0_0",   "PP_1_5",   "SWV_0_2",  "FG_0_5" ,  "EVI_0_6", "ALT", "OVI", "BETAIL")

predictors_abundance <- c("TX_0_0" , "SWV_0_2", "FG_0_5",  "EVI_0_5", "ALT",     "OVI",     "BETAIL") #27/04/2025

#### Final data frame for the multivariate analysis
df_model_abundance <- df_model %>%
  mutate(year = year(as.Date(DATE)))%>%
  filter(NBINDIV>=1) %>%
  dplyr::select("ID_SITE","DATE","ECO_CLI","Cell", "year", "NBINDIV", "PRES_OBSCOT", predictors_abundance)%>%
  mutate(across(ID_SITE:year, as.factor))%>%
  filter(!is.na(EVI_0_5), !is.na(FG_0_5))



df_model_abundance$NBINDIV <- log(df_model_abundance$NBINDIV) 

#### First step: to parameter the model: leave-one-site-out cross validation
cv_col <- "Cell"

#### Second step: It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)
indices_cv <- CAST::CreateSpacetimeFolds(df_model_abundance, spacevar = cv_col,k = length(unique(unlist(df_model_abundance[,cv_col])))) 

## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
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
  dplyr::select(pred,obs,Cell, ID_SITE, ECO_CLI, DATE, year)

res_multiv_model_abundance_LLO <- list(model = mod_abundance_LLO, df_cv = df_cv_abundance_LLO, df_mod = df_model_abundance) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_abundance_LLO, "./1NBINDIV_abundance_LLO_TX_EVI.rds")

#### Additionnal step: same method but with a cross validation with the numero of sampling session 

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
  dplyr::select(pred,obs,Cell, ID_SITE, ECO_CLI, DATE, year)

res_multiv_model_abundance_LTO <- list(model = mod_abundance_LTO, df_cv = df_cv_abundance_LTO, df_mod = df_model_abundance)  ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_abundance_LTO, "./1NBINDIV_abundance_LTO_TX_EVI.rds")





