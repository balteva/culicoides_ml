########################### Opening packages

library(tidyverse) 
library(caret) 
library(CAST) 
library(ranger) 
library(correlation) 
library(fuzzySim) 

# Loading france separation via grid file
grid <- read.csv("../../data/culicoides_point_locations_grid_150_150.csv")%>%
  select(ID_SITE, Cell, longitude, latitude)

# loading landscape data and cleaning the df for NA/empty values

dirty_data <- read.csv("../../data/ocapi_paysagere_micro_climat_obscot.csv") %>%
  select(-c(X, COMMUNELOC, DESCRIPTION_EXPLOIT, DESCRIPTION_EXT, DESCRIPTION_INT,
            DATEDETERMINATIONDEF, OBSERVATIONSDIVERSES, CHGTEMPLACEMENT))
dirty_data <- dirty_data %>%
  mutate(across(everything(), as.character)) ###changing everything to char to remove NULL and "" entries

data_clean <- dirty_data %>%
  mutate(across(everything(), ~ na_if(.x, "NULL"))) %>% #making all NULL and "" values into NA
  mutate(across(everything(), ~ na_if(.x, "")))%>%
  select(c(ID_SITE,DATE, TEMPERATUREDEBUT, TEMPERATUREFIN, TEMPMAX, TEMPMINI, ALT, OVI, BETAIL, SITUATIONPIEGE, VENTFIN, VENTDEBUT, PLUIEDEBUT, CLC, OUVERTURE_BAT, ACTIV_VIANDE))%>%
  mutate(across(TEMPERATUREDEBUT:BETAIL, as.numeric))



# loading meteo vars
var_meteo <- read.csv("../../data/df_to_model_fixed_obscot.csv")%>%
  mutate(num_releve = seq(1,nrow(.),1)) %>%
  mutate(idpointdecapture = paste(ID_SITE,num_releve, sep="_"))%>%
  select(c(idpointdecapture,ID_SITE, date,NBINDIV,
           TM_0_0, TM_0_1, TM_0_2, #Avg temp
           TN_0_0, TN_0_2, TN_0_1, #min Temp
           TX_0_0, TX_0_2, TX_1_4, TX_0_4, #Max temp
           UM_0_0, UM_0_1, #Relative Humidity
           TPS_0_0, TPS_0_1, TPS_0_2, #Soil Temp
           QQ_0_5, QQ_1_6, #Radiation
           PP_3_3, PP_1_5, PP_3_5,#Photoperiod
           NDVI_0_6,
           EVI_0_6)) %>%
  rename(DATE= date)

#combining all df (grid, meteo and landscape)
df_combined <- merge(x = data_clean, y = var_meteo, by=c("ID_SITE", "DATE"),all.x=TRUE)%>% 
 left_join(grid)%>%
  relocate(Cell, .after = ID_SITE)%>%
  relocate(idpointdecapture, .before=ID_SITE)%>%
  relocate(NBINDIV, .after = DATE)

#creating clumns for presence/absence for the presence model
df_model <- df_combined %>% 
  mutate(PRES_CUL = ifelse(NBINDIV>0,"Presence","Absence")) %>% ## to create a "character" variable for presence or absence 
  mutate(PRES_CUL = fct_relevel(PRES_CUL,c("Presence","Absence"))) %>%
  mutate(PRES_CUL_NUMERIC = ifelse(NBINDIV>0,1,0)) %>% ## to create a numeric variable for presence or absence for culicoides
  filter(!is.na(NBINDIV))%>%
  filter(!is.na(TEMPMINI),!is.na(TEMPMAX),!is.na(TEMPERATUREDEBUT), !is.na(TEMPERATUREFIN), !is.na(OVI), !is.na(BETAIL), !is.na(VENTFIN), !is.na(VENTDEBUT), !is.na(SITUATIONPIEGE), !is.na(PLUIEDEBUT), !is.na(OUVERTURE_BAT), !is.na(ACTIV_VIANDE), !is.na(CLC))%>% ## to delete data where micro climatic variables are not present 
  relocate(PRES_CUL:PRES_CUL_NUMERIC, .after = NBINDIV)

#########Presence model preparation.
#stage 1: var selection.

#########################categorical vars only

categ_presence_vars <- c("SITUATIONPIEGE", "ACTIV_VIANDE", "OUVERTURE_BAT", "PLUIEDEBUT", "VENTDEBUT", "VENTFIN", "CLC") #selecting only categorical vars for presence for chi square test

categ_presence_df <- df_model %>%
  select(categ_presence_vars)%>% #(ID_SITE, DATE, PRES_CUL, 
  mutate(across(SITUATIONPIEGE:CLC, as.factor))

# install.packages("lsr")
# library(lsr)
install.packages("sjstats")
library(sjstats)



######################## function for calculating correlation  btw categorical vars (need to look at output manually).
# high p values (p>0.05) means that the vars are independent.
#therefore im choosing those vars that had a high p value as my categorical vars


#tests for if the relationship is significant btw 2 categorical vars. Low p value means relationship is not significant, therefore will pick those with low p
fishers_func_categorical <- function(df, vars, directory) {
  output_file <- directory
  sink(output_file)
  
  var_combinations <- combn(vars, 2)  #makes unique combinations so wont have redundant pairing
  
  for (i in 1:ncol(var_combinations)) {#will give the number of unique combinations 
    var1 <- var_combinations[1, i] #takes the first variable in the pair
    var2 <- var_combinations[2, i] #takes the second variable in the pair 
    
    cat("\n--------------------------------------------\n")
    cat("Fisher's Exact test between", var1, "and", var2, "\n")
    
    table_temp <- table(df[[var1]], df[[var2]])
    test_result <- fisher.test(table_temp, simulate.p.value = TRUE)
    print(test_result)
  }
  sink()
} 

#fishers_func_categorical(categ_presence_df, categ_presence_vars, "C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/var_selection/categ_presence_var_fishers.txt")
#####           after running this func, need to look at the output file to select the non confounding vars

#put in the selected categorical vars in a new vector called 'filtered' according to fishers

#filtered_categ_presence_vars_fisher <- c("SITUATIONPIEGE", "ACTIV_VIANDE", "VENTFIN")


#tests if the relationship strength btw 2 categorical vars. 
# range 0 - 0.1 = very weak association
# range 0.1 - 0.2 = weak association
# range 0.2 - 0.3 moderate
# range > 0.3 strong asssocation

cramersv_function <- function(df, vars) {
  
  result <- data.frame(Comparison=character(), CramersV = numeric(), stringsAsFactors = FALSE) #initiating empty df to store results
  var_combinations <- combn(vars, 2)  #makes unique combinations so wont have redundant pairing
  
  for (i in 1:ncol(var_combinations)) {#will give the number of unique combinations 
    var1 <- var_combinations[1, i] #takes the first variable in the pair
    var2 <- var_combinations[2, i] #takes the second variable in the pair 
    
    formula <- as.formula(paste(var1, "~", var2))
    
    coefficient <- cramer(formula, data=df)
    result <- rbind(result, data.frame(Comparison = paste(var1, "~", var2), CramersV = coefficient))
  }
  return(result)
} 

cramers_vars_presence <- cramersv_function(categ_presence_df,categ_presence_vars )

strong_assoc_vars <- cramers_vars_presence%>%
  filter(CramersV >= 0.2)

weak_assoc_vars <- cramers_vars_presence%>%
  filter(CramersV <= 0.2)

filtered_categ_presence_vars_cramersv <- c("SITUATIONPIEGE", "OUVTERTURE_BAT", "ACTIV_VIANDE", "VENTFIN", "PLUIEDEBUT")# only issue is maybe ouverture_bat and situation_piege bcs theyre correlated
#can choose either "SITUATIONPIEGE" or "OUVERTURE_BAT"; can choose "VENTFIN" or "VENTDEBUT". Ventfin because chi sq test output prefered this one
#situation piege more informative for explaining current data, ouverture_bat maybe have more predictive power

###########################################"""
##### Numerical var selection for presence model
vars_presence_numerical <- c("TM_0_0", "TM_0_2", "TN_0_0", "TN_0_1", "TX_0_4", "TX_1_4",
                             "UM_0_0", "TPS_0_0", "TPS_0_2", "QQ_1_6", "PP_3_5", "PP_1_5","TEMPERATUREDEBUT", "TEMPERATUREFIN", "TEMPMINI", "TEMPMAX",
                              "ALT", "OVI", "BETAIL")

#if I remove these vars : "TEMPERATUREDEBUT", "TEMPERATUREFIN", "TEMPMINI", "TEMPMAX", the VIF method wont select temp vars at all
#because they are good predictors, I will replace TEMPMAX and TEMPMINI with TN_0_0 and TX_0_4


##### Pearson correlation coefficient
m <- cor(df_model[,vars_presence_numerical], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > 0.7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]]) # p prints highly correlated pairs

#caret package, using pearsons corr coef matrix to get the least corr variables
highCorrVars <- findCorrelation(m, cutoff = 0.7, names = TRUE, exact= TRUE, verbose=TRUE)
# only uncorrelated variables
filtered_vars_presence <- setdiff(colnames(m), highCorrVars) #this will print the selected vars according to pearsons 0.7

#### 2nd method via VIF (used this one)
variables_pres_corselect<-fuzzySim::corSelect(df_model, sp.cols="PRES_CUL_NUMERIC", var.cols=vars_presence_numerical, coeff = TRUE,
                                    cor.thresh =  0.7,
                                    select =  "VIF", family = "binomial",
                                    use = "pairwise.complete.obs", method = "pearson", verbosity = 1)
variables_pres_corselect_fin<-variables_pres_corselect$selected.vars


### PSELECTED RESENCE MODEL NUMERICAL VARS ARE:
#"UM_0_0", "PP_1_5", "TX_0_4" instead of "TEMPMAX","TN_0_0" instead of "TEMPMINI", "ALT", "OVI","BETAIL" | REMOVED "TEMPERATUREFIN"
filtered_numer_presence_vars <- c("UM_0_0","PP_1_5", "TX_0_4","TN_0_0","ALT", "OVI","BETAIL", "TEMPERATUREFIN" )

#############################       vISUALISATION
##### Plot the bivariate relationship between presence and each selected predictor 
p_pres <- df_model %>% 
  dplyr::select(PRES_CUL_NUMERIC,vars_presence_numerical) %>%
  pivot_longer(-PRES_CUL_NUMERIC) %>%
  ggplot(aes(y = PRES_CUL_NUMERIC, x = value))+
  geom_point() + 
  ylim(c(0,1)) +
  geom_smooth() +
  facet_wrap(.~name, scales = "free") + 
  theme_bw() + 
  ggtitle("Presence Obsoletus/Scoticus ~ selected variables")+
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 4) 

#ggsave("../../multivariate_anal/plot_presence_vs_selected_vars_poll.png",p_pres,width = 9.65, height = 6.42, units = "in") 

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
    labels = labels,               # Assign labels to each range
    name = "Correlation",
    limits = c(-1, 1))+              # Make sure the color scale spans the full correlation range
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  labs(title = "Correlation Heatmap for Numerical Variables in Obs/Scot Presence model")
#################################################### END OF VIZ

#########'Second stage of analysis: multivariate anaylsis using a leave-one-site-out cross validation and a leave-one-session-out cross validation (but juste to valdiate and evaluate the model)
#########' For presence models

###### categorical and numerical vars for presence 
predictors_presence <-  c(filtered_numer_presence_vars, filtered_categ_presence_vars_cramersv)

df_model_presence <- df_model %>%
  mutate(year=year(as.Date(DATE)))%>%
  dplyr::select("idpointdecapture", "ID_SITE", "DATE", "Cell", "year",  "NBINDIV", "PRES_CUL", predictors_presence) %>%
  mutate(across(SITUATIONPIEGE:VENTFIN, as.factor))%>%
  mutate(across(ID_SITE:year, as.factor))


#### First step: to parameter the model: LLO CV
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
mod_presence <- caret::train(x = df_model_presence[,predictors_presence], y = df_model_presence$PRES_CUL, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE") 
# ROC for presence/absence models

#### Last step: to put predictions on same data frame
df_model_presence$rowIndex <- seq(1,nrow(df_model_presence),1)
df_cv_presence <- mod_presence$pred %>%
  left_join(df_model_presence) %>%
  dplyr::select(pred,Presence,obs,cell,DATE,idpointdecapture, ID_SITE) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)

res_multiv_model_presence <- list(model = mod_presence, df_cv = df_cv_presence, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence,"C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/res_multiv_model_presence.rds")


#### Additionnal step: same method but with a cross validation with the numero of sampling session 

## To parameter the model: leave-one-session-out cross validation
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
mod_presence_session <- caret::train(x = df_model_presence[,predictors_presence], y = df_model_presence$PRES_CUL, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE") 
## Adding prediction with session cross validation 
df_cv_presence_session<-mod_presence_session$pred %>%
  left_join(df_model_presence) %>%
  dplyr::select(pred,Presence,obs,year,DATE,idpointdecapture, ID_SITE) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)
res_multiv_model_presence_session <- list(model = mod_presence_session, df_cv = df_cv_presence_session, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence,"C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/res_multiv_model_presence_year.rds")

##################### ABUNDANCE MODEL


###########################
#########'Abundance model preparation
#########'First step: to select for meteorological and pollutants variables, for every type of variable, the time lag for which the r2 was the highest. Same work is realized for micro climatic, land cover (for each buffer) and socio demographic data.
#########'Second step: to evaluate the correlation between these variables.
#########'Third step: to select the variables not correlated with the highest sense ecological. The first selection is crossed with the other selection done with the VIF with the corSelect function of the fuzzySim package to select variables with the lowest VIF. The final selection is a mixed of both methods.
###########################

##### First step: select variables for abundance models
predictors_abundance <- c("RFDode_6_6", 
                          "RHmf_5_6", "WINDmf_0_1",
                          "GDDjour_1_1",
                          "PM2.5_0_1", "PM10_0_1", "O3_0_0","NO2_0_0", "NOX_0_0",
                          "lsm_c_pland_LCG_20_12",  # %  vegetation higher than 3 m at 20 m
                          "lsm_c_pland_LCG_50_13",  # %  vegetation lower than 3 m at 50 m
                          "lsm_c_pland_LCG_50_10",  # % building at  100 m
                          "lsm_c_pland_LCG_100_11",  # % roads at100 m
                          "lsm_l_shdi_LCG_20_NA",   # Shannon diversity index at  20 m
                          "RHMEAN_collection", "RFSUM_48hprec", "TMEAN_collection", "TMIN_collection", "TMAX_collection",#,
                          "FIL_Log_av45" # number of building built before 1945
)   



df_model_abundance <- df_model %>%
  filter(NB_ALBO_TOT>0) %>%
  dplyr::select("idpointdecapture", "ID_PIEGE", "num_session","ID_COLLECTE", "ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "lieu",  "NB_ALBO_TOT", "PRES_ALBO", predictors_abundance)

#####  Plot the bivariate relationship between abundance and each selected predictor 
p_ab <- df_model_abundance %>% 
  dplyr::select(NB_ALBO_TOT,predictors_abundance) %>%
  pivot_longer(-NB_ALBO_TOT) %>%
  ggplot(aes(y = NB_ALBO_TOT, x = value)) +
  geom_point() + 
  geom_smooth() +
  ylim(c(0,80)) +
  facet_wrap(.~name, scales = "free_x") + 
  theme_bw() + 
  ggtitle("Abondance albo ~ variables séléctionnées")

ggsave("02_Data/processed_data/plots/modelling_adults_abundance/plot_abundance_vs_selected_vars_poll.png",p_ab,width = 9.65, height = 7.42, units = "in") ## to save

##### Second step: identify correlated variables that are greater than 0.7 (Pearson correlation coefficient)
m <- cor(df_model_abundance[,predictors_abundance], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]])

#### Third select: to select final variables using the VIF 
variables_abond_corselect<-fuzzySim::corSelect(df_model, sp.cols="NB_ALBO_TOT", var.cols=predictors_abundance, coeff = TRUE,
                                               cor.thresh =  0.7,
                                               select =  "VIF", family = "truncated_nbinom2",
                                               use = "pairwise.complete.obs", method = "pearson", verbosity = 1)
variables_abond_corselect_fin<-variables_abond_corselect$selected.vars

# RFDode_6_6"            "RHmf_5_6"              "WINDmf_0_1"            "PM10_0_1"              "O3_0_0"                "NOX_0_0"               "lsm_c_pland_LCG_50_13"
# "lsm_c_pland_LCG_50_10" "lsm_l_shdi_LCG_20_NA"  "RHMEAN_collection"     "TMAX_collection"       "FIL_Log_av45"             


## Final variables selections
predictors_abundance <- c("RFDode_6_6", "GDDjour_1_1", "WINDmf_0_1",
                          "lsm_c_pland_LCG_20_12",  
                          "lsm_c_pland_LCG_50_13",  
                          "TMAX_collection",
                          "RHMEAN_collection",
                          "NOX_0_0")

#### Final data frame for the multivariate analysis
df_model_abundance <- df_model %>%
  filter(NB_ALBO_TOT>0) %>%
  dplyr::select("idpointdecapture", "ID_PIEGE", "num_session","ID_COLLECTE", "ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "lieu",  "NB_ALBO_TOT", "PRES_ALBO", predictors_abundance)



###########################
#########'Second stage of analysis: multivariate anaylsis using a leave-one-site-out cross validation and a leave-one-session-out cross validation (but juste to valdiate and evaluate the model)
#########' For abundance models
###########################

df_model_abundance$NB_ALBO_TOT <- log(df_model_abundance$NB_ALBO_TOT) 

#### First step: to parameter the model: leave-one-site-out cross validation
cv_col <- "lieu"

#### Second step: It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)
indices_cv <- CAST::CreateSpacetimeFolds(df_model_abundance, spacevar = cv_col,k = length(unique(unlist(df_model_abundance[,cv_col])))) 

## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
tr = trainControl(method="cv",
                  index = indices_cv$index, 
                  indexOut = indices_cv$indexOut,
                  savePredictions = 'final')


#### Third step: realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the MAE
mod_abundance <- caret::train(x = df_model_abundance[,predictors_abundance], y = df_model_abundance$NB_ALBO_TOT, method = "ranger", tuneLength = 10, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")

#### Last step: to put predictions on same data frame
df_model_abundance$rowIndex <- seq(1,nrow(df_model_abundance),1)
df_cv_abundance <- mod_abundance$pred %>%
  left_join(df_model_abundance) %>%
  dplyr::select(pred,obs,ZONE,lieu,num_session,idpointdecapture, ID_PIEGE)

res_multiv_model_abundance <- list(model = mod_abundance, df_cv = df_cv_abundance, df_mod = df_model_abundance) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_abundance,"02_Data/processed_data/01_Adults_Abundance/res_multiv_model_abundance.rds")

#### Additionnal step: same method but with a cross validation with the numero of sampling session 

## To parameter the model: leave-one-session-out cross validation
cv_col <- "num_session"
##It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)
indices_cv <- CAST::CreateSpacetimeFolds(df_model_abundance, spacevar = cv_col,k = length(unique(unlist(df_model_abundance[,cv_col])))) 
## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
tr = trainControl(method="cv",
                  index = indices_cv$index, 
                  indexOut = indices_cv$indexOut,
                  savePredictions = 'final')
## realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the MAE
mod_abundance_session <- caret::train(x = df_model_abundance[,predictors_abundance], y = df_model_abundance$NB_ALBO_TOT, method = "ranger", tuneLength = 10, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")
## Adding prediction with session cross validation 
mod_abundance_session$rowIndex <- seq(1,nrow(df_model_abundance),1)
df_cv_abundance_session <- mod_abundance_session$pred %>%
  left_join(df_model_abundance) %>%
  dplyr::select(pred,obs,ZONE,lieu,num_session,idpointdecapture, ID_PIEGE)
res_multiv_model_abundance_session <- list(model = mod_abundance_session, df_cv = df_cv_abundance_session, df_mod = df_model_abundance)  ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_abundance_session,"02_Data/processed_data/01_Adults_Abundance/res_multiv_model_abundance_art_session.rds")



