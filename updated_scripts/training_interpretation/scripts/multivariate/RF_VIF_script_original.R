library(tidyverse) 
library(caret) 
library(CAST) 
library(ranger) 
library(correlation) 
library(fuzzySim) 

aaa <- readRDS("./LTO_abundance_RR.rds")
modelaaa <- aaa$model #model details (mtrees, specificity, sensitivity)
df <- aaa$df_mod #og dataframe
df_cv <-  aaOa$df_cv #og data frame with predictions

##easier to understand importace measure
importance <- varImp(modelaaa, scale=T)
plot(importance)

# loading landscape data and cleaning the df for NA/empty values

micro_landscape_df<- read.csv('./training_interpretation/data/df_landscape_for_multivariate.csv')%>%
  mutate(NBINDIV = as.numeric(NBINDIV), PRES_CUL= as.numeric(PRES_CUL))%>%
  mutate(PLUIEDEBUT=factor(fct_relevel(PLUIEDEBUT, "Aucune","Brouillard / Bruine", "Pluie fine", "Pluie forte"), ordered = T),
         PLUIEFIN=factor(fct_relevel(PLUIEFIN, "Aucune","Brouillard / Bruine", "Pluie fine", "Pluie forte"), ordered = T),
         VENTDEBUT= factor(fct_relevel(VENTDEBUT, "Vent nul", "Légère brise à vent faible", "Vent moyen","Vent fort à grand vent"), ordered = T),
         VENTFIN= factor(fct_relevel(VENTFIN, "Vent nul", "Légère brise à vent faible", "Vent moyen","Vent fort à grand vent"), ordered = T),
         NUAGEDEBUT= factor(fct_relevel(NUAGEDEBUT, "Très beau","Beau", "Couvert","Complètement couvert"), ordered = T),
         NUAGEFIN= factor(fct_relevel(NUAGEFIN, "Très beau","Beau", "Couvert","Complètement couvert"), ordered = T),
         OUVERTURE_BAT= factor(fct_relevel(OUVERTURE_BAT, "0 - 5 %","5 - 25 %", "25 - 75 %","75 - 100 %"), ordered = T))

#for the second model replacing tempmin with TM/TN/TX:
#force added for presence model : SWV_0_4, NDVI_0_1, EVI_0_3
#force added for abundance model : SWV_0_0, NDVI_0_6

#for the third model did not add microclimatic temp vars, and included those found significant by spearmans correlation


# loading meteo vars (for the model replacing tempmin with TM/TX/TN)
# ccm_meteo <- read.csv("./updated_scripts/data/df_to_model_obscot.csv")%>%
#   select(c(ID_SITE, DATE,
#            TM_0_0, TM_0_1, TM_0_2, #Avg temp
#            TN_0_0, TN_0_2, TN_0_1, #min Temp
#            TX_0_0, TX_0_2, TX_0_4, #Max temp
#            UM_0_0, UM_0_1, #Relative Humidity
#            SWV_0_4, SWV_0_0, #water volume in soil, i force this var because biologically important even tho corr was 0.15
#            TPS_0_0, TPS_0_1, TPS_0_2, #Soil Temp
#            QQ_0_5,QQ_1_5, QQ_1_6, #Radiation
#            PP_0_5, PP_5_5 ,PP_3_3, PP_1_5,#Photoperiod
#            NDVI_0_1, NDVI_0_6, #force this one as well for abundance
#            EVI_0_6, EVI_1_5, EVI_0_3)) #force added EVI_0_3 for presence

ccm_meteo <- read.csv("./training_interpretation/data/df_to_model_obscot.csv") %>%
  select(c(ID_SITE, DATE,
           # PRESENCE VARS 
           # col1 = most correlated, #col2= biologically important but still highly corr
           # TM_0_0, TM_0_2, #average temperature, 
           # TN_0_0, TN_0_5, #min temperature
           # TX_0_4,#max temp
           # UM_0_0, UM_0_3, #relative humidity
           # TPS_0_0, TPS_0_2, #Soil temperature
           # QQ_1_6, #solar radiation
           # SWV_0_4, SWV_2_5, #water volume in soil
           # PP_5_5, PP_2_5, #phoroperiod
           # FG_4_6,     #wind strength
           # NDVI_0_1,NDVI_0_4,
           # EVI_0_2, EVI_0_4,
           # ABUNDANCE VARS
           TM_0_0, TM_0_1, #mean temp
           TN_0_0, TN_0_2, #min temp
           TX_0_0, TX_0_3, #max temp
           UM_0_0, UM_0_1,
           TPS_0_0, TPS_0_2,
           SWV_0_0, SWV_0_3,
           QQ_0_5, 
           PP_3_3, PP_1_5,
           FG_0_3, FG_0_1,
           NDVI_0_5,
           EVI_0_6, EVI_0_4,
           RR_0_3))



#filtered_meteo <- ccm_meteo %>% filter(is.na(TX_0_4)) #check foor errors

df_combined <-micro_landscape_df %>%
  left_join(ccm_meteo)%>%
  relocate(NBINDIV, .after = DATE)%>%
  mutate(CLC_level2=as.factor(CLC_level2))



#filtered_combined <- df_combined %>% filter(is.na(TX_0_4)) #check for errors

# duplicates <- df_combined %>% #check for errors
#   group_by(ID_SITE, DATE) %>%
#   filter(n() > 1)%>%
#   as.data.frame()


#creating clumns for presence/absence for the presence model
df_model <- df_combined %>% 
  select(-c(longitude,latitude))%>%
  mutate(PRES_CUL = ifelse(NBINDIV>0,"Presence","Absence")) %>% ## to create a "character" variable for presence or absence 
  mutate(PRES_CUL = fct_relevel(PRES_CUL,c("Presence","Absence"))) %>%
  mutate(PRES_CUL_NUMERIC = ifelse(NBINDIV>0,1,0)) %>% ## to create a numeric variable for presence or absence for culicoides
  mutate(across(TEMPMINI:TEMPMAX, as.numeric ), across(TEMPERATUREDEBUT:TEMPERATUREFIN, as.numeric))%>%
  relocate(PRES_CUL:PRES_CUL_NUMERIC, .after = NBINDIV) %>%
  mutate(across(ANIMAUXDEBUT:ANIMAUXFIN, as.factor), across(ELEV_OVIN:OUVERTURE_BAT, as.factor))


#stage 1: var selection.

categ_presence_vars <- c("VENTDEBUT", "VENTFIN", "SITUATIONPIEGE", "OUVERTURE_BAT", "PLUIEDEBUT", "NUAGEFIN", "ACTIV_VIANDE") #selecting only categorical vars for presence for chi square test

categ_presence_df <- df_combined %>%
  select(all_of(categ_presence_vars))%>% 
  na.omit()%>%
  mutate(across(SITUATIONPIEGE:VENTFIN, as.factor))

######################## function for calculating correlation  btw categorical vars (need to look at output manually).
# high p values (p>0.05) means that the vars are independent == choosing those vars that had a high p value


#tests if the frequency of distribution of one variable is dependent on the other  p < 0.05 rejects the H0 of independence, meaning the relationship is dependent.
#therefore we choose vars with p > 0.05 because they were found to be independently distributed
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


fishers_func_categorical(categ_presence_df, categ_presence_vars, "./updated_scripts/data/significance_tests/obscot_presence_fishers_reduced_df_vars.txt")


#put in the selected categorical vars in a new vector called 'filtered' according to fishers

#fisher_vars_presence <- c("ACTIV_VIANDE", "VENTDEBUT","PLUIEDEBUT", "OUVERTURE_BAT")


#tests if the relationship strength btw 2 categorical vars. 
# range 0 - 0.1 = very weak association
# range 0.1 - 0.2 = weak association
# range 0.2 - 0.3 moderate
# range > 0.3 strong asssocation


library(sjstats) #for calculating cramers v
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
  filter(CramersV <= 0.15)
#the presence vars were ran across biserial and kruskal testing to remove highly correlated cat x numerical values
cramersv_presence_vars <- c("SITUATIONPIEGE", "ACTIV_VIANDE", "VENTDEBUT") #VENTEBUT was found to be corr to altitude

cramersv_presence_vars <- c("OUVERTURE_BAT", "PLUIEDEBUT")


#the presence vars were ran across biserial and kruskal testing to remove highly correlated cat x numerical values
##### Numerical var selection for presence model
vars_presence_numerical <- c("TM_0_0", "TM_0_2", "TN_0_0", "TN_0_1", "TX_0_4",
                             "TPS_0_0", "TPS_0_2", "SWV_0_4", "EVI_0_3", # #"NDVI_0_1 #forced added the last 3 for the presence model
                             "ALT", "OVI", "BETAIL") #"TEMPERATUREDEBUT", "TEMPERATUREFIN", "TEMPMINI", "TEMPMAX" removing these because i they dont give as accurate details about micoclimate (random sampling during the day, relying on meteo data + errors in input)
#choosing only those that i find relevant, therefore i force remove:
#"QQ_1_6", "QQ_1_5","PP_5_5", "PP_0_5" ## trying to remove "UM_0_0" to see if it gives temp
############################################################"
############################################################"
vars_presence_numerical <- c(
"TM_0_0", 
"TM_0_2", 
"TN_0_0",
"TN_0_5",
#"TX_0_4", max temp is better for abundance imo
#"TX_0_0",
#"UM_0_0", 
#"UM_0_3",
"TPS_0_0", "TPS_0_2",
#"QQ_1_6",#removing solar rad because ERA5 overestimates + harder to interpret compared to temperature
"SWV_0_4", "SWV_2_5",
#"PP_2_5", "PP_5_5",
"FG_4_6",
"NDVI_0_1","NDVI_0_4",
"EVI_0_2",
"EVI_0_4",
"ALT",
"OVI", "BETAIL")

df_model_numerical <- df_model %>%
  select(all_of(vars_presence_numerical))%>%
filter(!is.na(OVI), !is.na(BETAIL), !is.na(ALT), !is.na(NDVI_0_1), !is.na(NDVI_0_4),!is.na(EVI_0_2), !is.na(EVI_0_4))

##### Pearson correlation coefficient
m <- cor(df_model_numerical[,vars_presence_numerical], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > 0.7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]]) # p prints highly correlated pairs

#caret package, using pearsons corr coef matrix to get the least corr variables
highCorrVars <- findCorrelation(m, cutoff = 0.7, names = TRUE, exact= TRUE, verbose=F)
# only uncorrelated variables
filtered_vars_presence <- setdiff(colnames(m), highCorrVars) #this will print the selected vars according to pearsons 0.7
# old selection : "UM_0_0"         "ALT"            "OVI"            "BETAIL"          "TEMPMINI"      
# new selection after forcing : TX_0_4"  "SWV_0_4" "EVI_0_3" "ALT"     "OVI"     "BETAIL"
#### 2nd method via VIF (used this one)
variables_pres_corselect<-fuzzySim::corSelect(df_model_numerical, sp.cols="PRES_CUL_NUMERIC", var.cols=vars_presence_numerical, coeff = TRUE,
                                              cor.thresh =  0.7,
                                              select =  "VIF", family = "binomial",
                                              use = "pairwise.complete.obs", method = "pearson", verbosity = 1)
variables_pres_corselect_fin<-variables_pres_corselect$selected.vars


### PSELECTED RESENCE MODEL NUMERICAL VARS ARE:

#filtered_numer_presence_vars <- c("TM_0_0","SWV_0_4", "EVI_0_3", "ALT", "OVI", "BETAIL") ##EVI_0_3 is more corr in CCM than NDVI
#filtered_numer_presence_vars2 <- c("TM_0_0","SWV_2_6","EVI_0_2","FG_5_6","ALT","OVI","BETAIL") #lets try with this
filtered_numer_presence_vars3 <-c("TN_0_0", "SWV_2_5","FG_4_6", "NDVI_0_1","ALT", "OVI","BETAIL") #kinda happy with this selection
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


# the selected vars "TN_0_0", "SWV_2_5","FG_4_6","NDVI_0_1", "ALT","OVI","BETAIL", "OUVERTURE_BAT", "PLUIEDEBUT"  

predictors_presence <-  c(filtered_numer_presence_vars3, cramersv_presence_vars)

grid <- read.csv("../../../qgis/culicoides_point_locations_grid_150_150.csv") %>%
  select(ID_SITE, Cell)

df_model<- df_model %>%
  left_join(grid)

#write.csv(df_model, "./df_model_for_RF.csv", row.names = F)

df_model_presence <- df_model %>%
  mutate(year=year(as.Date(DATE)))%>%
  dplyr::select("idpointdecapture", "ID_SITE", "ECO_CLI","DATE", "Cell", "year",  "NBINDIV", "PRES_CUL", predictors_presence) %>%
  mutate(across(OUVERTURE_BAT:PLUIEDEBUT, as.factor))%>% 
  mutate(across(ID_SITE:year, as.factor))%>%
  filter(!is.na(OUVERTURE_BAT), !is.na(PLUIEDEBUT), !is.na(NDVI_0_1), !is.na(FG_4_6))
#nrow(df_model_presence) #after filtering out NA =  12132 (before was 14895)

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
mod_presence_LLO <- caret::train(x = df_model_presence[,predictors_presence], y = df_model_presence$PRES_CUL, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE") 
# ROC for presence/absence models ##tunelength = the number of features the model will try in mtry

#### Last step: to put predictions on same data frame
df_model_presence$rowIndex <- seq(1,nrow(df_model_presence),1)
df_cv_presence_LLO <- mod_presence_LLO$pred %>%
  left_join(df_model_presence) %>%
  dplyr::select(idpointdecapture, ID_SITE, ECO_CLI, DATE, Cell, Presence ,pred, obs) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)

res_multiv_model_presence_LLO <- list(model = mod_presence_LLO, df_cv = df_cv_presence_LLO, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence_LLO,"./updated_scripts/models/reduced_RF_model_presence_LLO_interpretation.rds")



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
mod_presence_LTO <- caret::train(x = df_model_presence[,predictors_presence], y = df_model_presence$PRES_CUL, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE") 


## Adding prediction with session cross validation 
df_cv_presence_LTO<-mod_presence_LTO$pred %>%
  left_join(df_model_presence) %>% 
  dplyr::select(idpointdecapture, ID_SITE, ECO_CLI, DATE, Cell, year, Presence , pred, obs) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)
res_multiv_model_presence_LTO <- list(model = mod_presence_LTO, df_cv = df_cv_presence_LTO, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence_LTO,"./updated_scripts/models/reduced_RF_model_presence_LTO_interpretation.rds")



##################### ABUNDANCE MODEL

	

##### First step: select variables for abundance models
categ_abundance_vars <- c("VENTDEBUT", "PLUIEDEBUT", 
                          #"PLUIEFIN",
                          #"ANIMAUXFIN",
                          "ELEV_OVIN", 
                          "OUVERTURE_BAT")
                          #"ELEV_CAPRIN",
                          #"ACTIV_VIANDE",
                          #"CLC_level2",



categ_abundance_df <- df_combined %>%
  select(all_of(categ_abundance_vars))%>%
  mutate(across(everything(), as.factor)) %>%
  na.omit()

fishers_func_categorical(categ_abundance_df, categ_abundance_vars, "./updated_scripts/data/significance_tests/more_reduced_obscot_abundance_fishers.txt")
#look at file to select non corr variables ( p > 0.05)
#VENTFIN et ACTIV_VIANDE, VENTDEBUT et ANIMAUXFIN, PLUIEDEBUT et ACTIV_VIANDE, PLUIEDEBUT et CLC, PLUIEDEBUT et ELEV_OVIN, PLUIEFIN et ACTIV_VIANDE, PLUIEFIN et CLC

cramers_vars_abundance <- cramersv_function(categ_abundance_df, categ_abundance_vars)
strong_assoc_vars <- cramers_vars_abundance %>%
  filter(CramersV >= 0.15)

weak_assoc_vars <- cramers_vars_abundance %>%
  filter(CramersV <= 0.15)

#ANIMAUXFIN, ELEV_OVIN or ACTIV_VIANDE, OUVERTURE_BAT, PLUIEDEBUT, VENTFIN 
#or could do CLC instead of ACTIV_VIANDE + ELEV_OVIN +OUVERTURE_BAT
#filtered_categ_abundance_vars <-c("ELEV_OVIN", "CLC_level2", "PLUIEDEBUT", "VENTDEBUT") #chose elev ovin because could be dairy cows



#after running biserial correlation and Kruskal Wallis test, where ALT was correlated to wind and rain, decided to keep altitude and remove all categorical vars 
#except for elev_ovin

#filtered_categ_abundance_vars <-("ELEV_OVIN")
filtered_categ_abundance_vars <- ("VENTDEBUT") #but remove ALT from numerical

### Numerical var selection
numeric_abundance_vars <- c("TM_0_0",	 #average temperature
                            "TN_0_0",	#min temp
                            "TX_0_0",	 #max temp
                            #"UM_0_0",	"UM_0_1", #relative humiditty #will probably remove relative humidity
                            "TPS_0_0",  #soil temperature
                            #"QQ_0_5",	 #solar radiation
                            #"PP_3_3",	"PP_1_5", #photoperiod
                            #"SWV_0_0", 
                            "SWV_0_3",#water volume in soil 
                            "FG_0_3", #"FG_0_1",
                            "EVI_0_6",#"EVI_0_4",
                            "NDVI_0_5", 
                            "ALT", 
                            "RR_0_3") #"TEMPERATUREDEBUT", "TEMPERATUREFIN", "TEMPMINI", "TEMPMAX"
                            
                          
                
                            
                            
df_model_numerical <- df_model %>%
  filter(!is.na(ALT), !is.na(EVI_0_6), !is.na(RR_0_3))

m <- cor(df_model_numerical[,numeric_abundance_vars], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > 0.7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]]) # p prints highly correlated pairs

highCorrVars <- findCorrelation(m, cutoff = 0.7, names = TRUE, exact= TRUE, verbose=F)
filtered_vars_abundance <- setdiff(colnames(m), highCorrVars) #this will print the selected vars according to pearsons 0.7
# Old output [1] "UM_0_0"   "EVI_1_5"  "TEMPMINI" "ALT"
#new output "TX_0_0"  "EVI_1_5" "SWV_0_0" "ALT"
#### Third select: to select final variables using the VIF 
variables_abond_corselect<-fuzzySim::corSelect(df_model_numerical, sp.cols="NBINDIV", var.cols=numeric_abundance_vars, coeff = TRUE,
                                               cor.thresh =  0.7,
                                               select =  "VIF", family = "truncated_nbinom2",
                                               use = "pairwise.complete.obs", method = "pearson", verbosity = 1)
variables_abond_corselect_fin<-variables_abond_corselect$selected.vars
# old output : "UM_0_0"   "PP_3_3"   "EVI_1_5"  "TEMPMINI" "TEMPMAX"  "ALT"


#filtered_numer_abundance_vars <- c("TX_0_0",  "EVI_1_5", "SWV_0_0", "ALT")
#filtered_numer_abundance_vars <- c("TX_0_0",  "EVI_1_5", "SWV_0_0") #if i want CLC and VENTDEBUT and PLUIEDEBUT
#reduced model selected vars
filtered_numer_abundance_vars <- c("TX_0_0","SWV_0_3","FG_0_1","NDVI_0_5")#removed ALT bcs corr with CLC
filtered_numer_abundance_vars <- c("TX_0_0",   "SWV_0_0",  "FG_0_1",   "NDVI_0_5", "ALT" )

#predictors_abundance <- c(filtered_numer_abundance_vars, filtered_categ_abundance_vars)


#25.04.2025 selection including RR to see if it can replace NDVI
##comment: apparently for predictive abundanc model i used the bad indicator for soil moisture! i used month when i shouldve used week.
#also i previously selected wind  FG_0_1 instead of FG_0_3 which is more correlated!!!! final seelction should be FG_0_3 and SWV_0_0
predictors_abundance <- c("TX_0_0",   "SWV_0_3",  "FG_0_1",   "NDVI_0_5", "ALT",      "RR_0_3")

predictors_abundance <- c("TX_0_0",   "SWV_0_3",  "FG_0_1",   "NDVI_0_5", "ALT",      "RR_0_3")
#### Final data frame for the multivariate analysis
df_model_abundance <- df_model %>%
  mutate(year = year(as.Date(DATE)))%>%
  filter(NBINDIV>0) %>%
  dplyr::select("idpointdecapture", "ID_SITE","DATE","ECO_CLI","Cell", "year", "NBINDIV", "PRES_CUL", predictors_abundance)%>%
  mutate(across(ID_SITE:year, as.factor))%>%
  filter(!is.na(RR_0_3), !is.na(FG_0_1))

#nrow before filtering =8847 ; after filtering n=7227

###########################
#########'Second stage of analysis: multivariate anaylsis using a LLO CV cross validation


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
saveRDS(res_multiv_model_abundance_LLO, "./LLO_abundance_RR.rds")

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
  dplyr::select(pred,obs,Cell,idpointdecapture, ID_SITE, ECO_CLI, DATE, year)

res_multiv_model_abundance_LTO <- list(model = mod_abundance_LTO, df_cv = df_cv_abundance_LTO, df_mod = df_model_abundance)  ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_abundance_LTO,"./LTO_abundance_RR.rds")



