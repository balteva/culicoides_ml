#########         Presence model preparation.
#stage 1: var selection.

#########################categorical vars only

categ_presence_vars <- c("SITUATIONPIEGE", "ACTIV_VIANDE", "OUVERTURE_BAT", "PLUIEDEBUT", "VENTDEBUT", "VENTFIN", "CLC") #selecting only categorical vars for presence for chi square test

categ_presence_df <- df_model %>%
  select(categ_presence_vars)%>% #(ID_SITE, DATE, PRES_CUL, 
  mutate(across(SITUATIONPIEGE:CLC, as.factor))


######################## function for calculating correlation  btw categorical vars (need to look at output manually).
# high p values (p>0.05) means that the vars are independent.
#therefore im choosing those vars that had a high p value as my categorical vars


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

#fishers_func_categorical(categ_presence_df, categ_presence_vars, "C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/var_selection/categ_presence_var_fishers.txt")
#####           after running this func, need to look at the output file to select the non confounding vars

#put in the selected categorical vars in a new vector called 'filtered' according to fishers

#filtered_categ_presence_vars_fisher <- c("SITUATIONPIEGE", "ACTIV_VIANDE", "VENTFIN")


#tests if the relationship strength btw 2 categorical vars. 
# range 0 - 0.1 = very weak association
# range 0.1 - 0.2 = weak association
# range 0.2 - 0.3 moderate
# range > 0.3 strong asssocation


#install.packages("sjstats")
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
  filter(CramersV <= 0.2)

filtered_categ_presence_vars_cramersv <- c("OUVERTURE_BAT", "ACTIV_VIANDE", "VENTFIN", "PLUIEDEBUT")# only issue is maybe ouverture_bat and situation_piege bcs theyre correlated
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
