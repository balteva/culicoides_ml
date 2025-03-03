########################### Opening packages

library(tidyverse)
library(glmmTMB)
library(purrr) 
library(furrr)
library(correlation) 
library(caret) 
library(performance) 
library(TMB)
library(Matrix)
library(broom.mixed)


########################### Open dataset containing the dependant and independent variables
dirty_data <- read.csv("../../data/ocapi_paysagere_micro_climat_obscot.csv") %>%
  select(-c(X, COMMUNELOC, DESCRIPTION_EXPLOIT, DESCRIPTION_EXT, DESCRIPTION_INT,
            DATEDETERMINATIONDEF, OBSERVATIONSDIVERSES, CHGTEMPLACEMENT))



#data <- read.csv("../../paysagere_df_model.csv")


###important,, add altitude as a var!
####cleaning the df to not contain NULL or "" values (need for later)
data <- dirty_data %>%
  mutate(across(everything(), as.character)) ###changing everything to char to remove NULL and "" entries


# sum(df_model == "NULL", na.rm = TRUE) n=9000
# sum(df_model == "", na.rm = TRUE) n= 629
# sum(is.na(df_model)) n= 2040

data_clean <- data %>%
  mutate(across(everything(), ~ na_if(.x, "NULL"))) %>% #making all NULL and "" values into NA
  mutate(across(everything(), ~ na_if(.x, "")))
######
df_model <- data_clean %>%
  select(-c( ECO_CLI, ELEV_CAPRIN, ELEV_MIXTE, longitude, latitude))%>%
  mutate(NBINDIV=as.numeric(NBINDIV), TEMPMINI=as.numeric(TEMPMINI), TEMPMAX= as.numeric(TEMPMAX), ID_SITE=as.factor(ID_SITE),
         SITUATIONPIEGE=as.factor(SITUATIONPIEGE), VENTDEBUT= as.factor(VENTDEBUT),
         VENTFIN = as.factor(VENTFIN), PLUIEDEBUT=as.factor(PLUIEDEBUT), PLUIEFIN= as.factor(PLUIEFIN), 
         NUAGEDEBUT=as.factor(NUAGEDEBUT), NUAGEFIN=as.factor(NUAGEFIN), OVI=as.numeric(OVI), EQU=as.numeric(EQU), BETAIL=as.numeric(BETAIL), CAP=as.numeric(CAP), BOV=as.numeric(BOV)) %>%
  mutate(
    PRES_CUL = ifelse(NBINDIV > 0, 1, 0),
    num_releve = row_number(),
    idpointdecapture = paste(ID_SITE, num_releve, sep = "_")) %>%
  filter(!is.na(NBINDIV)) %>%
  mutate(num_releve = as.factor(num_releve), DATE=as.factor(as.Date(DATE)))%>%
  relocate(NBINDIV, .after=ID_SITE)
  

  df_model <- df_model %>%
  # select(-c ( 'ACTIV_LAIT', 'ACTIV_VIANDE', 'ANIMAUXDEBUT', 'ANIMAUXFIN', 'TEMPERATUREFIN', 'TEMPERATUREDEBUT', 'num_releve', 'PLUIEFIN', 'PLUIEDEBUT', 'idpointdecapture')) %>%
  select(-c ( 'num_releve', 'idpointdecapture')) %>%
     mutate(ELEV_OVIN=as.factor(ELEV_OVIN), ELEV_BOVIN=as.factor(ELEV_BOVIN), OUVERTURE_BAT=as.factor(OUVERTURE_BAT), CLC=as.factor(CLC))




#creating vector "predictors. setdiff computes the set diff aka removes these columns specified
predictors <- setdiff(colnames(df_model), c( "NBINDIV", "ID_SITE", "PRES_CUL", 'DATE' )) ## Selecting all the columns which are not data on presence/abundance and metadata as predictors
------------------------------------------ #up to here all is good
#predictors1 <- c("SITUATIONPIEGE", "TEMPMAX")
##### To prepare the data frame for the GLMMM

df_glmm <- df_model %>% 
  dplyr::select(NBINDIV,PRES_CUL,ID_SITE,DATE,predictors) %>% ## Selecting only both response variables + 2 random effects (ID_PIEGE + num_session) + predictors 
  mutate(NBINDIV= as.character(NBINDIV), PRES_CUL = as.character(PRES_CUL)) %>%
  mutate(TEMPMAX= as.numeric(TEMPMAX), TEMPMINI=as.numeric(TEMPMINI),ALT=as.numeric(ALT), TEMPERATUREDEBUT=as.numeric(TEMPERATUREDEBUT), TEMPERATUREFIN=as.numeric(TEMPERATUREFIN),
         OVI=as.numeric(OVI), EQU=as.numeric(EQU), BETAIL=as.numeric(BETAIL), CAP=as.numeric(CAP), BOV=as.numeric(BOV),
         ANIMAUXDEBUT=as.factor(ANIMAUXDEBUT),ANIMAUXFIN=as.factor(ANIMAUXFIN), ACTIV_LAIT=as.factor(ACTIV_LAIT),ACTIV_VIANDE=as.factor(ACTIV_VIANDE), CLC=as.factor(CLC))%>%
  mutate_if(is.numeric, ~scale(., center = TRUE, scale = TRUE)) %>%  # we center and scale to be able to compare the magnitudes (centering also helps with allowing easier interpretation of variable coefficients associated with different magnitudes, e.g. when one regressor is measured on a very small order, while another on a very large order.  )
  mutate(NBINDIV = as.numeric(NBINDIV), PRES_CUL= as.numeric(PRES_CUL))

######################


# df_glmm1 <- df_glmm%>%
#   filter(NBINDIV >1)
# model  <- glmmTMB(NBINDIV ~ TEMPMAX + (1|ID_SITE) + (1|num_releve), data = df_glmm1, family = truncated_nbinom2) ## Realization of GLMM with binomial distribution for each predictor and with 2 random effects
# 
# write.csv(df_glmm1, "../../df_glmm1.csv")
# 
# # ggplot(df_model, aes(x = TEMPMINI, y = PRES_CUL)) + geom_jitter() + geom_smooth(method = "glm", method.args = list(family = "binomial"))





##### A function which realizes all GLMMs for presence and all GLMMs for abundance
fun_compute_glmm_univ <- function(df_glmm, indicator) {
  
  ## Define GLMM model functions
  if (indicator == "presence") { 
    func <- function(x) {
      ret <- glmmTMB(as.formula(paste0("PRES_CUL ~ ", x, " + (1|ID_SITE) + (1|DATE)")), 
                     data = df_glmm, family = binomial(link = "logit"))
      return(ret)
    }
  } else if (indicator == "abundance") { 
    func <- function(x) {
      ret <- glmmTMB(as.formula(paste0("NBINDIV ~ ", x, " + (1|ID_SITE) + (1|DATE)")), 
                     data = df_glmm, family = truncated_nbinom2)
      return(ret)
    }
  }
  
  ##Apply models safely
  possible_a <- possibly(func, otherwise = NULL)  # Use NULL instead of NA
  glmms_univs <- future_map(colnames(df_glmm[5:ncol(df_glmm)]), possible_a)
  
  ##Remove failed models
  glmms_univs <- compact(glmms_univs)  # Removes NULLs directly
  
  ## Remove models with missing AIC
  glmms_univs <- keep(glmms_univs, ~ !is.na(summary(.)$AICtab[1]))
  
  ##Convert models to tidy format
  func2 <- function(x, indicator) {
    ret <- broom.mixed::tidy(x, conf.int = TRUE, exponentiate = ifelse(indicator == "abundance", FALSE, TRUE))
    ret$r2 <- performance::r2_nakagawa(x)$R2_marginal
    return(ret)
  }
  
  possible_b <- possibly(function(x) func2(x, indicator), otherwise = NULL)
  glmms_univs <- future_map(glmms_univs, possible_b)
  glmms_univs <- compact(glmms_univs)  # Remove NULLs
  glmms_univs <- bind_rows(glmms_univs) %>% 
    filter(effect == "fixed" & term != "(Intercept)")
  
  ## Return final data frame
  return(glmms_univs)
}

# df_glmm_0 <- df_glmm %>%
#   filter(!NBINDIV==0)
# glmmTMB(NBINDIV ~ ELEV_OVIN + (1|ID_SITE) + (1|num_releve), data = df_glmm_0, family = truncated_nbinom2)

#the issue is that i need to average out catches per month per site maybe... 


##### For presence models: code make take long to run (~10 min)
glmm_univ_presence <- fun_compute_glmm_univ(df_glmm, "presence")
write.csv(glmm_univ_presence,'../../../glmm_univ_presence_obscot.csv', row.names = F)

summary(glmm_univ_presence[[1]])


##### For abundance models: code make take long to run (~10 min)
df_glmm <- df_glmm %>% filter(NBINDIV>0) ## Selecting only the strictly positive number of Aedes albopictus caught in a trap
glmm_univ_abundance <- fun_compute_glmm_univ(df_glmm, "abundance")
write.csv(glmm_univ_abundance,'../../../glmm_univ_abundance_obscot.csv', row.names = F)




