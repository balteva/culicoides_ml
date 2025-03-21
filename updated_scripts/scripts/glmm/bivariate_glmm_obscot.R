
library(tidyverse)
library(glmmTMB)
library(purrr) 
library(furrr)
library(correlation) 
library(caret) 
library(performance) 
library(TMB)
library(Matrix)


########################### Open dataset containing the dependant and independent variables

raw_data <- read.csv("../../updated_scripts/data/obscot_micro_landscape_df.csv") %>%
  select(-c(DATEDETERMINATIONDEF, OBSERVATIONSDIVERSES, CHGTEMPLACEMENT, "na.rm", "ESPECE"))%>%
  mutate(across(everything(), as.character)) ##changing everything to char to remove NULL and "" entries


# sum(raw_data == "NULL", na.rm = TRUE) n=24443
# sum(raw_data == "", na.rm = TRUE) n=1265
# sum(is.na(raw_data)) n= 0

data <- raw_data %>%
  mutate(across(everything(), ~ na_if(.x, "NULL"))) %>% #making all NULL and "" values into NA
  mutate(across(everything(), ~ na_if(.x, "")))

######
df_model <- data %>%
  mutate(across(ID_SITE:ECO_CLI, as.factor),
         CLC=as.factor(CLC),
         NBINDIV=as.numeric(NBINDIV),
         across(ALT:SURF_CANT, as.numeric),
         across(ELEV_OVIN:OUVERTURE_BAT, as.factor),
         across(ANIMAUXDEBUT:ANIMAUXFIN, as.factor),
         across(TEMPMINI:latitude, as.numeric),
         across(TEMPERATUREFIN:TEMPERATUREDEBUT, as.numeric)) %>%
  mutate(
    PRES_CUL = ifelse(NBINDIV > 0, 1, 0),
    num_releve = row_number(),
    idpointdecapture = paste(ID_SITE, num_releve, sep = "_")) %>%
  mutate(num_releve = as.factor(num_releve), DATE=as.factor(as.Date(DATE)))%>%
  relocate(NBINDIV,PRES_CUL, .after=ID_SITE)%>%
  relocate(num_releve, idpointdecapture, .before= ID_SITE)


##################removing errors in dataframe
upper_bound <- 50
lower_bound <- -30


#for these variables values below lower bound and above upper bound are replaced with NA
df_model$TEMPERATUREDEBUT[df_model$TEMPERATUREDEBUT < lower_bound | df_model$TEMPERATUREDEBUT > upper_bound] <- NA
df_model$TEMPERATUREFIN[df_model$TEMPERATUREFIN < lower_bound | df_model$TEMPERATUREFIN > upper_bound] <- NA
df_model$TEMPMINI[df_model$TEMPMINI < lower_bound | df_model$TEMPMINI > upper_bound] <- NA
df_model$TEMPMAX[df_model$TEMPMAX < lower_bound | df_model$TEMPMAX > upper_bound] <- NA




df_model <- df_model %>%
  mutate(CLC_level2 = case_when(
    CLC %in% ("Non-irrigated arable land") ~"Arable land",
    CLC %in% c("Land principally occupied by agriculture, with significant areas of natural vegetation", "Complex cultivation patterns") ~"Heterogeneous agricultural areas",
    CLC %in%  ("Pastures") ~ "Pastures",
    CLC %in%  ("Fruit trees and berry plantations") ~"Permanent crops",
    CLC %in% c("Broad-leaved forest","Mixed forest") ~"Forests",
    CLC %in% c("Transitional woodland-shrub","Natural grasslands") ~ "Scrub and/or herbaceous associations",
    CLC %in% ("Discontinuous urban fabric") ~"Urban fabric",
    CLC %in% ("Mineral extraction sites") ~"Mine, dump and construction sites",
    CLC %in%  c("Industrial or commercial units","Airports") ~ "Industrial, commercial and transport units",
    CLC %in%  c("Sport and leisure facilities","Green urban areas") ~ " Artificial, non-agricultural vegetated areas")) %>%
  mutate(CLC_level2 = as.factor(CLC_level2)) %>%
  select(-c(CLC, ELEV_MIXTE))





#creating vector "predictors. setdiff computes the set diff aka removes these columns specified
predictors <- setdiff(colnames(df_model), c( "NBINDIV", "ID_SITE", "PRES_CUL", 'DATE', "ECO_CLI", "num_releve","idpointdecapture", "longitude", "latitude")) ## Selecting all the columns which are not data on presence/abundance and metadata as predictors

  
  df_glmm <- df_model %>% 
  dplyr::select(NBINDIV,PRES_CUL,ID_SITE,DATE,predictors) %>% ## Selecting only both response variables + 2 random effects (ID_SITE and DATE) + predictors  
  mutate(NBINDIV= as.character(NBINDIV), PRES_CUL = as.character(PRES_CUL)) %>%
  mutate_if(is.numeric, ~scale(., center = TRUE, scale = TRUE)) %>%  # we center and scale to be able to compare the magnitudes (centering also helps with allowing easier interpretation of variable coefficients associated with different magnitudes, e.g. when one regressor is measured on a very small order, while another on a very large order.  )
  mutate(NBINDIV = as.numeric(NBINDIV), PRES_CUL= as.numeric(PRES_CUL)) 

#glmm tmb handles automatically NA values

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



##### For presence models: code make take long to run (~10 min)
glmm_univ_presence <- fun_compute_glmm_univ(df_glmm, "presence")
write.csv(glmm_univ_presence,'../../updated_scripts/data/glmm/glmm_presence_obscot_clc2.csv', row.names = F)

summary(glmm_univ_presence[[1]])


##### For abundance models: code make take long to run (~10 min)
df_glmm <- df_glmm %>% filter(NBINDIV>0) ## Selecting only the strictly positive number 
glmm_univ_abundance <- fun_compute_glmm_univ(df_glmm, "abundance")
write.csv(glmm_univ_abundance,'../../updated_scripts/data/glmm/glmm_abundance_obscot_clc2.csv', row.names = F)


write.csv(df_model, '../../updated_scripts/data/df_mandscape_for_multivariate.csv', row.names = F)