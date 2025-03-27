

# https://stats.stackexchange.com/questions/484299/how-to-check-the-correlation-between-categorical-and-numeric-independent-variabl

library(ltm) #for biserial cor which is similar to pearsons for categorical x numerical vars

test <- df_model %>% #model with all vars for presence
  dplyr::select(predictors_presence) %>%
  mutate(across(SITUATIONPIEGE:ACTIV_VIANDE, as.factor),
         VENTDEBUT = 
           fct_relevel(VENTDEBUT, "Vent nul", "Légère brise à vent faible", "Vent moyen",  "Vent fort à grand vent"))%>% 
  filter(!is.na(SITUATIONPIEGE), !is.na(VENTDEBUT), !is.na(ACTIV_VIANDE),!is.na(EVI_0_3))

Numer_vars <- c("TX_0_4","SWV_0_4", "EVI_0_3", "ALT", "OVI", "BETAIL")
categ_vars <- c("VENTDEBUT", "ACTIV_VIANDE", "SITUATIONPIEGE")


test_df <- data.frame(x=as.factor(test$VENTDEBUT), y=as.numeric(test$EVI_0_3))




##for presence
#kruskal test for categorical vars with more than 2 levels such as VENTDEBUT
kruskal.test(x ~ y, data=test_df)
ggplot(test_df) + geom_boxplot(aes(x, y))   

          #       VENTDEBUT VS
#TX_0_4 OK  #SWV_0_4 OK #EVI_0_3 OK
#ALT NO #OVI NO #BETAIL NO
test_df  <- data.frame(x=as.numeric(test$BETAIL), y=as.factor(test$SITUATIONPIEGE))

# biserial cor will be used only for dichotomous categorical vars such as ACTIV_VIANDE, SITUATIONPIEGE
biserial.cor(test_df$x, test_df$y)
          #       ACTIV_VIANDE VS
#BETAIL OK
#OVI OK
#ALT OK
#EVI_0_3 OK
#SWV_0_4 OK
#TX_0_4 OK

        #        SITUATIONPIEGE VS
#BETAIL OK
#OVI OK
#ALT OK
#EVI_0_3 OK
#SWV_0_4 OK
#TX_0_4 OK

###################################for abundance:
test <- df_model %>% #model with all vars for presence
  dplyr::select(predictors_abundance) %>%
  mutate(VENTDEBUT = 
           fct_relevel(VENTDEBUT, "Vent nul", "Légère brise à vent faible", "Vent moyen",  "Vent fort à grand vent"),
         PLUIEDEBUT =
           fct_relevel(PLUIEDEBUT, "Aucune", "Brouillard / Bruine","Pluie fine","Pluie forte")) %>%
  filter(!is.na(PLUIEDEBUT), !is.na(VENTDEBUT), !is.na(ELEV_OVIN), !is.na(CLC_level2), !is.na(EVI_1_5))

categ_vars <- c("ELEV_OVIN", "CLC_level2", "PLUIEDEBUT", "VENTDEBUT")
numer_vars <- c("TX_0_0",  "EVI_1_5", "SWV_0_0", "ALT")

test_df <- data.frame(x=as.factor(test$PLUIEDEBUT), y=as.numeric(test$TX_0_0))


#kruskal test for categorical vars with more than 2 levels such as CLC_level2, VENTDEBUT, PLUIEDEBUT
kruskal.test(x ~ y, data=test_df)
ggplot(test_df) + geom_boxplot(aes(x, y))   
#### CLC_level2 VS
#TX_0_0 OK
#EVI_1_5 OK
#SWV_0_0 OK
#ALT NO

#### VENTDEBUT VS
#TX_0_0 OK
#EVI_1_5 OK
#SWV_0_0 OK
#ALT NO


#### PLUIEDEBUT VS
#TX_0_0 OK
#EVI_1_5 OK
#SWV_0_0 OK
#ALT NO

test_df  <- data.frame(x=as.numeric(test$TX_0_0), y=as.factor(test$ELEV_OVIN))

# biserial cor will be used only for dichotomous categorical vars such as ELEV_OVIN
biserial.cor(test_df$x, test_df$y)

#ELEV_OVIN vs
#TX_0_0 OK
#EVI_1_5 OK
#SWV_0_0 OK
#ALT OK


# so can either keep ALT and get rid of categorical vars,
#or get rid of ALT and keep PLUIEDEBUT, VENTDEBUT, CLC_level2