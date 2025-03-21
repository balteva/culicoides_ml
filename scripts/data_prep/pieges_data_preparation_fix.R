library(tidyverse)
library(fst)

load("../../data/database_ocapi_09_12_survFR.rda")
database_ocapi <- database_ocapi %>%
  mutate(DATEFIN = case_when(
    DATEFIN == "0000-00-00" ~ "2012-08-17",TRUE ~ DATEFIN),
    ESPECE = case_when(
      ESPECE == "scoticus s.st" ~ "obsoletus/scoticus", TRUE ~ ESPECE),
    ESPECE = case_when(
      ESPECE == "obsoletus s.st" ~ "obsoletus/scoticus", TRUE ~ ESPECE))


ecocli <-  read_delim(file.path("../../data","meteo_df.csv")) %>%
  dplyr::select(ID_SITE,ECO_CLI, SURF_CANT) %>%#adding altitude and corine land cover data to my last df for glmm
  distinct() #from the meteo df im just extracting the ecoclim zone because ocapi doesnt have it

database_ocapi_prepared <- database_ocapi %>%
  dplyr::select(ID_SITE, COMMUNELOC, DATEFIN, NBINDIV, ESPECE) %>%
  filter(ESPECE=="obsoletus/scoticus") %>%
  group_by(ID_SITE, COMMUNELOC, DATEFIN) %>%
  summarise(NBINDIV = sum(NBINDIV)) %>%
  ungroup() %>%
  left_join(ecocli) %>%
  relocate(ECO_CLI, .after = ID_SITE) %>%
  rename(DATE = DATEFIN) %>%
  filter(!is.na(DATE)) #not necessary becasue no NA values

# 
# 
# traps2009 <- database_ocapi_prepared %>%
#   mutate(year=year(as.Date(DATE)))%>%
#   filter(year==2009) %>%
#   select(ID_SITE)%>%
#   unique() %>% count()
# 
# traps2010 <- database_ocapi_prepared %>%
#   mutate(year=year(as.Date(DATE)))%>%
#   filter(year==2010) %>%
#   select(ID_SITE)%>%
#   unique() %>% count()
# 
# traps2011 <- database_ocapi_prepared %>%
#   mutate(year=year(as.Date(DATE)))%>%
#   filter(year==2011) %>%
#   select(ID_SITE)%>%
#   unique() %>% count()
# 
# traps2012 <- database_ocapi_prepared %>%
#   mutate(year=year(as.Date(DATE)))%>%
#   filter(year==2012) %>%
#   select(ID_SITE)%>%
#   unique() %>% count()



write.csv(database_ocapi_prepared,  file.path("../../","culicoides_df_fixed_obscot.csv"))
 #keeping the rest of the results unchanged
####extracting same df with absolutely every paysagere and microclimate var
ecocli <-  read_delim(file.path("../../data","meteo_df.csv")) %>%
  dplyr::select(ID_SITE,ECO_CLI, CLC,ALT, OVI, EQU, BETAIL, CAP, BOV, SURF_CANT) %>%#adding altitude and corine land cover data to my last df for glmm
  distinct() #from the meteo df im just extracting the ecoclim zone because ocapi doesnt have it



database_ocapi_paysagere_climat <- database_ocapi %>%
  dplyr::select(ID_SITE, COMMUNELOC, DATEFIN, NBINDIV,ESPECE, DESCRIPTION_EXPLOIT, ELEV_OVIN:ACTIV_VIANDE, DESCRIPTION_EXT, DESCRIPTION_INT, OUVERTURE_BAT,TEMPERATUREDEBUT, TEMPERATUREFIN:TEMPMAX, longitude, latitude) %>%
  filter(ESPECE == "obsoletus/scoticus")%>%
  group_by(across(!c(NBINDIV, ESPECE))) %>%
  summarise(NBINDIV = sum(NBINDIV)) %>%
  ungroup() %>%
  left_join(ecocli) %>%
  relocate(ECO_CLI, .after = ID_SITE) %>%
  rename(DATE = DATEFIN) %>%
  filter(!is.na(DATE))

colnames(database_ocapi_paysagere_climat)
write.csv(database_ocapi_paysagere_climat,  file.path("../../data/obscot","ocapi_paysagere_micro_climat_obscot_tempfin.csv"))
