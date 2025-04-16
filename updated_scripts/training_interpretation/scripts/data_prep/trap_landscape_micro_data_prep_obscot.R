library(tidyverse)
library(fst)

load("../../data/database_ocapi_09_12_survFR.rda")

test <- database_ocapi %>%
  filter(ID_SITE=="2BPL2" & DATEFIN=="2012-01-24" & ESPECE == "obsoletus/scoticus")

database_ocapi <- database_ocapi %>%
  mutate(DATEFIN = case_when(
    DATEFIN == "0000-00-00" ~ "2012-08-17",TRUE ~ DATEFIN))%>%
mutate(across(everything(), ~ if_else(ID_CAMPAGNE == 14347 & ID_SITE == "2BPL2" & DATEFIN == "2012-01-24", NA, .)))
#   filter(!(ID_CAMPAGNE == 14347 & ID_SITE == "2BPL2" & DATEFIN == "2012-01-24")) #this entry appears twice for same site and date with different counts, maybe they messed up the species?



# This is for CCMs
ecocli <-  read_delim(file.path("../../data","meteo_df.csv")) %>%
  dplyr::select(ID_SITE,ECO_CLI) %>%#adding altitude and corine land cover data to my last df for glmm
  distinct() #from the meteo df im just extracting the ecoclim zone because ocapi doesnt have it

 database_ocapi_prepared <- database_ocapi %>%
   dplyr::select(ID_SITE, DATEFIN, NBINDIV, ESPECE) %>%
   filter(ESPECE=="obsoletus/scoticus") %>%
   group_by(ID_SITE, DATEFIN) %>%
   summarise(NBINDIV = sum(NBINDIV)) %>%
   ungroup() %>%
   left_join(ecocli) %>%
   relocate(ECO_CLI, .after = ID_SITE) %>%
   rename(DATE = DATEFIN) %>%
   filter(!is.na(DATE), !is.na(NBINDIV)) 


write.csv(database_ocapi_prepared,  file.path("../../updated_scripts/data","obscot_nbindiv_id_date_simple_df.csv"), row.names=F)

 # This is for GLMMs 

####extracting same df with absolutely every paysagere and microclimate var
satelite_data <-  read_delim(file.path("../../data","meteo_df.csv")) %>%
  dplyr::select(ID_SITE, ECO_CLI, DATE, CLC,ALT, OVI, EQU, BETAIL, CAP, BOV, SURF_CANT)%>%
  group_by(ID_SITE, ECO_CLI, DATE, CLC, ALT, OVI, EQU, BETAIL, CAP, BOV, SURF_CANT)%>%
  summarise(across(everything(), first), .groups = "drop")%>%
  mutate(DATE=as.Date(DATE))



local_data<- database_ocapi %>%
  dplyr::select(ID_SITE, DATEFIN, NBINDIV,ESPECE, ELEV_OVIN:ACTIV_VIANDE, OUVERTURE_BAT,TEMPERATUREDEBUT, TEMPERATUREFIN:TEMPMAX, longitude, latitude) %>%
  filter(ESPECE == "obsoletus/scoticus")%>%
  group_by(across(!(NBINDIV))) %>%
  summarise(NBINDIV = sum(NBINDIV), na.rm=T) %>%
  ungroup() %>%
  mutate(DATE=as.Date(DATEFIN))%>%
  select(-DATEFIN)


# duplicates <- local_data %>%   # checking for errors
#   group_by(ID_SITE, DATE) %>%
#   filter(n() > 1)%>%
#   as.data.frame()

obscot_micro_landscape <- local_data %>%
  inner_join(satelite_data, by = c("ID_SITE", "DATE")) %>%
  relocate(DATE,ECO_CLI,NBINDIV, .after = ID_SITE)

# duplicates <- obscot_micro_landscape %>%   # checking for errors
#   group_by(ID_SITE, DATE) %>%
#   filter(n() > 1)%>%
#   as.data.frame()


write.csv(obscot_micro_landscape,  file.path("../../updated_scripts/data","obscot_micro_landscape_df.csv"), row.names=F)
