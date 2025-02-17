library(tidyverse)
library(fst)

data<- load("../database_ocapi_09_12_survFR.rda")

ecocli <-  read_delim(file.path("../2025_ieva_culicoides/data","meteo_df.csv")) %>%
  dplyr::select(ID_SITE,ECO_CLI) %>%
  distinct() #from the meteo df im just extracting the ecoclim zone because ocapi doesnt have it

database_ocapi_prepared <- database_ocapi %>%
  dplyr::select(ID_SITE, COMMUNELOC, DATEFIN, NBINDIV, ESPECE) %>%
  group_by(ID_SITE, COMMUNELOC, DATEFIN) %>%
  summarise(NBINDIV = sum(NBINDIV)) %>%
  ungroup() %>%
  left_join(ecocli) %>%
  relocate(ECO_CLI, .after = ID_SITE) %>%
  rename(DATE = DATEFIN) %>%
  filter(!is.na(DATE)) #not necessary becasue no NA values


####extracting same df with absolutely every paysagere and microclimate var



database_ocapi_paysagere_climat <- database_ocapi %>%
  dplyr::select(ID_SITE, COMMUNELOC, DATEFIN, NBINDIV, ESPECE, longitude, latitude, DESCRIPTION_EXPLOIT:ACTIV_VIANDE, DESCRIPTION_EXT, DESCRIPTION_INT, OUVERTURE_BAT,TEMPERATUREDEBUT, TEMPERATUREFIN:TEMPMAX, SOUSECH:PRETRI ) %>%
  group_by(across(!c(NBINDIV, ESPECE))) %>%
  summarise(NBINDIV = sum(NBINDIV)) %>%
  ungroup() %>%
  left_join(ecocli) %>%
  relocate(ECO_CLI, .after = ID_SITE) %>%
  rename(DATE = DATEFIN) %>%
  filter(!is.na(DATE))

colnames(database_ocapi_paysagere_climat)
write.csv(database_ocapi_paysagere_climat,  file.path("../../","ocapi_paysagere_climat.csv"))
