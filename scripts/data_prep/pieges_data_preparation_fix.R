library(tidyverse)
library(fst)

load("../../database_ocapi_09_12_survFR.rda")
database_ocapi <- database_ocapi %>%
  mutate(DATEFIN = case_when(
    DATEFIN == "0000-00-00" ~ "2012-08-17",TRUE ~ DATEFIN))


ecocli <-  read_delim(file.path("../../data","meteo_df.csv")) %>%
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


write.csv(database_ocapi_prepared,  file.path("../../","culicoides_df_fixed.csv"))
 #keeping the rest of the results unchanged
####extracting same df with absolutely every paysagere and microclimate var



database_ocapi_paysagere_climat <- database_ocapi %>%
  dplyr::select(ID_SITE, COMMUNELOC, DATEFIN, NBINDIV,ESPECE, DESCRIPTION_EXPLOIT, ELEV_OVIN:ACTIV_VIANDE, DESCRIPTION_EXT, DESCRIPTION_INT, OUVERTURE_BAT,TEMPERATUREDEBUT, TEMPERATUREFIN:TEMPMAX, longitude, latitude ) %>%
  group_by(across(!c(NBINDIV, ESPECE))) %>%
  summarise(NBINDIV = sum(NBINDIV)) %>%
  ungroup() %>%
  left_join(ecocli) %>%
  relocate(ECO_CLI, .after = ID_SITE) %>%
  rename(DATE = DATEFIN) %>%
  filter(!is.na(DATE))

colnames(database_ocapi_paysagere_climat)
write.csv(database_ocapi_paysagere_climat,  file.path("../../","ocapi_paysagere_micro_climat.csv"))
