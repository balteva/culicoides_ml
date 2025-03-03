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
  group_by(ID_SITE, COMMUNELOC, DATEFIN,ESPECE) %>%
  summarise(NBINDIV = sum(NBINDIV)) %>%
  ungroup() %>%
  left_join(ecocli) %>%
  relocate(ECO_CLI, .after = ID_SITE) %>%
  rename(DATE = DATEFIN) %>%
  filter(!is.na(DATE), !(NBINDIV==0))

-----------------
data <- database_ocapi_prepared %>%
  mutate(year=year(DATE), ESPECE = case_when(
    ESPECE == "obsoletus s.st" ~ "obsoletus/scoticus",TRUE ~ ESPECE))

espece <-c("obsoletus/scoticus", "dewulfi", "newsteadi" , "chiopterus", "imicola")

######
  species_renamer<- function(df) {
    df<- df %>% 
      mutate(ESPECE=ifelse(ESPECE %in% espece, ESPECE, "other"))
    return(df)
  }
######

renamed_sp <- species_renamer(data)


data2009 <- renamed_sp %>%
  filter(year==2009)%>%
  group_by(ESPECE, ECO_CLI, DATE)%>%
  summarise(NBINDIV=sum(NBINDIV))

data2010 <- renamed_sp %>%
  filter(year==2010)%>%
  group_by(ESPECE, ECO_CLI, DATE)%>%
  summarise(NBINDIV=sum(NBINDIV))

data2011 <- renamed_sp %>%
  filter(year==2011)%>%
  group_by(ESPECE, ECO_CLI, DATE)%>%
  summarise(NBINDIV=sum(NBINDIV))

data2012 <- renamed_sp %>%
  filter(year==2012)%>%
  group_by(ESPECE, ECO_CLI, DATE)%>%
  summarise(NBINDIV=sum(NBINDIV))





----------------------------------------------
  
# data2009_sp <- data2009 %>%
#   group_by(ESPECE, ECO_CLI) %>%
#   summarise(NBINDIV=sum(NBINDIV))%>%
#   arrange(desc(NBINDIV))
# 
# data2009_sp <- data2009 %>%
#   group_by(ESPECE) %>%
#   summarise(NBINDIV=sum(NBINDIV))%>%
#   arrange(desc(NBINDIV))



data2012_t <- data2012 %>%
  group_by(ESPECE, ECO_CLI)%>%
  summarise(NBINDIV =mean(NBINDIV)) #avg number of species across all sites



# data2009_t<- data2009_t %>%
#   arrange(NBINDIV) %>%
#   mutate(ESPECE = factor(ESPECE, levels = ESPECE),  
#          Percentage = round(NBINDIV / sum(NBINDIV) * 100, 1),  
#          Label = paste0(ESPECE, "\n", Percentage, "%"))  # Species + percentage in label

# Bar chart with species names inside
# ggplot(data2010_t, aes(x = NBINDIV, y = ESPECE, fill = ESPECE)) +
#   geom_col(show.legend = FALSE) +
#   geom_text(aes(label = Label), color = "black", size = 5, fontface = "bold", hjust = 0.5, vjust = 0.5) +
#   scale_fill_brewer(palette = "Blues") +  # Use a simple color palette
#   labs(title = "Culicoides sp. collected in 2010", x = "Mean NBINDIV across all the different sites", y="Species") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))

ggplot(data2012_t, aes(x = NBINDIV, y = ESPECE, fill = ESPECE)) +
  geom_col(show.legend = FALSE) +
  #geom_text(aes(label = Label), color = "black", size = 5, fontface = "bold", hjust = 0.5, vjust = 0.5) +
  scale_fill_brewer(palette = "Blues") +  # Use a simple color palette
  labs(title = "Culicoides sp. collected in 2012",
       x = "Mean NBINDIV collected per trap",
       y = "Species") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(size=5, face="bold"))+
  facet_wrap(~ ECO_CLI, scales="free")


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
