library(tidyverse)
library(fst)
library(lubridate)


load("../database_ocapi_09_12_survFR.rda")

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

############################### extracting data separated by collection year
ocapi_2009 <- database_ocapi_prepared %>%
  mutate(DATE=as.Date(DATE),
         year=year(DATE)) %>%
  filter(year==2009) #%>%
 # mutate(week=lubridate::week(ymd(DATE)))


ocapi_2010 <- database_ocapi_prepared %>%
  mutate(DATE=as.Date(DATE),
         year=year(DATE)) %>%
  filter(year==2010) #%>%
  #mutate(week=lubridate::week(ymd(DATE)))

ocapi_2011 <- database_ocapi_prepared %>%
  mutate(DATE=as.Date(DATE),
         year=year(DATE)) %>%
  filter(year==2011)# %>%
 #mutate(week=lubridate::week(ymd(DATE)))

ocapi_2012 <- database_ocapi_prepared %>%
  mutate(DATE=as.Date(DATE),
         year=year(DATE)) %>%
  filter(year==2012) #%>%
  #mutate(week=lubridate::week(ymd(DATE)))

#############################################"

#these are for separating by eco_clim zone

df_pieges_2009 <- ocapi_2009 %>%
group_by(ECO_CLI, ID_SITE, DATE) %>% #
  summarise(effectif_jour_mean=mean(NBINDIV, na.rm = T)) %>%
  ungroup() %>%
  mutate(ECO_CLI = as.factor(ECO_CLI)) %>%
  mutate(ECO_CLI= forcats::fct_relevel(ECO_CLI, "Continental", "Mediterranean", "Atlantic" ,"Alpine" )) #%>%
 # mutate(date = as.Date(paste(year, month, "01", sep="-")))
df_pieges_2010 <- ocapi_2010 %>%
  group_by(ECO_CLI,ID_SITE, DATE) %>%
  summarise(effectif_jour_mean=mean(NBINDIV, na.rm = T)) %>%
  ungroup() %>%
  mutate(ECO_CLI = as.factor(ECO_CLI)) %>%
  mutate(ECO_CLI= forcats::fct_relevel(ECO_CLI, "Continental", "Mediterranean", "Atlantic" ,"Alpine" )) #%>%
#
df_pieges_2011 <- ocapi_2011 %>%
  group_by(ECO_CLI,ID_SITE, DATE) %>%
  summarise(effectif_jour_mean=mean(NBINDIV, na.rm = T)) %>%
  ungroup() %>%
  mutate(ECO_CLI = as.factor(ECO_CLI)) %>%
  mutate(ECO_CLI= forcats::fct_relevel(ECO_CLI, "Continental", "Mediterranean", "Atlantic" ,"Alpine" )) #%>%

df_pieges_2012 <- ocapi_2012 %>%
  group_by(ECO_CLI,ID_SITE, DATE) %>%
  summarise(effectif_jour_mean=mean(NBINDIV, na.rm = T)) %>%
  ungroup() %>%
  mutate(ECO_CLI = as.factor(ECO_CLI)) %>%
  mutate(ECO_CLI= forcats::fct_relevel(ECO_CLI, "Continental", "Mediterranean", "Atlantic" ,"Alpine" )) #%>%




# ggplot(df_pieges_2009, aes(x = DATE)) +
#   geom_line(aes(y = effectif_jour_mean)) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() + # theme_classic()
#   theme(panel.grid = element_blank())


####################   function to extract dates when peak occurs
get_peak_dates <- function(df, indicator){
  if(indicator == "france"){
  df %>%
    # group_by(DATE) %>% #should probably work not on absolute values per each day
    # mutate(effectif_jour_mean=mean(effective_jour_mean), .groups="drop"
    filter(effectif_jour_mean == max(effectif_jour_mean)) %>%
    summarise(peak_date = min(DATE)) #in case theres same max counts across several days
    } 
  
  else if(indicator == "eco_cli"){
  df %>%
    group_by(ECO_CLI) %>%
    filter(effectif_jour_mean == max(effectif_jour_mean)) %>%
    summarise(peak_date = min(DATE))}
}

#peak dates for each year either for entire france or for each eco_cli
peak_dates_2009_france <- get_peak_dates(df_pieges_2009, "france")
peak_dates_2009_ecocli <- get_peak_dates(df_pieges_2009, "eco_cli")

peak_dates_2010_france <- get_peak_dates(df_pieges_2010,"france")
peak_dates_2010_ecocli <- get_peak_dates(df_pieges_2010, "eco_cli")

peak_dates_2011_france <- get_peak_dates(df_pieges_2011,"france")
peak_dates_2011_ecocli <- get_peak_dates(df_pieges_2011, "eco_cli")

peak_dates_2012_france <- get_peak_dates(df_pieges_2012,"france")
peak_dates_2012_ecocli <- get_peak_dates(df_pieges_2012, "eco_cli")


###########################"
##splitting the data into two parts by peak dates
split_by_peak <- function(df, peak_dates, indicator, peak) {
 
  if(indicator=="france"){
    df <- df %>% 
      mutate(peak_date = peak_dates$peak_date)
  } else if(indicator == "eco_cli"){
    df <- df %>%
      left_join(peak_dates, by= "ECO_CLI")}
  
    before_peak <- df %>% filter(DATE < peak_date)
    after_peak <- df %>% filter(DATE >= peak_date)
    
  if(peak=="before"){  
    return(before_peak)}
    
  else if(peak=="after"){
    return(after_peak)}  
} 

#################"
france2009before <-split_by_peak(df_pieges_2009, peak_dates_2009_france, "france", "before")
france2010before <-split_by_peak(df_pieges_2010, peak_dates_2010_france, "france", "before")
france2011before <-split_by_peak(df_pieges_2011, peak_dates_2011_france, "france", "before")
france2012before <-split_by_peak(df_pieges_2012, peak_dates_2012_france, "france", "before")

france_before <-rbind(france2009before,france2010before, france2011before, france2012before)

write.csv(france_before, "../../france_before_peak.csv")

ggplot(france_before, aes(x = month(DATE))) +
  geom_line(aes(y = effectif_jour_mean)) +
  facet_grid(~year(DATE), scale="free")+
  theme_bw() + # theme_classic()
  theme(panel.grid = element_blank())
















before_peak_france <- rbind(before_peak_france2009, before_peak_france2010, before_peak_france2011, before_peak_france2012)

after_peak_france  <- rbind(after_peak_france2009, after_peak_france2010, after_peak_france2011, after_peak_france2012)



ggplot(before_peak_france2010, aes(x = DATE)) +
  geom_line(aes(y = effectif_jour_mean)) +
  theme_bw() + # theme_classic()
  theme(panel.grid = element_blank())

write.csv(before_peak_france, "../../before_peak_france.csv")
write.csv(after_peak_france, "../../after_peak_france.csv")
