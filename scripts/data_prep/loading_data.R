library(fst)
library(tidyverse)
library(lubridate)

# piegedata prep
# meteodata prepend(
#   dataviz
#   ccm
# resume what we said abt separate modelisation presence absence And abundance
# analysing separately spatial and meteo variables in a multivariate model
###try separating species
#calculate other coefficients such as spearmans
##

data <-load("../database_ocapi_09_12_survFR.rda")

df <- read.fst("../covariates.fst")
setDT(df)
data <- as.data.frame(database_ocapi)

data <- database_ocapi %>% 
  select(ID_SITE, DATE=DATEFIN, NBINDIV, COMMUNELOC)


# commune_info <- data %>%
#   select(ID_SITE, COMMUNELOC) %>%
#   distinct(ID_SITE, .keep_all = TRUE)
#joining communes to df with climate info
# df <- df %>%
#   left_join(commune_info, by = "ID_SITE")

data$NBINDIV <- as.numeric(df$NBINDIV)
    
setDT(df)
#making df with count info


NBINDIV_na <- is.na(data$NBINDIV)
df_new <- subset(data, !NBINDIV_na)

#selecting columns of interest
df_sum <- df_new %>%
  group_by(ID_SITE, DATE, COMMUNELOC)%>%
  summarise(NBINDIV_sum=sum(NBINDIV))%>%
  ungroup()
             
# names(df_new)
# culic <- culic %>%
#   group_by(DATE,ID_SITE, ECO_CLI, COMMUNELOC) %>% # ECO_CLI) %>%
#   summarize(NBINDIV=sum(NBINDIV), .groups="drop")
# 
# df_sum <- df_new %>%
#   group_by(DATE, ID_SITE) %>%
#   summarise(NBINDIV=sum(NBINDIV))%>%
#   ungroup()


# df_sum <- df_new %>%
#   group_by(DATE, ID_SITE) %>%
#   summarise(
#     NBINDIV = sum(NBINDIV, na.rm = TRUE),
#     across(everything(), first)  # Keeps the first value of all other columns
#   ) %>%
#   ungroup()


view(df_sum)

NBINDIV_file <- ("C:/Users/ibalt/OneDrive/Desktop/culicoides_df.csv")
write.csv(culic ,file=NBINDIV_file, row.names = FALSE)

species_counts<- ("C:/Users/ibalt/OneDrive/Desktop/species_df.csv")
 
write.csv(df_new ,file=species_counts, row.names = FALSE)

#making df with meteo info (need to choose variables)
meteo <- df %>%
  select(-ESPECE, -NBINDIV)%>%
  distinct()

meteo_file <-("C:/Users/ibalt/OneDrive/Desktop/meteo_df.csv")
write.csv(meteo ,file=meteo_file, row.names = FALSE)


 
# NBINDIV_file <- ("C:/Users/ibalt/OneDrive/Desktop/df_short.csv")
# write.csv(df_short ,file=output_file, row.names = FALSE)









#selecting columns of interest
# df_short <- as.data.frame(data) %>%
#   select(ID_SITE, DATE, NBINDIV,ESPECE,ECO_CLI, longitude, latitude) %>%
#   filter(NBINDIV != 0) %>%
#   mutate(DATE = as.Date(DATE))
# 
# 
# #sum(df_short$NBINDIV) #total number of culicoides in the dataset 6404081 for 2009-2012 period
# 
# #adding up all species per day per site 
# 
# df_MY<- df_short%>%
#   group_by(DATE,ID_SITE) %>% #,ECO_CLI) %>%
#   summarize(NBINDIV_per_trap=sum(NBINDIV), .groups="drop")

#removing NA values (introduced by adding climate data)
# NBINDIV_na <- is.na(data_na$NBINDIV)
# data <- subset(data_na, !NBINDIV_na)