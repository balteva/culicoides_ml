library(tidyverse)
library(scales)
library(patchwork)
library(data.table)
ecocli <- read.delim("./data/measurementorfacts.txt", header = TRUE)%>%
  filter(measurementID=="ECO_CLI") %>%
  select(c(id,measurementValue))%>%
  rename("ECO_CLI"=measurementValue)%>%
  rename("eventID"=id)


data <- read.delim("./data/occurrence.txt", header = TRUE) %>%
  select(eventID,individualCount, organismQuantityType, occurrenceStatus,occurrenceID, organismQuantity, scientificName)



#loading collection details (date, location, region)
event <- read.delim("./data/event.txt", header = TRUE) %>%
  select(eventID, eventDate,startDayOfYear,month,year, habitat, locationID, stateProvince)%>%
  mutate(date = as.Date(startDayOfYear - 1, origin = paste0(year, "-01-01")))

df_indiv <- data %>%
  left_join(event, by="eventID") %>%
  left_join(ecocli, by="eventID") %>%
  select(-c(eventDate, startDayOfYear)) %>%
  #filter(!occurrenceStatus == "absent")%>% ##remove filter for habitat/unique site counts
  relocate(stateProvince, locationID,date,ECO_CLI, .after= eventID) %>%
  filter(organismQuantityType=="individuals") #select organism type of interest (i.e. all type, only female, female, only parous female etc)

#sum with filter == "individuals" = [1] 6340177 #this is the correct one!


DP <- df_indiv %>%
  select(c(eventID, date, stateProvince, individualCount,locationID, year)) %>%
  group_by(eventID, date, stateProvince,locationID)%>%
  summarise(countPT=sum(individualCount))

# DP_empty <- DP%>%
#   filter(countPT<2)%>%
#   group_by(stateProvince)%>%
#   summarise(n=n())%>%
#   arrange(desc(n))
  
#> DP_empty
# A tibble: 13 × 2
# stateProvince                  n
# <chr>                      <int>
#   1 Auvergne-Rhône-Alpes         969
# 2 Occitanie                    861
# 3 Grand Est                    829
# 4 Bourgogne-Franche-Comté      700
# 5 Nouvelle-Aquitaine           569
# 6 Hauts-de-France              439
# 7 Centre-Val de Loire          407
# 8 Provence-Alpes-Côte d'Azur   386
# 9 Pays de la Loire             380
# 10 Normandie                    375
# 11 Île-de-France                299
# 12 Bretagne                     196
# 13 Corse                         50


DP_tpr <- DP%>%
  group_by(stateProvince)%>%
  summarise(zum=n_distinct(locationID))%>%
  arrange(desc(zum))

# stateProvince                zum
# <chr>                      <int>
#   1 Occitanie                     30
# 2 Auvergne-Rhône-Alpes          28
# 3 Nouvelle-Aquitaine            24
# 4 Grand Est                     23
# 5 Bourgogne-Franche-Comté       16
# 6 Provence-Alpes-Côte d'Azur    16
#  7 Hauts-de-France               15
#  8 Bretagne                      14
#  9 Pays de la Loire              14
# 10 Centre-Val de Loire           11
# 11 Normandie                     10
# 12 Île-de-France                  6
# 13 Corse                          4








####################### Pachka's code######################
# df_indiv%>%
#   setDT()
# 
# df_indiv[organismQuantityType == 'individuals', sum(organismQuantity), by = eventID] %>% .[V1 > 0,] %>% summary()
# Min.   : 2079   Min.   :     1.0  
# 1st Qu.: 5826   1st Qu.:     5.0  
# Median :10635   Median :    40.0  
# Mean   : 9985   Mean   :   677.4  
# 3rd Qu.:13694   3rd Qu.:   301.5  
# Max.   :19737   Max.   :163725.0 
###########################################################

#farms
farm_counts <- df_indiv %>% ### INFO! cannot filter out ABSENT occurences for these stages!!!
  select(c(locationID, habitat))%>%
  group_by(locationID, habitat)%>%
  unique() %>%
  group_by(habitat)%>%
  summarise(total_sites=n())%>%
  mutate(percent=total_sites/210*100, 
  total_count=sum(total_sites))
# habitat                                       total_sites percent total_count
# <chr>                                               <int>   <dbl>       <int>
# 1 cattle farm                                           137  65.2           210
# 2 educational farm - cattle farm                          1   0.476         210
# 3 goat farm                                               7   3.33          210
# 4 horse farm                                              1   0.476         210
# 5 mixed farming                                          44  21.0           210
# 6 sheep farm                                             19   9.05          210
# 7 vocational agricultural college - cattle farm           1   0.476         210


unique_trap_counts <- df_indiv %>%
  select(c(locationID, year))%>%
  group_by(locationID, year)%>%
  unique()%>%
  group_by(year)%>%
  summarise(total_sites=n())
  
# > unique_trap_counts
# # A tibble: 4 × 2
# year total_sites
# <int>       <int>
#   1  2009         168
# 2  2010         171
# 3  2011         166
# 4  2012         162

####################################################################################

most_abundant_species <- df_indiv %>%
  group_by(scientificName)%>%
  summarise(total=sum(individualCount))%>%
  arrange(desc(total))
# 1 obsoletus/scoticus 4186089
# 2 dewulfi             597361
# 3 imicola             521319
# 4 chiopterus          177757
# 5 newsteadi           163853
# 6 punctatus           139213
# 7 pulicaris           103284
# 8 achrayi              51401
# 9 brunnicans           49644
# 10 deltus               49536
####################################################################################

main_species_metrics <-  most_abundant_species %>%
  slice_head(n = 7) %>%  
  summarise(total_mainspecies=sum(total))%>%
  mutate(percentage=total_mainspecies/6340177*100)

# scientificName       total percentage_of_all
# <chr>                <int>             <dbl>
#   1 obsoletus/scoticus 4186089             66.0 
# 2 dewulfi             597361              9.42
# 3 imicola             521319              8.22
# 4 chiopterus          177757              2.80
# 5 newsteadi           163853              2.58
# 6 punctatus           139213              2.20
# 7 pulicaris           103284              1.63
###################################################################################
# total_mainspecies percentage
# <int>      <dbl>
#   1           5888876       92.9


#selecting species of veterinary interest 
species_of_interest <- c("obsoletus/scoticus", "dewulfi", "imicola", "chiopterus","newsteadi", "punctatus", "pulicaris" ) 
#obsoletus   scoticus  -> remove  , keep only complex species

#renaming species not in species of interest list into 'other'
df_filtered <- df_indiv %>%
  mutate(scientificName = case_when( !(scientificName %in% species_of_interest) ~ "other species", TRUE ~ scientificName))%>%
  #filter(!scientificName=="other species") %>% #put this in for maps
  mutate(week=week(date), month=month(date))
  group_by(locationID, date,year, scientificName) %>%
  summarise(count = sum(individualCount), .groups = "drop")




df_summary_median <- df_indiv %>%
  group_by(eventID) %>%
  summarise(NBINDIV_session=sum(individualCount))%>%
  summarise(
    mean_abundance = round(mean(NBINDIV_session),2),
    median_abundance = round(median(NBINDIV_session),2),
    q1 = round(quantile(NBINDIV_session, 0.25),2),
    q2 = round(quantile(NBINDIV_session, 0.75),2))
#MEAN   677.44
#MEDIAN 40
#Q1     5
#Q2     301.5

df_indiv%>%
  group_by(eventID)%>%
  summarise(individualCount=sum(individualCount))%>%
  ggplot(aes(individualCount)) +
  geom_histogram(col = "black", alpha= 0.7, fill="deepskyblue")+
  geom_vline(aes(xintercept = mean(individualCount), color = "mean"), linetype = "solid", size = 1)+ #adding descriptive stats
  geom_vline(aes(xintercept = median(individualCount), color = "median"), linetype = "solid", size = 1)+
  scale_x_log10(labels = label_number(), breaks=breaks_log(n=6, base=10))+
  theme_minimal()+
  
  ggtitle(expression("Distribution of" *italic(" Culicoides spp. ")* "capture values (France 2009 - 2012)"))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        axis.text.x = element_text(size=10, face="bold"),
        axis.text.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        strip.text=element_text(size=12,face="bold"),
        legend.position = "bottom")+
  labs(x="Individuals caught in one trapping session (log scale)",
       y="Frequency", color = "Statistics")+
  scale_color_manual(values=c("mean"="darkred", "median"="darkblue"))

#############"

df_indiv2<- df_indiv %>%
  mutate(scientificName = case_when( !(scientificName %in% species_of_interest) ~ "other species", TRUE ~ scientificName))%>%
  filter(!scientificName=="other species") #put this in for maps




# group_by(locationID, date,year, scientificName) %>%
#   summarise(count = sum(individualCount), .groups = "drop")%>%#not calculating the mean ,just renaming for functions




## manual function


df_temp <- df_indiv2 %>%
  mutate(week=week(date)) %>%
  group_by(ECO_CLI,month,year, scientificName)%>%
  summarise(
    mean_abundance = mean(individualCount),
    median_abundance = median(individualCount),
    q1 = quantile(individualCount, 0.25),
    q2 = quantile(individualCount, 0.75), .groups = "drop") %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-")))


df_temp %>%
  filter(scientificName=="obsoletus/scoticus")%>%
  ggplot(aes(x = month, y = median_abundance)) +
  geom_ribbon(aes(ymin = q1, ymax = q2, fill = "Central Range (25–75%)"), alpha = 0.6) +
  geom_point(aes(y = median_abundance), shape = 21, fill = "white", size = 1, stroke = 1, alpha=0.8) +
  geom_line(aes(y = median_abundance, color = "Median"), linewidth = 0.8, alpha = 0.9) +
  scale_x_continuous(breaks=seq(1, 12, 1))+
  labs(title = paste0("Interannual  Culicoides spp.  population dynamics"),
       y = "Number of individuals per trap",
       x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        axis.title.x = element_text(size = 10, face="bold"),
        axis.title.y = element_text(size = 10, face="bold"),
        plot.title = element_text(hjust = 0.5, size= 12, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.text=element_text(size=10,face="bold"))+
  scale_fill_manual(values = c("Central Range (25–75%)" = "deepskyblue2")) +
  scale_color_manual(values = c("Median" = "deepskyblue4")) +
  facet_grid(~ECO_CLI~year, scales="free") #ncol=2


