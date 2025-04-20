library(tidyverse)
library(sf)
library(gganimate)
library(RColorBrewer)

library(fst)


ndvi <- read_fst ("E:/Data_FR/NDVI.fst")
longlat <- read_fst("E:/Data_FR/covariates_lon_lat.fst")



abundance_data<- readRDS("./projections/abundance_predictions/data2000_2025_abundance_predictions.rds")%>%
  rename(prediction_abundance = prediction)
presence_data <- readRDS("./projections/presence_predictions/data2000_2025_predictions.rds")%>%
  rename(prediction_presence = prediction)

joined_data <- presence_data %>%
  select(c(ID_SITE, DATE, longitude, latitude, prediction_presence))%>%
  left_join(abundance_data, by = c("ID_SITE", "DATE", "longitude" ,"latitude")) 



# trial <- joined_data%>%
#   mutate(month=month(DATE), year=year(DATE))%>%
#   mutate(prediction_abundance=exp(prediction_abundance)) %>%
#   mutate(prediction_final= ifelse(prediction_presence == 'Absence', 0, prediction_abundance))%>%
#   group_by(year,month, ID_SITE,longitude, latitude, ECO_CLI)%>% #month,
#   summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
#   mutate(date = as.Date(paste(year, month, "01", sep="-")))%>%
#  mutate(month = factor(month, levels = 1:12, labels = month.name))
# 
# 
# trial <- trial %>%
#   mutate(ranges = cut(mean_pred, 
#                       breaks = c(-Inf, 0, 10, 50, 100, 500, 1000),
#                       labels = c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)"),
#                       right = T)) %>%
#   mutate(ranges =factor(ranges, levels = c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)")))
# 




###################

#this is the base im working on.
trial <- joined_data%>%
  mutate(month=month(DATE), year=year(DATE))%>%
  mutate(prediction_abundance=exp(prediction_abundance)) %>%
  mutate(prediction_final= ifelse(prediction_presence == 'Absence', 0, prediction_abundance))

##avg abundance per month across the years:



trial_avg_month_00_24 <- trial %>%
  group_by(month, ID_SITE,longitude, latitude)%>% #month,#ECO_CLI
  summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
  #mutate(date = as.Date(paste(year, month, "01", sep="-")))%>%
  mutate(month = factor(month, levels = 1:12, labels = month.name))#%>%
  # mutate(ranges = cut(mean_pred, 
  #                   breaks = c(-Inf, 0, 10, 50, 100, 500, 1000),
  #                   labels = c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)"),
  #                   right = T)) %>%
  # mutate(ranges =factor(ranges, levels = c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)")))
    
  # trial_avg_month_00_24_p <- trial_avg_month_00_24 %>%
  # mutate(ranges = cut(mean_pred,
  #                       breaks = unique(c(-Inf, 0, 10, 100, 1000, 10000, Inf)), 
  #                       labels = c("Absence", "Très faible (1-10)", "Faible (11-100)","Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)"),
  #                       right = TRUE)) %>%
  #   mutate(ranges =factor(ranges, levels = c("Absence", "Très faible (1-10)", "Faible (11-100)",
  #                                           "Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)")))
  #        

###average activity period
trial_avg_activity_period<- trial %>%
  group_by(month, ID_SITE,longitude, latitude)%>% #month,#ECO_CLI
  summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
  #mutate(date = as.Date(paste(year, month, "01", sep="-")))%>%
  mutate(month = factor(month, levels = 1:12, labels = month.name))%>%
  mutate(is_active=mean_pred >= 5)
###########################################################################

trial_start_of_season_avg <-  trial %>%
  mutate(week=week(as.Date(DATE)))%>% #calculate the earliest week
  group_by(week,  ID_SITE,longitude, latitude)%>% 
  summarise(mean_pred=mean(prediction_final), .groups="drop")%>%
  group_by(ID_SITE,longitude, latitude) %>% #remove year to have overall avg 
  mutate(avg_start = ifelse (any (mean_pred >= 5), min(week[mean_pred >= 5], na.rm=TRUE), NA))%>%#luckily theres no NA
  select(c(ID_SITE,longitude, latitude,avg_start))%>%
  distinct()



####start of activity season
trial_start_of_season<- trial %>%
  mutate(week=week(as.Date(DATE)))%>% #calculate the earliest week
  group_by(week, year, ID_SITE,longitude, latitude)%>% 
  summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
  group_by(ID_SITE,longitude, latitude, year) %>% #remove year to have overall avg 
  mutate(first_week = ifelse (any (mean_pred >= 5), min(week[mean_pred >= 5], na.rm=TRUE), 52))%>%#put 52 because thats like the entire year and it means it ddint start
  select(year, ID_SITE, longitude, latitude, first_week)%>%
  distinct() %>%
  left_join(trial_start_of_season_avg, by=c("ID_SITE", "longitude", "latitude"))%>%
  mutate(error_weeks = first_week - avg_start) %>%
  mutate(ranges = cut(error_weeks,
                        breaks = c(-Inf,-15, -10, -5, -1, 1, 5, 10, 15, Inf),
                        labels = c("Earlier by more than 15 ","Earlier by 15 to 10 ", "Earlier by 10 to 5", "Earlier by 5 to 1",
                                 "No deviation", "Later by 1-5", "Later by 5 to 10", "Later by 10 to 15", "Later by more than 15"),
                        right = TRUE))

trial_start_of_season <- trial_start_of_season %>%
  mutate(ranges = cut(error_weeks,
                      breaks = c(-Inf, -15, -10, -5, -1, 1, 5, 10, 15, Inf),
                      labels = c("Earlier by more than 15 ","Earlier by 15 to 10 ", "Earlier by 10 to 5", "Earlier by 5 to 1",
                                 "No deviation", "Later by 1-5", "Later by 5 to 10", "Later by 10 to 15", "Later by more than 15"),
                      right = TRUE))


 #diverging for presence absence ['#b2182b','#ef8a62','#fddbc7','#d1e5f0','#67a9cf','#2166ac']

activity_colors <-c('darkred', '#b2182b','#ef8a62','#fddbc7',"grey85",'#d1e5f0','#67a9cf','#2170ac','dodgerblue4')
activity_levels <- c("Earlier by more than 15 ","Earlier by 15 to 10 ", "Earlier by 10 to 5", "Earlier by 5 to 1",
                     "No deviation", "Later by 1-5", "Later by 5 to 10", "Later by 10 to 15", "Later by more than 15")


################### FIN D'ACTIVITE
trial_end_of_season_avg <-  trial %>%
  mutate(week=week(as.Date(DATE)))%>% #calculate the earliest week
  group_by(week,  ID_SITE,longitude, latitude)%>% 
  summarise(mean_pred=mean(prediction_final), .groups="drop")%>%
  group_by(ID_SITE,longitude, latitude) %>% #remove year to have overall avg 
  mutate(avg_end = ifelse (any (mean_pred >= 5), max(week[mean_pred >= 5], na.rm=TRUE), NA))%>%#luckily theres no NA
  select(c(ID_SITE,longitude, latitude,avg_end))%>%
  distinct()

####start of activity season
trial_end_of_season<- trial %>%
  mutate(week=week(as.Date(DATE)))%>% #calculate the earliest week
  group_by(week, year, ID_SITE,longitude, latitude)%>% 
  summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
  group_by(ID_SITE,longitude, latitude, year) %>% #remove year to have overall avg 
  mutate(last_week = ifelse (any (mean_pred >= 5), max(week[mean_pred >= 5], na.rm=TRUE), 52))%>%#put 52 because thats like the entire year and it means it ddint start
  select(year, ID_SITE, longitude, latitude, last_week)%>%
  distinct() %>%
  left_join(trial_end_of_season_avg, by=c("ID_SITE", "longitude", "latitude"))%>%
  mutate(error_weeks = last_week - avg_end) %>%
  mutate(ranges = cut(error_weeks,
                      breaks = c(-Inf,-15, -10, -5, -1, 1, 5, 10, 15, Inf),
                      labels = c("Earlier by more than 15 ","Earlier by 15 to 10 ", "Earlier by 10 to 5", "Earlier by 5 to 1",
                                 "No deviation", "Later by 1-5", "Later by 5 to 10", "Later by 10 to 15", "Later by more than 15"),
                      right = TRUE))



#diverging for presence absence ['#b2182b','#ef8a62','#fddbc7','#d1e5f0','#67a9cf','#2166ac']

activity_colors <-c('darkred', '#b2182b','#ef8a62','#fddbc7',"grey85",'#d1e5f0','#67a9cf','#2170ac','dodgerblue4')
activity_levels <- c("Earlier by more than 15 ","Earlier by 15 to 10 ", "Earlier by 10 to 5", "Earlier by 5 to 1",
                     "No deviation", "Later by 1-5", "Later by 5 to 10", "Later by 10 to 15", "Later by more than 15")


###################






#####################


# diverging for presence absence ['#b2182b','#ef8a62','#fddbc7','#d1e5f0','#67a9cf','#2166ac']

# Pachka colors:
# > library(RColorBrewer)
# > brewer.pal(n=6, name="RdYlGn")
# diverging for abuncance ['#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850']



france <- st_read("./new_data/france_ecoclimatic_zones.gpkg")
france <- st_transform(france,4326)


################
abundance_levels <- c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)")
abundance_colors <- rev(c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')) #rev to reverse!!

abundance_levels_p <- c("Absence", "Très faible (1-10)", "Faible (11-100)","Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)")
activity_colors <- c("orange", "#91CF60")
activity_levels <- c("TRUE", "FALSE")

ggplot()+
  geom_sf(data=france, color="cornsilk3", linewidth=0, alpha=1) +
  geom_tile(data = trial_end_of_season,aes(x=longitude, y=latitude, fill=ranges))+  
  facet_wrap(~year)+
  scale_fill_manual(
    values = activity_colors,
     breaks = activity_levels,
    name = "Deviation (weeks)", drop = F ) +
  theme_minimal()+
  labs(title="C. obsoletus/C. scoticus start of activity period \n (2000-2024)")+
  theme(legend.position = 'right',
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))
################################################  making a dynamic map #############################

# 
# p <- ggplot()+
#   geom_tile(data = trial, aes(x=longitude, y=latitude, fill=ranges))+
#   geom_sf(data=france, color="black", linewidth=0.8, alpha=0) +
#   scale_fill_manual(
#     values = abundance_colors,
#     breaks = abundance_levels,
#     name = "Abundance", drop = F ) +
#   theme_minimal()+
#   labs(fill = "Abundance level", title="C. obsoletus / C. scoticus projected abundance over time: {frame_time}")+
#   theme(legend.position = 'bottom',
#         plot.title = element_text(hjust=0.5, face="bold", size=11),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text = element_blank())+
#   transition_time(date) #+
#   # ease_aes('linear')
# 
# animate(p, width = 800, height = 600, fps = 6, duration = 50, renderer = gifski_renderer("abundance_map2.gif"))


