library(tidyverse)
library(sf)
library(gganimate)
library(RColorBrewer)

library(fst)



abundance_data<- readRDS("./projections/abundance_predictions/data2000_2025_abundance_predictions.rds")%>%
  rename(prediction_abundance = prediction)
presence_data <- readRDS("./projections/presence_predictions/data2000_2025_predictions.rds")%>%
  rename(prediction_presence = prediction)

joined_data <- presence_data %>%
  select(c(ID_SITE, DATE, longitude, latitude, prediction_presence))%>%
  left_join(abundance_data, by = c("ID_SITE", "DATE", "longitude" ,"latitude")) %>%
  mutate(month=month(DATE), year=year(DATE), week=week(DATE))%>%
  mutate(prediction_abundance=exp(prediction_abundance)) %>%
  mutate(prediction_final= ifelse(prediction_presence == 'Absence', 0, prediction_abundance))%>%
  select(-c("TX_0_0", "SWV_0_3","NDVI_0_5","FG_0_1", "ALT", "prediction_abundance", "prediction_presence"))

saveRDS(joined_data, "./projections/presence_predictions/joined_abundance_presence_00_24.rds")

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




joined_data <- readRDS( "./projections/presence_predictions/joined_abundance_presence_00_24.rds")
trial <- joined_data




##avg abundance per month across the years:



trial_avg_month_00_24 <- joined_data %>%
  group_by(month, ID_SITE,longitude, latitude)%>% #month,#ECO_CLI
  summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
  #mutate(date = as.Date(paste(year, month, "01", sep="-")))%>%
  mutate(month = factor(month, levels = 1:12, labels = month.name))#%>%
  mutate(ranges = cut(mean_pred,
                    breaks = c(-Inf, 0, 10, 50, 100, 500, 1000),
                    labels = c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)"),
                    right = T)) %>%
  mutate(ranges =factor(ranges, levels = c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)")))

  # trial_avg_month_00_24_p <- trial_avg_month_00_24 %>%
  # mutate(ranges = cut(mean_pred,
  #                       breaks = unique(c(-Inf, 0, 10, 100, 1000, 10000, Inf)), 
  #                       labels = c("Absence", "Très faible (1-10)", "Faible (11-100)","Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)"),
  #                       right = TRUE)) %>%
  #   mutate(ranges =factor(ranges, levels = c("Absence", "Très faible (1-10)", "Faible (11-100)",
  #                                           "Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)")))
  #        

###average abundance period for each month
trial_avg_abundance_period<- joined_data %>%
  group_by(month, ID_SITE,longitude, latitude)%>% #month,#ECO_CLI
  summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
  #mutate(date = as.Date(paste(year, month, "01", sep="-")))%>%
  mutate(month = factor(month, levels = 1:12, labels = month.name))%>%
  mutate(is_active=mean_pred >= 5) ###no clue what this is for
###########################################################################

trial_length_of_season_avg <-  joined_data %>%
  group_by(year,ID_SITE,longitude, latitude)%>% 
  mutate(end = ifelse (any (prediction_final >= 1), max(week[prediction_final >= 1], na.rm=TRUE), NA),
         start = ifelse (any (prediction_final >= 1), min(week[prediction_final >= 1], na.rm=TRUE), NA))%>%
  group_by(ID_SITE,longitude, latitude)%>%
  summarise(median_end=median(end), avg_end=mean(end),
            median_start=median(start), avg_start=mean(start))%>%
  ungroup() %>%
  select(c(ID_SITE,longitude, latitude,median_end, avg_end, median_start, avg_start))%>%
  distinct()%>%
  mutate(length=median_end-avg_start)

# 
# sus_sites <- trial_start_of_season_avg %>%
#   filter(is.na(start))%>%
#   distinct(ID_SITE)
# geom_sf(sus_sites, x=longitude, y=latitude)
# 
# test <- joined_data %>%
#   filter(ID_SITE=="5651"& year=="2014")
# ggplot()+
#   geom_tile(data=sus_sites,aes(x=longitude, y=latitude), fill="red")+
#   geom_sf(data=france, color="cornsilk3", linewidth=0, alpha=1) 

####start of activity season
trial_length_of_season<- joined_data %>%
  group_by(year, ID_SITE,longitude, latitude)%>% 
  summarise(last_week = ifelse (any (prediction_final >= 1), max(week[prediction_final >= 1], na.rm=TRUE), NA),
            first_week = ifelse (any (prediction_final >= 1), min(week[prediction_final >= 1], na.rm=TRUE), NA), .groups="drop")%>%
  mutate(length_unique = last_week - first_week)%>%
  left_join(trial_length_of_season_avg, by=c("ID_SITE", "longitude", "latitude"))%>%
  # mutate(error_weeks_avg_end = last_week - avg_end, error_weeks_median_end= last_week - median_end,
  #        error_weeks_avg_start = first_week - avg_start, error_weeks_median_start= first_week - median_start) %>%
  mutate(length_error=length_unique-length)
  

  # mutate(ranges = cut(error_weeks_avg,
  #                       breaks = c(-Inf,-15, -10, -5, -1, 1, 5, 10, 15, Inf),
  #                       labels = c("Earlier by more than 15 ","Earlier by 15 to 10 ", "Earlier by 10 to 5", "Earlier by 5 to 1",
  #                                "No deviation", "Later by 1-5", "Later by 5 to 10", "Later by 10 to 15", "Later by more than 15"),
  #                       right = TRUE))




 #diverging for presence absence ['#b2182b','#ef8a62','#fddbc7','#d1e5f0','#67a9cf','#2166ac']

#activity_colors <-c('darkred', '#b2182b','#ef8a62','#fddbc7',"grey85",'#d1e5f0','#67a9cf','#2170ac','dodgerblue4')
activity_levels <- c("Earlier by more than 15 ","Earlier by 15 to 10 ", "Earlier by 10 to 5", "Earlier by 5 to 1",
                     "No deviation", "Later by 1-5", "Later by 5 to 10", "Later by 10 to 15", "Later by more than 15")

activity_colors <- c('#a50f15', '#cb181d', '#ef3b2c',  '#ef8a62','grey82', 'skyblue2', '#6baed6', '#2171b5', 'dodgerblue4')


activity_colors2 <- c('#cb181d', '#ef3b2c',  '#ef8a62','grey82', 'skyblue2', '#6baed6', '#2171b5')



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
  geom_sf(data=france, color="cornsilk4", linewidth=0, alpha=1) +
  #geom_tile(data = trial_start_of_season,aes(x=longitude, y=latitude, fill=ranges))+  
  geom_tile(data = trial_length_of_season,aes(x=longitude, y=latitude, fill=length_error))+ 
  facet_wrap(~year)+
  scale_fill_gradientn(
    colors = activity_colors2,
    values = scales::rescale(c(-25, -15,  -5, 0, 5,  15, 25)),
    limits = c(-25, 25),
    breaks = c(-25, -15,  -5, 0, 5, 15, 25),
    labels = c("≤ -25", "-15", "-5", "0", "5","15", "≥ 25"),
    name = "Deviation (weeks)")+
  # scale_fill_manual(
  #   values = activity_colors,
  #    breaks = activity_levels,
  #   name = "Deviation (weeks)", drop = F ) +
  theme_minimal()+
  labs(title="C. obsoletus/C. scoticus change in activity period duration \n (2000-2024)")+
  theme(legend.position = 'right',
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))
################################################  making a dynamic map #############################
# colors = activity_colors,
# values = scales::rescale(c(-15, -12, -9, -6, 0, 3, 6, 9, 15)),
# limits = c(-15, 15),
# breaks = c(-15, -10, -5, 0, 5, 10, 15),
# labels = c("≤ -15", "-10", "-5", "0", "5", "10", "≥ 15"),
# name = "Deviation (weeks)")+
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
##########




p <- ggplot()+
  geom_sf(data=france, color="cornsilk4", linewidth=0, alpha=1) +
  geom_tile(data = trial_start_of_season,aes(x=longitude, y=latitude, fill=error_weeks_avg))+ 
  scale_fill_gradientn(
    colors = activity_colors,
    values = scales::rescale(c(-15, -12, -9, -6, 0, 3, 6, 9, 15)),
    limits = c(-15, 15),
    breaks = c(-15, -10, -5, 0, 5, 10, 15),
    labels = c("≤ -15", "-10", "-5", "0", "5", "10", "≥ 15"),
    name = "Deviation (weeks)")+
  theme_minimal()+
  labs(title="C. obsoletus/C. scoticus start of activity period \n over time: {frame_time}")+
  theme(legend.position = 'right',
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))+
  transition_time(year) #+

animate(p, width = 800, height = 600, fps = 4, duration = 10, renderer = gifski_renderer("activity_start_map2.gif"))

###########################################################################
#########################################
##################################
############################
################
###########

########
####
##########
###################
#############################"
#########################################
##############################################################################
#########################################
############################
######################
trial_median <-  joined_data %>%
  group_by(ID_SITE,longitude, latitude)%>% 
  summarise(median_total=median(prediction_final), .groups="drop")


trial_median_yearly<- joined_data %>%
  group_by(year, ID_SITE,longitude, latitude)%>% 
  summarise(median_yearly=median(prediction_final), .groups="drop")%>%
  left_join(trial_median, by=c("ID_SITE", "longitude", "latitude"))%>%
   mutate(median_error=median_yearly-median_total)


activity_colors <- c('#0A2AA8', "#6baed6", "grey90", "#ef3b2c", "#a50f15")
values <- scales::rescale(c(-50, -20, 0, 20, 50)) 


ggplot()+
  geom_sf(data=france, color="cornsilk4", linewidth=0, alpha=1) +
  #geom_tile(data = trial_start_of_season,aes(x=longitude, y=latitude, fill=ranges))+  
  geom_tile(data = trial_median_yearly,aes(x=longitude, y=latitude, fill=median_error))+ 
  facet_wrap(~year)+
  scale_fill_gradientn(
    colors = activity_colors,
    values = values,  
    limits = c(-50, 50),
    breaks = c(-50, -20, 0, 20, 50),
    labels = c("≤ -50", "-20", "0", "20", "≥ 50"),
    name = "Deviation in specimens caught per trap") +
  theme_minimal()+
  labs(title="C. obsoletus/C. scoticus change median abundance \n (2000-2024)")+
  theme(legend.position = 'right',
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))