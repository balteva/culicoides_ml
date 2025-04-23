library(tidyverse)
library(sf)
library(gganimate)
library(RColorBrewer)

library(fst)


france <- st_read("./new_data/france_ecoclimatic_zones.gpkg")
france <- st_transform(france,4326)


joined_data <- readRDS( "./projections/presence_predictions/joined_abundance_presence_00_24.rds")


##avg monthly abundance
trial_avg_month_00_24 <- joined_data %>%
  group_by(month, ID_SITE,longitude, latitude)%>% #month,#ECO_CLI
  summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
  #mutate(date = as.Date(paste(year, month, "01", sep="-")))%>%
  mutate(month = factor(month, levels = 1:12, labels = month.name))%>%
  mutate(ranges = cut(mean_pred,
                      breaks = c(-Inf, 0, 10, 50, 100, 500, 1000),#my values
                      labels = c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)"),
                      right = T)) %>%
  mutate(ranges =factor(ranges, levels = c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)")))
#pachka values
# trial_avg_month_00_24_p <- trial_avg_month_00_24 %>%
# mutate(ranges = cut(mean_pred,
#                       breaks = unique(c(-Inf, 0, 10, 100, 1000, 10000, Inf)), 
#                       labels = c("Absence", "Très faible (1-10)", "Faible (11-100)","Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)"),
#                       right = TRUE)) %>%
#   mutate(ranges =factor(ranges, levels = c("Absence", "Très faible (1-10)", "Faible (11-100)",
#                                           "Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)")))
#

abundance_levels <- c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)")

abundance_colors <- rev(c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')) #rev to reverse!!

abundance_levels_p <- c("Absence", "Très faible (1-10)", "Faible (11-100)","Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)")
###########
ggplot()+
  geom_sf(data=france, color="cornsilk3", linewidth=0, alpha=1) +
  geom_tile(data =trial_avg_month_00_24,aes(x=longitude, y=latitude, fill=ranges))+  
  facet_wrap(~month)+
  scale_fill_manual(
    values = abundance_colors,
    breaks = abundance_levels,
    name = "Deviation (weeks)", drop = F ) +
  theme_minimal()+
  labs(title="C. obsoletus/C. scoticus mean monthly abundance \n (2000-2024)")+
  theme(legend.position = 'right',
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))



#############activity TRUE FALSE plots

trial_avg_activity_period<- joined_data %>%
  group_by(month, ID_SITE,longitude, latitude)%>% #month,#ECO_CLI
  summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
  #mutate(date = as.Date(paste(year, month, "01", sep="-")))%>%
  mutate(month = factor(month, levels = 1:12, labels = month.name))%>%
  mutate(is_active = ifelse(mean_pred >= 1, TRUE, FALSE)) 

activity_colors <- c("orange", "#91CF60")
activity_levels <- c("TRUE", "FALSE")

ggplot()+
  geom_sf(data=france, color="cornsilk3", linewidth=0, alpha=1) +
  geom_tile(data =trial_avg_activity_period,aes(x=longitude, y=latitude, fill=is_active))+  
  facet_wrap(~month)+
  scale_fill_manual(
    values = activity_colors,
    breaks = activity_levels,
    name = "> 1 individuals/ trap on average", drop = F ) +
  theme_minimal()+
  labs(title="C. obsoletus/C. scoticus monthly zones of vectorial activity \n (2000-2024)")+
  theme(legend.position = 'right',
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))

###################################################

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

activity_colors <-c('darkred', '#b2182b','#ef8a62','#fddbc7',"grey85",'#d1e5f0','#67a9cf','#2170ac','dodgerblue4')
activity_levels <- c("Earlier by more than 15 ","Earlier by 15 to 10 ", "Earlier by 10 to 5", "Earlier by 5 to 1",
                     "No deviation", "Later by 1-5", "Later by 5 to 10", "Later by 10 to 15", "Later by more than 15")



#####################################
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



##########################deviation mediane from abundance

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