library(tidyverse)
library(sf)
library(gganimate)
library(RColorBrewer)
library(fst)



# abundance_data<- readRDS("D:/abundance_preds_28/data2000_2025_abundance_predictions_tx.rds")%>%
#   rename(prediction_abundance = prediction)
# presence_data <- readRDS("D:/presence_preds_29/data2000_2025_presence_pares_predictions.rds")%>%
#   rename(prediction_presence = prediction)
# 
# joined_data <- presence_data %>%
#   select(c(ID_SITE, DATE, longitude, latitude, prediction_presence))%>%
#   left_join(abundance_data, by = c("ID_SITE", "DATE", "longitude" ,"latitude")) %>%
#   mutate(month=month(DATE), year=year(DATE), week=week(DATE))%>%
#   mutate(prediction_abundance=exp(prediction_abundance)) %>%
#   mutate(prediction_final= ifelse(prediction_presence == 'Absence', 0, prediction_abundance))%>%
#   select(c(ID_SITE, DATE, longitude, latitude, prediction_final, year, month, week))
# 
# saveRDS(joined_data, "./joined_abundance_presence_00_24_new.rds")##the data is already exponentiated!
# 

joined_data <- readRDS("D:/joined_abundance_presence_00_24_new.rds")




########################
month_avg20yr <- joined_data %>%
  group_by(month, ID_SITE,longitude, latitude)%>% #month,#ECO_CLI
  summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
  #mutate(date = as.Date(paste(year, month, "01", sep="-")))%>%
  mutate(month = factor(month, levels = 1:12, labels = month.name))%>%
  mutate(ranges = cut(mean_pred,
                      breaks = c(-Inf, 0, 10, 50, 100, 500, 1000),
                      labels = c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)"),
                      right = T)) %>%
  mutate(ranges =factor(ranges, levels = c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)")))

#   mutate(ranges = cut(mean_pred,
#                       breaks = unique(c(-Inf, 0, 10, 100, 1000, 10000, Inf)),
#                       labels = c("Absence", "Très faible (1-10)", "Faible (11-100)","Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)"),
#                       right = TRUE)) %>%
#   mutate(ranges =factor(ranges, levels = c("Absence", "Très faible (1-10)", "Faible (11-100)",
#                                           "Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)")))

#   abundance_levels_p <- c("Absence", "Très faible (1-10)", "Faible (11-100)","Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)")









################### Plot
france <- st_read("D:/DATA_FR/france_ecoclimatic_zones.gpkg")
france <- st_transform(france,4326)


################
abundance_levels <- c("Absence", "Very low (1-10)", "Low (11-50)", "Moderate (51-100)", "High (101-500)", "Very High (>500)")
abundance_colors <- rev(c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')) #rev to reverse!!

ggplot()+
  geom_sf(data=france, color="cornsilk4", linewidth=0, alpha=1) +
  geom_tile(data = month_avg20yr,aes(x=longitude, y=latitude, fill=ranges))+ 
  facet_wrap(~month)+
  scale_fill_manual(
    values = abundance_colors,
    breaks = abundance_levels,
    name = "Abundance", drop = F ) +
  theme_minimal()+
  labs(title="C. obsoletus/C. scoticus monthly abundance \n (2000-2024 average)")+
  theme(legend.position = 'right',
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))

##############################################
# Is ACTIVE? Y/N 20 yr average for each month
#############################################


is_active<- joined_data %>%
  group_by(month, ID_SITE,longitude, latitude)%>%
  summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
  mutate(month = factor(month, levels = 1:12, labels = month.name))%>%
  mutate(is_active=mean_pred >= 1) 


###########################################
# PLOT
###########################################

activity_colors <- c("orange", "#91CF60")
activity_levels <- c("TRUE", "FALSE")

ggplot()+
  geom_sf(data=france, color="cornsilk4", linewidth=0, alpha=1) +
  geom_tile(data = is_active,aes(x=longitude, y=latitude, fill=is_active))+ 
  facet_wrap(~month)+
  scale_fill_manual(
    values = activity_colors,
    breaks = activity_levels,
    name = "IS ACTIVE?", drop = F ) +
  theme_minimal()+
  labs(title="C. obsoletus/C. scoticus monthly activity\n(2000-2024 average)",
       subtitle="(determined by > 5 parous female threshold)")+
  theme(legend.position = 'right',
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        plot.subtitle = element_text(hjust=0.5, face="italic", size=9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))



###############################################
# ACTIVITY PERIOD START
###############################################


allyr_avg <- joined_data %>%
  group_by(year,ID_SITE,longitude, latitude)%>% 
  summarise(end = ifelse (any (prediction_final >= 1), max(week[prediction_final >= 1], na.rm=TRUE), NA),
         start = ifelse (any (prediction_final >= 1), min(week[prediction_final >= 1], na.rm=TRUE), NA))%>%
  group_by(ID_SITE,longitude, latitude)%>%
  summarise(median_end=median(end, na.rm=T), avg_end=mean(end, na.rm=T),
            median_start=median(start, na.rm=T), avg_start=mean(start, na.rm=T))%>%
  ungroup() %>%
  mutate(duration_weeks=median_end-median_start)


season_start_end <-  joined_data %>%
  group_by(year,ID_SITE)%>% 
  summarise(end = ifelse (any (prediction_final >= 1), max(week[prediction_final >= 1], na.rm=TRUE), NA),
         start = ifelse (any (prediction_final >= 1), min(week[prediction_final >= 1], na.rm=TRUE), NA))%>%
  mutate(duration_yearly=end-start)%>%
  left_join(allyr_avg, by="ID_SITE")%>%
  group_by(year, ID_SITE, longitude, latitude)%>%
  summarise(duration_error=duration_yearly - duration_weeks,
            start_error=start-median_start,
            end_error=end-median_end)%>%
  ungroup()
  
  
df1 <- season_start_end %>%
  filter(!is.na(duration_error), !is.na(start_error), !is.na(end_error))
#> hist(df1$duration_error)

#############################################
# PLOT
#############################################

activity_levels <- c("Earlier by more than 15 ","Earlier by 15 to 10 ", "Earlier by 10 to 5", "Earlier by 5 to 1",
                     "No deviation", "Later by 1-5", "Later by 5 to 10", "Later by 10 to 15", "Later by more than 15")
activity_colors <- c('#a50f15', '#cb181d', '#ef3b2c',  '#ef8a62','grey82', 'skyblue2', '#6baed6', '#2171b5', 'dodgerblue4')

activity_colors2 <- c('#cb181d', '#ef3b2c',  '#ef8a62','grey82', 'skyblue2', '#6baed6', '#2171b5')


limits_anomalies <- 15 #25 for activity duration, 15 for activity start, #15 for end
ggplot()+
  geom_sf(data=france, color="cornsilk4", linewidth=0, alpha=1) +
  geom_tile(data = df1%>%filter(year>2000 & year<2024),aes(x=longitude, y=latitude, fill=end_error))+ #duration_error, start_error, end_error 
  facet_wrap(~year)+
  scale_fill_stepsn(colors=rev(c('#b2182b','#ef8a62','#fddbc7', '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')), #add rev(c(colors)) for colors for anomalies in duration and for activity end
                    n.breaks=9,
                    limits=c(-limits_anomalies,limits_anomalies), show.limits=T)+
  theme_minimal()+
  labs(title="C. obsoletus/C. scoticus activity period end \n (2000-2024)", fill="Variation of activity end\ncompared to the median\n(in weeks)")+
  theme(legend.position = 'right',
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        legend.title = element_text(hjust=0.5, face="bold", size=9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))


###############################################################################
# ANOMALIES COMPARED TO MEDIAN ABUNDANCE
###############################################################################
abund_median <-  joined_data %>%
  group_by(ID_SITE)%>% 
  summarise(median_total=median(prediction_final), .groups="drop")


abund_median_yearly<- joined_data %>%
  group_by(year, ID_SITE,longitude, latitude)%>% 
  summarise(median_yearly=median(prediction_final), .groups="drop")%>%
  left_join(abund_median, by="ID_SITE") %>%
  mutate(median_error=median_yearly-median_total)


limits_anomalies <- 80

ggplot()+
  geom_sf(data=france, color="cornsilk4", linewidth=0, alpha=1) +
  geom_tile(data = abund_median_yearly,aes(x=longitude, y=latitude, fill=median_error))+ 
  facet_wrap(~year)+
  # scale_fill_stepsn(colors=rev(c('#b2182b','#ef8a62','#f9a19a', 'white','#b5e2f1','#67a9cf','#2166ac')),
  #                   n.breaks=9,
  #                  limits=c(-limits_anomalies,limits_anomalies), show.limits=T)+
  scale_fill_stepsn(colors=c('#2166ac', "#6baed6", "#b5e2f1", "#f7f7f7", "#ef8a62", "#ef5f3d", "darkred"), #my colors
                                       n.breaks=9,
                                       limits=c(-limits_anomalies,limits_anomalies), show.limits=T)+
  theme_minimal()+
  labs(title="C. obsoletus/C. scoticus change median abundance\n (2000-2024)", fill="Deviation in captures per trap")+
  theme(legend.position = 'right',
        legend.title = element_text(hjust=0.5, face="bold", size=9),
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))




