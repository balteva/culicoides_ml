library(data.table)
library(tidyverse)
library(sf)
setwd("E:/ieva")



abundance_data<- readRDS("./abundance_predictions/data2000_2025_abundance_predictions.rds")%>%
  rename(prediction_abundance = prediction)
presence_data <- readRDS("./presence_predictions/data2000_2025_predictions.rds")%>%
  rename(prediction_presence = prediction)

joined_data <- presence_data %>%
  select(c(ID_SITE, DATE, longitude, latitude, prediction_presence))%>%
  left_join(abundance_data, by = c("ID_SITE", "DATE", "longitude" ,"latitude")) 






trial <- joined_data%>%
  mutate(month=month(DATE), year=year(DATE))%>%
  mutate(month = factor(month, levels = 1:12, labels = month.name))%>%
  mutate(prediction_abundance=exp(prediction_abundance)) %>%
  mutate(prediction_final= ifelse(prediction_presence == 'Absence', 0, prediction_abundance))%>%
  group_by(year,ID_SITE, month,longitude, latitude, ECO_CLI)%>% #month,
  summarise(mean_pred=mean(prediction_final), median_pred=median(prediction_final), .groups="drop")%>%
  as.data.table()

 



trial[,V1_class:=cut(median_pred,
                     breaks = unique(c(-Inf, 0, 10, 100, 1000, 10000, Inf)), 
                     labels = c("Absence", "Très faible (1-10)", "Faible (11-100)","Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)"),
                     right = TRUE)]#intervals are right closed [1-10] instead of [1-10)

trial$V1_class <- factor(trial$V1_class, levels = c("Absence", "Très faible (1-10)", "Faible (11-100)",
                                    "Moyenne (101-1000)", "Élevée (1001-10000)", "Très élevé (>10000)"))

france <- st_read("../Data_FR/france_ecoclimatic_zones.gpkg")
france <- st_transform(france,4326)


ggplot()+
  geom_tile(data = trial %>% filter(year==2008) ,aes(x=longitude, y=latitude, fill=V1_class))+ 
   geom_sf(data=france, color="red", alpha=0) +
  facet_wrap(~month)+
  scale_fill_brewer(palette = "RdYlGn", direction = -1, drop = FALSE) +#direction -1 reverses color palette and drop=F ensures all factor levels appear even if theyre unused
  theme_minimal()+
  labs(fill = "Predicted abundance", title="Abundance model predictions for 2001 data")+
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust=0.5, face="bold", size=11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank())

#
