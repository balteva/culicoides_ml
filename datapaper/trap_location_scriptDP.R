
library(tidyverse)
library(sf)
library(gganimate)
library(RColorBrewer)

library(fst)


france <- st_read("D:/DATA_FR/france_ecoclimatic_zones.gpkg")
france <- st_transform(france,4326)



all_yrs <- coord2009 %>%
  rbind(coord2010)%>%
  rbind(coord2011)%>%
  rbind(coord2012)
all_colors <- paletteer::paletteer_d("ggthemes::Jewel_Bright")

coord_all <- read.csv("D:/DATA_FR/culicoides_point_locations_grid_150_150.csv")

selected_colors1 <- c("#31a354", "#7fcdbb", "deepskyblue3","#253494" )


ggplot()+
  geom_sf(data=france, aes(fill=code), linewidth=0.8, alpha=0.6, color="black") +
  annotation_scale(location = "bl", width_hint = 0.3, 
                   bar_cols = c("black", "white"), 
                   unit_category = "metric") + 
  geom_point(data = coord_all ,aes(x=longitude, y=latitude), stroke=0.9, size=2, shape=21,fill="red", color="black")+ 
  theme_minimal()+
  labs(title="Trap locations", fill="Biogreographic Region")+
  scale_fill_manual(values = selected_colors1)+
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust=0.5, face="bold", size=13),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face="bold", size =10))
# +facet_wrap(~year)





ggplot() +
  geom_sf(data = france, aes(fill = code), linewidth = 0.8, alpha = 0.6, color = "black") +
  geom_point(data = all_yrs, aes(x = decimalLongitude, y = decimalLatitude, color = as.factor(year)), stroke = 2, size = 2, shape = 21) +
  scale_color_paletteer_d("tvthemes::Stannis")+
  scale_fill_manual(values = selected_colors1) +
  theme_minimal() +
  labs(
    title = "Trap locations",
    fill = "Biogeographic Region",
    color = "Trap year"
  ) +
  theme(
    legend.position = 'right',
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = 10))
