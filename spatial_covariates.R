###Spatial data
library(sf)
library(modisfast)
library(terra)
library(mapview)
library(tidyverse)


library(hrbrthemes)
library(viridis)

df_short <- read.csv("../df_short.csv") 
df_short_sf <- df_short %>%
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)  #st_as_sf is convert st to sf object



###spatial mapping




##culicoides count range because its different across the years, will use for plots
count_range <- as.data.frame(df_short_sf) %>%
  select(year, ID_SITE, NBINDIV) %>%
  group_by(ID_SITE, year) %>%
  summarise(sum_cul = sum(NBINDIV), .groups = "drop") %>%
  summarise(min_cul = min(sum_cul), max_cul = max(sum_cul))

min_cul <- count_range$min_cul
max_cul <- count_range$max_cul


##plotting the culicoides count across the years
counts2009 <- df_short_sf %>%
  select(!ESPECE) %>%
  filter(year == 2009) %>%
  group_by(ID_SITE) %>%
  summarise(sum_cul = sum(NBINDIV))%>%
  ungroup() %>%
  st_as_sf()

france <- st_read("../gadm40_FRA_shp/gadm40_FRA_0.shp")

  
 map2009 <-ggplot() +
  geom_sf(data = france, fill = "lightblue", color = "black") +
  geom_sf(data = counts2009, aes(size = sum_cul), color = "red") + 
  scale_size_continuous(name = "Number of individuals",
                        range= c(2,9), ##setting the size of the NBINDV foe easier visualisation/representation, aka circle size from 2 to 9
                        limits= c(min_cul, max_cul))+ #ensuring count scale is same acoss all maps
  labs(title = "Culicoides counts in 2009")+
  theme_minimal()

####
  
  
  counts2010 <- df_short_sf %>%
  select(!ESPECE) %>%
  filter(year == 2010) %>%
  group_by(ID_SITE) %>%
  summarise(sum_cul = sum(NBINDIV))%>%
  ungroup() %>%
  st_as_sf()
# min(counts2010$sum_cul)
# max(counts2010$sum_cul)
# 
# 
# min(counts2011$sum_cul)
# max(counts2011$sum_cul)
  map2010 <-  ggplot() +
  geom_sf(data = france, fill = "lightblue", color = "black") +
  geom_sf(data = counts2010, aes(size = sum_cul), color = "red") + 
  scale_size_continuous(name = "Number of individuals",
                        range=c(2,9),
                        limits= c(min_cul, max_cul))+
  labs(title = "Culicoides counts in 2010")+
  theme_minimal()
  ###
  
  counts2011 <- df_short_sf %>%
    select(!ESPECE) %>%
    filter(year == 2011) %>%
    group_by(ID_SITE) %>%
    summarise(sum_cul = sum(NBINDIV))%>%
    ungroup() %>%
    st_as_sf()
  
  map2011 <-  ggplot() +
    geom_sf(data = france, fill = "lightblue", color = "black") +
    geom_sf(data = counts2010, aes(size = sum_cul), color = "red") + 
    scale_size_continuous(name = "Number of individuals",
                          range=c(2,9),
                          limits= c(min_cul, max_cul))+
    labs(title = "Culicoides counts in 2011")+
    theme_minimal()
  
  ## 
  counts2012 <- df_short_sf %>%
    select(!ESPECE) %>%
    filter(year == 2012) %>%
    group_by(ID_SITE) %>%
    summarise(sum_cul = sum(NBINDIV))%>%
    ungroup() %>%
    st_as_sf()
  
 map2012 <- ggplot() +
    geom_sf(data = france, fill = "lightblue", color = "black") +
    geom_sf(data = counts2012, aes(size = sum_cul), color = "red") +
    scale_size_continuous(name = "Number of individuals",
                          range=c(2,9),
                          limits= c(min_cul, max_cul))+
    labs(title = "Culicoides counts in 2012")+
    theme_minimal()
 library(gridExtra)
 pdf("C:/Users/ibalt/OneDrive/Desktop/france2009_2012_maps.pdf", width = 17, height = 15)
 grid.arrange(map2009, map2010, map2011, map2012, ncol = 2, nrow = 2)
 dev.off()
 
 

 
counts_all <- as.data.frame(df_short_sf) %>%
  select(NBINDIV, year, ID_SITE) %>%
  group_by(year)

counts_all$year <- as.factor(counts_all$year)

 
boxplot_vertical <- counts_all %>%
   ggplot( aes(x=year, y=NBINDIV, fill=year)) +
   geom_boxplot() +
  scale_y_log10() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) + ##to show all data points(makes it look messy)
  theme_minimal() +
   theme(
     legend.position="none",
     plot.title = element_text(size=11)) +
   ggtitle("Boxplot of Culicoides counts for different years") +
   xlab("year") +
   ylab("n of culicoides (log scale)")



boxplot_horizontal <- counts_all %>%
  ggplot( aes(x=NBINDIV, y=year, fill=year)) +
  geom_boxplot() +
  scale_x_log10() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) + 
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)) +
  ggtitle("Boxplot of Culicoides counts for different years") +
  xlab("n of culicoides (log scale)") +
  ylab("year")
 
library(gridExtra)
pdf("C:/Users/ibalt/OneDrive/Desktop/boxplots.pdf", width = 8, height = 10)
grid.arrange(boxplot_vertical, ncol = 1, nrow = 1)
dev.off()