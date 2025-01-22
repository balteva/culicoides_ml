

library(sf)
library(modisfast)
library(terra)
library(mapview)
library(tidyverse)
library(lubridate)
load("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/database_ocapi_09_12_survFR.rda") 

culdata <- database_ocapi

library(lubridate)

names(culdata)
culdata <- culdata %>%
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>%  #st_as_sf is convert st to sf object
  mutate(
    DATEDETERMINATIONDEF = as.Date(DATEDETERMINATIONDEF, format="%Y-%m-%d"),
    DATEFIN = as.Date(DATEFIN, format="%Y-%m-%d"),
    DATEDEBUT = as.Date(DATEDEBUT, format="%Y-%m-%d"),
    year = year(DATEDEBUT),
    month = month(DATEDEBUT)
  )



culdata_mainvar <- culdata %>%
  select(NOM_SITE, year, month, NBINDIV, COMMUNELOC, geometry) %>%
  drop_na(year, month)
#unique(culdata_mainvar$year)


culdata_mainvar_2009 <- culdata_mainvar %>%
  filter(year == 2009) %>% # filtering data by year and grouping by site name
  group_by(month,NOM_SITE ) %>%
  summarise(
    count = sum(NBINDIV, na.rm = TRUE))

  
culdata_mainvar_2010 <- culdata_mainvar %>%
  filter(year == 2010) %>%
  group_by(month,NOM_SITE ) %>%
  summarise(
    count = sum(NBINDIV, na.rm = TRUE))



culdata_mainvar_2011 <- culdata_mainvar %>%
  filter(year == 2011) %>%
  group_by(month,NOM_SITE ) %>%
  summarise(
    count = sum(NBINDIV, na.rm = TRUE))


culdata_mainvar_2012 <- culdata_mainvar %>%
  filter(year == 2012) %>%
  group_by(month,NOM_SITE ) %>%
  summarise(
    count = sum(NBINDIV, na.rm = TRUE))

ggplot(culdata_mainvar_2009) +
  geom_sf(color = "black", fill = "blue")

mapview(culdata$COMMUNELOC)
##making maps for each month +look into how to stack them?



#mapview(culdata_mainvar_2009, legend = F, layer.name='culicoides presence 2009')

#class(summary)
#methods(class="tbl_df")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Get France's boundaries and filter for mainland
france <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(iso_a3 == "FRA")  # Filter to include only mainland France
culdata_mainvar_2009 <- st_transform(culdata_mainvar_2009, st_crs(france))

st_crs(culdata_mainvar_2009)
st_crs(france)
# Plot points on the map of mainland France
ggplot() +
  geom_sf(data = france, fill = "lightgreen", color = "black") +  # Plot mainland France's boundary
  geom_sf(data = culdata_mainvar_2009, aes(color = count), size = 2) +  # Plot your points
  scale_color_viridis_c() +  # Optional: Better color scale for counts
  labs(title = "Culicoides Counts in Mainland France (2009)", color = "Count") +
  theme_minimal()
