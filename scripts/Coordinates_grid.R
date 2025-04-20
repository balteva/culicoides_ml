
library(magrittr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)

# Télécharger les données géospatiales pour la france
france <- ne_states(geounit = 'france') %>% st_transform(., 4326)

# Extraire la bbox pour la France
bbox_france <- st_bbox(france)

cr = rast(ncols=23, nrows=43, 
          nlyrs=1, 
          xmin=bbox_france$xmin, xmax=bbox_france$xmax, 
          ymin=bbox_france$ymin, ymax=bbox_france$ymax, 
          names=c('rastFr'), crs='EPSG:4326')

res(cr) <- 0.1

cr$rastFr <- seq(ncell(cr))

cr <- mask(cr, france)

coords <- crds(cr, df=FALSE, na.rm=TRUE, na.all=FALSE)

data.frame(ID_SITE = seq(nrow(coords)), coords)

spatial_points <- SpatialPointsDataFrame(coords, 
                                         data.frame(ID_SITE = seq(nrow(coords))))

crs(spatial_points) <- 'EPSG:4326'

plot(spatial_points)
