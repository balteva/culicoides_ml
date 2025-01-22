install.packages("fst")
install.packages("datadictionary")
library(fst)
library(datadictionary)
library(tidyverse)
library(lubridate)
library(sf)
library(gridExtra)
dd <- read.fst("covariates_dict.fst")

data_na <- read.fst("covariates.fst")

NBINDIV_na <- is.na(data_na$NBINDIV)
data <- subset(data_na, !NBINDIV_na)
##just testing something




df_short <- as.data.frame(data) %>%
  select(ID_SITE, DATE, NBINDIV, longitude, latitude) %>%
  distinct(ID_SITE, DATE, .keep_all = TRUE) %>%
  mutate(DATE = as.Date(DATE),
  year = year(DATE),
  month = month(DATE))

#df_short_sf <- df_short %>%
#  st_as_sf(coords = c("longitude", "latitude"), crs=4326)  #st_as_sf is convert st to sf object


# counts across all sites will be summarised by month, this will show in the entirety of france monthly counts
summary2009 <- df_short_sf %>%
  filter(year == 2009) %>%  
  group_by(ID_SITE) %>%
  summarize(total_NBINDIV = sum(NBINDIV), .groups = "drop", geometry = first(geometry)) 
#  st_as_sf()


summary2010 <- df_short_sf %>%
  filter(year == 2010) %>%  
  group_by(ID_SITE) %>%
  summarize(total_NBINDIV = sum(NBINDIV), .groups = "drop", geometry = first(geometry)) 


summary2011 <- df_short_sf %>%
  filter(year == 2011) %>%  
  group_by(ID_SITE) %>%
  summarize(total_NBINDIV = sum(NBINDIV), .groups = "drop", geometry = first(geometry)) 


summary2012 <- df_short_sf %>%
  filter(year == 2012) %>%  
  group_by(ID_SITE) %>%
  summarize(total_NBINDIV = sum(NBINDIV), .groups = "drop", geometry = first(geometry)) 


pdf("C:/Users/ibalt/OneDrive/Desktop/his_nbindiv_grouped_by_site.pdf", width = 15, height = 13)


par(mfrow = c(2, 2))
hist(summary2009$total_NBINDIV, 
     main = "Histogram of Culicoides counts (2009)", 
     xlab = "across different sites", 
     ylab = "Frequency",
     col = "orange")

hist(summary2010$total_NBINDIV, 
     main = "Histogram of Culicoides counts (2010)", 
     xlab = "across different sites", 
     ylab = "Frequency",
     col = "blue")

hist(summary2011$total_NBINDIV, 
     main = "Histogram of Culicoides counts (2011)", 
     xlab = "across different sites", 
     ylab = "Frequency",
     col = "red")

hist(summary2012$total_NBINDIV, 
     main = "Histogram of Culicoides counts (2012)", 
     xlab = "across different sites", 
     ylab = "Frequency",
     col = "violet")
dev.off()
####
df_short <- as.data.frame(data) %>%
  select(ID_SITE, DATE, NBINDIV, longitude, latitude) %>%
  distinct(ID_SITE, DATE, .keep_all = TRUE) %>%
  mutate(DATE = as.Date(DATE),
         year = year(DATE),
         month = month(DATE))

#df_short_sf <- df_short %>%
#  st_as_sf(coords = c("longitude", "latitude"), crs=4326)  #st_as_sf is convert st to sf object

temporal <- df_short %>%
  group_by(year, month) %>%
  summarize(total_NBINDIV = sum(NBINDIV),
  DATE = first(DATE),.groups = "drop")


temporal2 <- df_short %>%
  group_by(year, month) %>%
  summarize(total_NBINDIV = mean(NBINDIV),
            DATE = first(DATE),.groups = "drop")

library(ggplot2)
pdf("C:/Users/ibalt/OneDrive/Desktop/temporal_changes.pdf", width = 18, height = 5)


temporalplot <- ggplot(temporal, aes(x=DATE, y=total_NBINDIV)) +
  geom_line() +
  labs(title = "Culicoides population fluctuations 2009-2012 in France",
       x = "Timeline",
       y = "Total number across all sites") +
  theme_minimal()



temporalplot2 <- ggplot(temporal2, aes(x=DATE, y=total_NBINDIV)) +
  geom_line() +
  labs(title = "Culicoides population fluctuations 2009-2012 in France",
       x = "Timeline",
       y = "mean number across all sites") +
  theme_minimal()


grid.arrange(temporalplot, temporalplot2, ncol = 2, nrow = 1)
dev.off()

