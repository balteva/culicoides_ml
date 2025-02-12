library(tidyverse)
library(fst)
library(lubridate)


##LOADING DATA
dd <- read.fst("./covariates_dict.fst")

culdata<- read.csv("../2025_ieva_culicoides/data/culicoides_df.csv", row.names =NULL)%>%
  select(-X)

meteo <- read.csv("../2025_ieva_culicoides/data/meteo_df.csv") %>%
  select(-CLC,-NBPARE, -NBFEMELLES, -BOV, -EQU, -BETAIL, -OVI, -CAP, -SURF_CANT, -longitude, -latitude)


## Données pièges

df_pieges <- culdata %>%
  mutate(date_releve = as.Date(DATE)) %>%
  mutate(month = month(date_releve), year = year(date_releve)) %>%
  group_by(ECO_CLI,year,month) %>%
  summarise(effectif_jour_mean=mean(NBINDIV, na.rm = T)) %>%
  ungroup() %>%
  mutate(ECO_CLI = as.factor(ECO_CLI)) %>%
  mutate(ECO_CLI= forcats::fct_relevel(ECO_CLI, "Continental", "Mediterranean", "Atlantic" ,"Alpine" )) %>% #changing the order of  ecoclim variables
  mutate(date = as.Date(paste(year, month, "01", sep="-"))) #basically pretending we got these values on the first of each month


ggplot(df_pieges, aes(x =date, color=ECO_CLI)) +
  geom_line(aes(y = effectif_jour_mean), linewidth=0.8) +
  facet_wrap(.~ECO_CLI) +
  labs(x="Year", y="Avg. count/trap/night", color="Eco-climatic zone", title = "Culicoides counts per trap per night in different ecoclimatic zones (France)")+
  theme_bw() + # theme_classic()
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), panel.grid = element_blank())
#ggsave("../../plot1.jpeg", dpi = 300, width = 10, height = 7, units = "in")




colnames(meteo)
## Données météo

# df_meteofrance <-  meteo %>%
#   mutate(date = as.Date(DATE), year = year(DATE), month = month(DATE)) %>%
#   group_by(ECO_CLI,year, month) %>%
#   summarise(Precip_daily = sum(RR_ERA5, na.rm = T), TMN = mean(TP_mean_ERA5, na.rm = T), TMIN = mean( TP_min_ERA5 , na.rm = T), TMAX = mean(TP_max_ERA5, na.rm = T),  HU = mean(HU_mean_ERA5, na.rm = T), QQ_ERA5 = mean(QQ_ERA5, na.rm = T), TPsoil = mean(TPsoil_mean, na.rm = T)) %>%
#   mutate(RFDcum = cumsum(RFD)) %>% #Daily precipitation cumulated  for the year
#   ungroup() %>%
#   mutate_at(4:ncol(.), funs(c(scales::rescale(., to=c(0,1), from = range(., na.rm = TRUE, finite = TRUE))))) %>%
#   mutate(ECO_CLI = as.factor(ECO_CLI)) %>%
#   mutate(ECO_CLI= forcats::fct_relevel(ECO_CLI, "Continental", "Mediterranean", "Atlantic" ,"Alpine" ))



#################messing around

df_meteofrance <-  meteo %>%
  mutate(date = as.Date(DATE), year = year(DATE), month = month(DATE)) %>%
  group_by(ECO_CLI,year, month) %>%
  summarise(
    Precip_daily = sum(RR_ERA5, na.rm = T), Tmean = mean(TP_mean_ERA5, na.rm = T),
    Tmin = mean( TP_min_ERA5 , na.rm = T), Tmax = mean(TP_max_ERA5, na.rm = T), 
    Tmean_dday = mean(TP_mean_ERA5_dday, na.rm = T), Tmean_dwk = mean(TP_mean_ERA5_dwk, na.rm = T),
    Humidity = mean(HU_mean_ERA5, na.rm = T), Radiation = mean(QQ_ERA5, na.rm = T),
    Wind = mean(FG_ERA5, na.rm=T), NDVI = mean(NDVI, na.rm = T),
    TPsoil = mean(TPsoil_mean, na.rm = T), PP=mean(PP, na.rm = T),
    EVI=mean(EVI, na.rm = T), water_in_soill = mean(swvl1_mean, na.rm = T)) %>%
    mutate(Precip_cum = cumsum(Precip_daily)) %>% #Daily precipitation cumulated  for the year
    ungroup() %>%
    mutate_at(4:ncol(.), funs(c(scales::rescale(., to=c(0,1), from = range(., na.rm = TRUE, finite = TRUE))))) %>% #from fourth column i standardise the data
  # same function is this: mutate(across(4:ncol(.), ~ scales::rescale(., to = c(0, 1), from = range(., na.rm = TRUE, finite = TRUE))))
    mutate(ECO_CLI = as.factor(ECO_CLI)) %>%
    mutate(ECO_CLI= forcats::fct_relevel(ECO_CLI, "Continental", "Mediterranean", "Atlantic" ,"Alpine" ))



## all together

df_pieges2 <- df_pieges %>%
  mutate(effectif_jour_mean = scales::rescale(effectif_jour_mean, to=c(0,1), from = range(effectif_jour_mean, na.rm = TRUE, finite = TRUE)))

df2 <- df_meteofrance %>%
  left_join(df_pieges2) %>%
  mutate(date = as.Date(paste(year, month, "01", sep="-"))) %>%
  relocate(c("year","effectif_jour_mean"), .after = month) %>%
  pivot_longer(!c(ECO_CLI,date, month, year)) %>% ##investigate wtf this do
  filter(!is.na(value))

colnames(df2)
# ##########precipitation
# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","Precip_daily")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "Precip_daily" = "steelblue1"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "Monthly Precipitation")) +
#   theme(panel.grid = element_blank())


# ##Daily mean temp.
# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","Tmean")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "Tmean" = "tomato"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "Mean daily temp.")) +
#   theme(panel.grid = element_blank())
# #Min daily temp
# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","Tmin")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "Tmin" = "steelblue1"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "Mean minimum daily temp.")) +
#   theme(panel.grid = element_blank())
# 
# ggsave("../../plot_Tmin_daily.jpeg", dpi = 300, width = 10, height = 7, units = "in")
# 
# #####max daily temp
# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","Tmax")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "Tmax" = "red2"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "Mean maximum daily temp.")) +
#   theme(panel.grid = element_blank())
# ggsave("../../plot_Tmax_daily.jpeg", dpi = 300, width = 10, height = 7, units = "in")
# ############""humidity
# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","Humidity")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "Humidity" = "cornflowerblue"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "Relative humidity")) +
#   theme(panel.grid = element_blank())
# ggsave("../../plot_relative_hum.jpeg", dpi = 300, width = 10, height = 7, units = "in")
# ###water volume in soil
# 
# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","water_in_soill")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "water_in_soill" = "cornflowerblue"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "Mean volume of water in soil")) +
#   theme(panel.grid = element_blank())
# ggsave("../../plot_water_in_soil.jpeg", dpi = 300, width = 10, height = 7, units = "in")


#df2 %>% pull(5) %>% unique()

# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","Radiation")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "Radiation" = "orange3"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "Mean Global Radiation")) +
#   theme(panel.grid = element_blank())
# ggsave("../../plot_global_rad.jpeg", dpi = 300, width = 10, height = 7, units = "in")

# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","Wind")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "Wind" = "black"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "Daily Mean Wind Speed")) +
#   theme(panel.grid = element_blank())
# ggsave("../../plot_windspeed.jpeg", dpi = 300, width = 10, height = 7, units = "in")

# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","Precip_cum")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "Precip_cum" = "navy"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "Yearly cumulated precipitation")) +
#   theme(panel.grid = element_blank())
# ggsave("../../plot_cumprecipitation.jpeg", dpi = 300, width = 10, height = 7, units = "in")

# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","TPsoil")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "TPsoil" = "brown"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "Mean Soil Temperature")) +
#   theme(panel.grid = element_blank())
# ggsave("../../plot_TPsoil.jpeg", dpi = 300, width = 10, height = 7, units = "in")

# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","PP")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "PP" = "purple"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "Photoperiod")) +
#   theme(panel.grid = element_blank())
# ggsave("../../plot_Photoperiod.jpeg", dpi = 300, width = 10, height = 7, units = "in")

# ggplot() +
#   geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","NDVI")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
#   facet_wrap(.~ECO_CLI) +
#   theme_bw() +
#   labs(x="Year", y="Normalized values", color="Metric") +
#   scale_color_manual(values = c("effectif_jour_mean" = "grey58", "NDVI" = "darkgreen"),  # Change legend labels
#                      labels = c("indiv./trap/night (average)", "mean NDVI")) +
#   theme(panel.grid = element_blank())
# ggsave("../../plot_NDVI.jpeg", dpi = 300, width = 10, height = 7, units = "in")

ggplot() +
  geom_line(data = df2 %>% filter(name %in% c("effectif_jour_mean","EVI")), aes(x = date, y = value, group = as.factor(name), color = as.factor(name)), linewidth=0.6) +
  facet_wrap(.~ECO_CLI) +
  theme_bw() +
  labs(x="Year", y="Normalized values", color="Metric") +
  scale_color_manual(values = c("effectif_jour_mean" = "grey58", "EVI" = "forestgreen"),  # Change legend labels
                     labels = c("indiv./trap/night (average)", "mean EVI")) +
  theme(panel.grid = element_blank())
ggsave("../../plot_EVI.jpeg", dpi = 300, width = 10, height = 7, units = "in")


