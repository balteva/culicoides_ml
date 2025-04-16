library(tidyverse)
library(furrr)
library(fst)
one_row <- read_fst("E:/Data_FR/covariates_corrected.fst", from = 1, to = 1)
alt <- read_fst("./ieva/ALT.fst")
coord <- read_fst("./ieva/covariates_lon_lat.fst")


coord1 <- alt %>%
  left_join(coord, by="ID_SITE")



### ### ### ### ### ### ### ###
### Variables météorologiques
### ### ### ### ### ### ### ###
#predictors_presence <-c("TN_0_0", "SWV_2_5","FG_4_6", "NDVI_0_1","ALT", "OVI","BETAIL")


#summary(data1[,predictors_presence])
predictors_presence <-c("TX_0_4", "SWV_0_4", "NDVI_0_0","ALT", "OVI","BETAIL")
predictors_abundance <-c("TX_0_0","SWV_0_3","FG_0_1","NDVI_0_5", "ALT")

cols <-c("ID_SITE","DATE","ECO_CLI","TP_max_ERA5_mth_mean","TP_max_ERA5_wk_mean", "swvl1_mean_mth_mean", "NDVI_wk_mean","NDVI_mth_mean","FG_ERA5_wk_mean", "OVI","BETAIL")

data <- read.fst("E:/Data_FR/covariates_corrected.fst", columns = cols) %>%
  as_tibble() %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE))%>%
  rename(TX_0_4 = TP_max_ERA5_mth_mean,
         TX_0_0 = TP_max_ERA5_wk_mean,
         SWV = swvl1_mean_mth_mean,
         NDVI_0_0 = NDVI_wk_mean,
         NDVI_0_5 = NDVI_mth_mean,
         FG_0_1 = FG_ERA5_wk_mean)
         




data_new <- data %>%
  left_join(coord1, by="ID_SITE")
write_fst(data_new, "E:/ieva/filtered_meteo_2000_2024.fst")


