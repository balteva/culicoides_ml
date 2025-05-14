library(tidyverse)
library(furrr)
library(fst)


setwd("D:/")


one_row <- read_fst("D:/Data_FR/covariates_corrected.fst", from = 1, to = 1)
alt <- read_fst("D:/Data_FR/ALT.fst")
coord <- read_fst("D:/Data_FR/covariates_lon_lat.fst")


coord1 <- alt %>%
  left_join(coord, by="ID_SITE")



### ### ### ### ### ### ### ###
### Variables météorologiques
### ### ### ### ### ### ### ###
predictors_presence <-c("TN_0_0", "EVI_0_0","SWV_0_2", "FG_0_5","ALT", "OVI","BETAIL")


cols <-c("ID_SITE","DATE","ECO_CLI","TP_min_ERA5_wk_mean", "swvl1_mean_mth_mean", "EVI_wk_mean","FG_ERA5_mth_mean", "OVI","BETAIL")

data <- read.fst("D:/Data_FR/covariates_corrected.fst", columns = cols) %>%
  as_tibble() %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE))%>%
  rename(TN_0_0 = TP_min_ERA5_wk_mean,
         SWV_0_2 = swvl1_mean_mth_mean,
         EVI_0_0 = EVI_wk_mean,
         FG_0_5=FG_ERA5_mth_mean)

data_f <- data %>%
  #filter(year<2004)
  #filter(year >= 2004 & year < 2007) 
  #filter(year >= 2007 & year < 2010) 
  #filter(year >= 2010 & year < 2013) 
  #filter(year >= 2013 & year < 2016)
  #filter(year >= 2016 & year < 2019)
  #filter(year >= 2019 & year < 2022) 
  filter(year >= 2022)


data8 <- data_f %>%
  left_join(coord1, by="ID_SITE")


### predicting

presence <- readRDS("D:/REDO/5PARES_presence_model_LTO_tune.rds")

## in case i have missing values, I inpute them:
# library(missRanger)
# new_data <- missRanger(data1, seed = 55)#no missing values

#presence_pred_2004 <- predict(presence, newdata = data1[,predictors_presence])
#presence_pred_2007 <- predict(presence, newdata = data2[,predictors_presence])
#presence_pred_2010 <- predict(presence, newdata = data3[,predictors_presence])
#presence_pred_2013 <- predict(presence, newdata = data4[,predictors_presence])
#presence_pred_2016 <- predict(presence, newdata = data5[,predictors_presence])
#presence_pred_2019 <- predict(presence, newdata = data6[,predictors_presence])
#presence_pred_2022 <- predict(presence, newdata = data7[,predictors_presence])
presence_pred_2025 <- predict(presence, newdata = data8[,predictors_presence])

# data1$prediction <- presence_pred_2004
# saveRDS(data1, file = "data2004_predictions.rds")

# data2$prediction <- presence_pred_2007
# saveRDS(data2, file = "data2007_predictions.rds")

# data3$prediction <- presence_pred_2010
# saveRDS(data3, file = "data2010_predictions.rds")

# data4$prediction <- presence_pred_2013
# saveRDS(data4, file = "data2013_predictions.rds")#finished here
# ##################################################################
# data5$prediction <- presence_pred_2016
# saveRDS(data5, file = "data2016_predictions.rds")

# data6$prediction <- presence_pred_2019
# saveRDS(data6, file = "data2019_predictions.rds")

# data7$prediction <- presence_pred_2022
# saveRDS(data7, file = "data2022_predictions.rds")

data8$prediction <- presence_pred_2025
saveRDS(data8, file = "data2025_predictions.rds")

data1 <- readRDS("data2005_predictions.rds")



precitions2000_2025 <- data1 %>%
  rbind(data2)%>%
  rbind(data3)%>%
  rbind(data4)%>%
  rbind(data5)%>%
  rbind(data6)%>%
  rbind(data7)%>%
  rbind(data8)

saveRDS(precitions2000_2025, file = "data2000_2025_presence_pares_predictions.rds")

