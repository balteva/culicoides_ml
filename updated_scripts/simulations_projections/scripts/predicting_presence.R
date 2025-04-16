library(tidyverse)
library(furrr)
library(fst)
one_row <- read_fst("E:/Data_FR/covariates_corrected.fst", from = 1, to = 1)
alt <- read_fst("./ALT.fst")
coord <- read_fst("./covariates_lon_lat.fst")


coord1 <- alt %>%
  left_join(coord, by="ID_SITE")



### ### ### ### ### ### ### ###
### Variables météorologiques
### ### ### ### ### ### ### ###
#predictors_presence <-c("TN_0_0", "SWV_2_5","FG_4_6", "NDVI_0_1","ALT", "OVI","BETAIL")


#summary(data1[,predictors_presence])
predictors_presence <-c("TX_0_4", "SWV_0_4", "NDVI_0_0","ALT", "OVI","BETAIL")
                      c("TX_0_0","SWV_0_3","FG_0_1","NDVI_0_5", "ALT")
cols <-c("ID_SITE","DATE","ECO_CLI","TP_max_ERA5_mth_mean", "swvl1_mean_mth_mean", "NDVI_wk_mean", "OVI","BETAIL")

data <- read.fst("D:/Data_FR/covariates_corrected.fst", columns = cols) %>%
  as_tibble() %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE))%>%
  #filter(year<2005)%>%
  #filter(year >= 2005 & year < 2009) %>%
  #filter(year >= 2009 & year < 2013) %>%
  #filter(year >= 2013 & year < 2017) %>%
  #filter(year >= 2017 & year < 2021) %>%
  filter(year >= 2021 & year < 2025) %>%
  rename(TX_0_4 = TP_max_ERA5_mth_mean,
          SWV_0_4 = swvl1_mean_mth_mean,
         NDVI_0_0 = NDVI_wk_mean)




data6 <- data %>%
  left_join(coord1, by="ID_SITE")


### predicting

presence <- readRDS("./models/full_model_tuned.rds" )

## in case i have missing values, I inpute them:
# library(missRanger)
# new_data <- missRanger(data1, seed = 55)#no missing values

#presence_pred_2009 <- predict(presence, newdata = data2[,predictors_presence])
#presence_pred_2013 <- predict(presence, newdata = data3[,predictors_presence])
#presence_pred_2017 <- predict(presence, newdata = data4[,predictors_presence])
#presence_pred_2021 <- predict(presence, newdata = data5[,predictors_presence])

presence_pred_2025 <- predict(presence, newdata = data6[,predictors_presence])

# data1$prediction <- presence_pred_2005
# saveRDS(data1, file = "data2005_predictions.rds")

# data2$prediction <- presence_pred_2009
# saveRDS(data2, file = "data2009_predictions.rds")

# data3$prediction <- presence_pred_2013
# saveRDS(data3, file = "data2013_predictions.rds")

# data4$prediction <- presence_pred_2017
# saveRDS(data4, file = "data2017_predictions.rds")

# data5$prediction <- presence_pred_2021
# saveRDS(data5, file = "data2021_predictions.rds")

data6$prediction <- presence_pred_2025
saveRDS(data6, file = "data2021_predictions.rds")



data1 <- readRDS("data2005_predictions.rds")



precitions2000_2025 <- data1 %>%
  rbind(data2)%>%
  rbind(data3)%>%
  rbind(data4)%>%
  rbind(data5)%>%
  rbind(data6)
saveRDS(precitions2000_2025, file = "data2000_2025_predictions.rds")

