library(tidyverse)
library(furrr)
library(fst)



setwd("D:/")
abundance <- readRDS("D:/REDO/1NBINDIV_abundance_model_LTO_tune.rds")

#one_row <- read_fst("D:/Data_FR/covariates_corrected.fst", from = 1, to = 1)
alt <- read_fst("D:/Data_FR/ALT.fst")
coord <- read_fst("D:/Data_FR/covariates_lon_lat.fst")


coord1 <- alt %>%
  left_join(coord, by="ID_SITE")


predictors_abundance <- c("TX_0_0","SWV_0_2","FG_0_5","EVI_0_5", "ALT", "OVI", "BETAIL")

cols <-c("ID_SITE","DATE","ECO_CLI","TP_max_ERA5_wk_mean", "swvl1_mean_mth_mean", "EVI_mth_mean", "FG_ERA5_mth_mean", "BETAIL", "OVI")

data <- read_fst("D:/Data_FR/covariates_corrected.fst", columns = cols) %>%
  as_tibble() %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE))%>%
  rename(TX_0_0 = TP_max_ERA5_wk_mean,
        SWV_0_2 = swvl1_mean_mth_mean,
        EVI_0_5 = EVI_mth_mean,
        FG_0_5 = FG_ERA5_mth_mean)



data_f <- data %>%
  # filter(year<2004)
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



## in case i have missing values, I inpute them:
# library(missRanger)
# new_data <- missRanger(data1, seed = 55)#no missing values



#abundance_pred_2004 <- predict(abundance, newdata = data1[,predictors_abundance])
#abundance_pred_2007 <- predict(abundance, newdata = data2[,predictors_abundance])
#abundance_pred_2010 <- predict(abundance, newdata = data3[,predictors_abundance])
#abundance_pred_2013 <- predict(abundance, newdata = data4[,predictors_abundance])
#abundance_pred_2016 <- predict(abundance, newdata = data5[,predictors_abundance])
#abundance_pred_2019 <- predict(abundance, newdata = data6[,predictors_abundance])
#abundance_pred_2022 <- predict(abundance, newdata = data7[,predictors_abundance])
abundance_pred_2025 <- predict(abundance, newdata = data8[,predictors_abundance])

# data1$prediction <- abundance_pred_2004
# saveRDS(data1, file = "data2004_abundance_predictions_tx.rds")

# data2$prediction <- abundance_pred_2007
# saveRDS(data2, file = "data2007_abundance_predictions_tx.rds")


# data3$prediction <- abundance_pred_2010
# saveRDS(data3, file = "data2010_abundance_predictions_tx.rds")##this is where i finished

# data4$prediction <- abundance_pred_2013
# saveRDS(data4, file = "data2013_abundance_predictions_tx.rds")

# data5$prediction <- abundance_pred_2016
# saveRDS(data5, file = "data2016_abundance_predictions_tx.rds")

# data6$prediction <- abundance_pred_2019
# saveRDS(data6, file = "data2019_abundance_predictions_tx.rds")

# data7$prediction <- abundance_pred_2022
# saveRDS(data7, file = "data2022_abundance_predictions_tx.rds")

data8$prediction <- abundance_pred_2025
saveRDS(data8, file = "data2025_abundance_predictions_tx.rds")

data1 <- readRDS("data2004_abundance_predictions_tx.rds")
data2 <- readRDS("data2007_abundance_predictions_tx.rds")
data3 <- readRDS("data2010_abundance_predictions_tx.rds")
data4 <- readRDS("data2013_abundance_predictions_tx.rds")
data5 <- readRDS("data2016_abundance_predictions_tx.rds")
data6 <- readRDS("data2019_abundance_predictions_tx.rds")
data7 <- readRDS("data2022_abundance_predictions_tx.rds")
data8 <- readRDS("data2025_abundance_predictions_tx.rds")




predictions_abundance_2000_2025 <- data1 %>%
  rbind(data2)%>%
  rbind(data3)%>%
  rbind(data4)%>%
  rbind(data5)%>%
  rbind(data6)%>%
  rbind(data7)%>%
  rbind(data8)

saveRDS(predictions_abundance_2000_2025, file = "data2000_2025_abundance_predictions_tx.rds")

