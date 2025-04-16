library(tidyverse)
library(furrr)
library(fst)
setwd("D:/ieva")

one_row <- read_fst("D:/Data_FR/covariates_corrected.fst", from = 1, to = 1)
alt <- read_fst("./ALT.fst")
coord <- read_fst("./covariates_lon_lat.fst")


coord1 <- alt %>%
  left_join(coord, by="ID_SITE")



#predictors_presence <-c("TX_0_4", "SWV_0_4", "NDVI_0_0","ALT", "OVI","BETAIL")
predictors_abundance <- c("TX_0_0","SWV_0_3","FG_0_1","NDVI_0_5", "ALT")

cols <-c("ID_SITE","DATE","ECO_CLI","TP_max_ERA5_wk_mean", "swvl1_mean_mth_mean", "NDVI_mth_mean", "FG_ERA5_wk_mean")

data <- read.fst("D:/Data_FR/covariates_corrected.fst", columns = cols) %>%
  as_tibble() %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE))%>%
  #filter(year<2004)%>%
  #filter(year >= 2004 & year < 2008) %>%
  #filter(year >= 2008 & year < 2012) %>%
  #filter(year >= 2012 & year < 2016) %>%
  #filter(year >= 2016 & year < 2020) %>%
 # filter(year >= 2020 & year < 2024) %>%
  filter(year >= 2024) %>%
  rename(TX_0_0 = TP_max_ERA5_wk_mean,
          SWV_0_3 = swvl1_mean_mth_mean,
         NDVI_0_5 = NDVI_mth_mean,
         FG_0_1 = FG_ERA5_wk_mean)




data7 <- data %>%
  left_join(coord1, by="ID_SITE")


### predicting

abundance <- readRDS("C:/Users/paloo/OneDrive/Bureau/final_models/LLTO_abundance_predictive/full_model_abundance_LTO_tune.rds")

## in case i have missing values, I inpute them:
 # library(missRanger)
 # new_data <- missRanger(data1, seed = 55)#no missing values


#abundance_pred_2004 <- predict(abundance, newdata = data1[,predictors_abundance])
abundance_pred_2025 <- predict(abundance, newdata = data7[,predictors_abundance])





# data1$prediction <- abundance_pred_2004
# saveRDS(data1, file = "data2004_abundance_predictions.rds")

# data2$prediction <- abundance_pred_2008
# saveRDS(data2, file = "data2008_abundance_predictions.rds")


# data3$prediction <- abundance_pred_2012
# saveRDS(data3, file = "data2012_abundance_predictions.rds")

# data4$prediction <- abundance_pred_2016
# saveRDS(data4, file = "data2016_abundance_predictions.rds")

# data5$prediction <- abundance_pred_2020
# saveRDS(data5, file = "data2020_abundance_predictions.rds")

# data6$prediction <- abundance_pred_2024
# saveRDS(data6, file = "data2024_abundance_predictions.rds")

data7$prediction <- abundance_pred_2025
saveRDS(data7, file = "data2025_abundance_predictions.rds")


data1 <- readRDS("data2004_abundance_predictions.rds")
data2 <- readRDS("data2008_abundance_predictions.rds")
data3 <- readRDS("data2012_abundance_predictions.rds")
data4 <- readRDS("data2016_abundance_predictions.rds")
data5 <- readRDS("data2020_abundance_predictions.rds")





predictions_abundance_2000_2025 <- data1 %>%
  rbind(data2)%>%
  rbind(data3)%>%
  rbind(data4)%>%
  rbind(data5)%>%
  rbind(data6)%>%
  rbind(data7)
saveRDS(predictions_abundance_2000_2025, file = "data2000_2025_abundance_predictions.rds")

model_abu <- readRDS("./models/LLTO_abundance_predictive/reduced_abundance_LLO.rds")
