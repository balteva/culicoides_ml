

error_LTO <- df_cv_presence_LTO %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE),week=lubridate::week(DATE))%>%
  dplyr::group_by(Cell,year,week) %>% #so my mean is gonna be for each cell per week across the study period
  summarise(cell_pred = mean(pred), cell_obs = mean(obs))%>% #calculating the mean of the prediction and observation for each cell per week
  group_by(Cell, year) %>%
  mutate(first_week_obs = ifelse (any (cell_obs > 0.5), min(week[cell_obs > 0.5], na.rm=TRUE), NA), #if in a given cell any of the obs are presence, we take the minimum week for that cell group, otherwise NA
         first_week_pred = ifelse (any (cell_pred > 0.5), min(week[cell_pred > 0.5], na.rm = TRUE), NA)) %>%
  mutate(error_weeks = first_week_pred - first_week_obs)


## If >0, model predicts too late; if <0, too early
mean_error_pred_LTO <- error_LTO %>%
  group_by(year)%>%
  summarise(mean_error_weeks = mean(error_weeks, na.rm=TRUE))


#This here should be per ECO_CLI
error_LTO <- df_cv_presence_LTO %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE),week=lubridate::week(DATE))%>%
  dplyr::group_by(ECO_CLI, ID_SITE,year,week) %>% #so my mean is gonna be for each cell per week across the study period
  summarise(idsite_pred = mean(pred), idsite_obs = mean(obs))%>% #this shouldnt change anything 
  group_by(ECO_CLI, year) %>%
  mutate(first_week_obs = ifelse (any (idsite_obs > 0.5), min(week[idsite_obs > 0.5], na.rm=TRUE), NA), #if in a given cell any of the obs are presence, we take the minimum week for that cell group, otherwise NA
         first_week_pred = ifelse (any (idsite_pred > 0.5), min(week[idsite_pred > 0.5], na.rm = TRUE), NA)) %>%
  mutate(error_weeks = first_week_pred - first_week_obs)


## If >0, model predicts too late; if <0, too early
mean_error_pred_LTO <- error_LTO %>%
  group_by(ECO_CLI)%>% #or ECO_CLI + year
  summarise(mean_error_weeks = mean(error_weeks, na.rm=TRUE))