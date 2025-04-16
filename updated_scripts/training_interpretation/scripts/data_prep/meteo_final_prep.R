library(tidyverse)
library(furrr)
library(fst)
one_row <- read_fst("E:/Data_FR/covariates_corrected.fst", from = 1, to = 5)


### ### ### ### ### ### ### ###
### Variables météorologiques
### ### ### ### ### ### ### ###
#predictors_presence <-c("TN_0_0", "SWV_2_5","FG_4_6", "NDVI_0_1","ALT", "OVI","BETAIL")



cols <- c("ID_SITE", "DATE", "OVI", "BETAIL", 'TP_min_ERA5', "TP_max_ERA5", "swvl1_mean", "NDVI_wk_mean", "FG_ERA5") # "BETAIL", "OVI" and "ALT"

meteo <- read.fst("E:/Data_FR/covariates_corrected.fst", columns = c("ID_SITE", "longitude", "latitude"), from = 1, to=54950870,  as.data.table=T) 



# %>%
#   as_tibble() %>%
#   mutate(DATE=as.Date(DATE))%>%
#   mutate(year=lubridate::year(DATE))%>%
#   filter(year<=2002)%>%
#   rename(TN = TP_min_ERA5,
#          SWV = swvl1_mean,
#          FG = FG_ERA5,
#          date= DATE)%>%
#   mutate(date = as.character(date))




meteo_fix <- meteo %>%
  mutate(NDVI = case_when(
    NDVI <= -1 ~ -NA,TRUE ~ NDVI))%>%
  mutate(num_releve = seq(1,nrow(.),1)) %>%
  mutate(idpointdecapture = paste(ID_SITE,num_releve, sep="_"))%>%
  mutate(date_releve=as.Date(date))





lag_max <- 45 # (6 semaines avant la capture)


#### Functions

fun_summarize_week <- function(df_meteo_pieges2,var_to_summarize,fun_summarize,new_var_name,n_days_agg){
  
  if(fun_summarize=="sum"){###this aggregates selected meteo variale over a selected time period (gives sum values within each period)
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture, lag_n = lubridate::week(date), year = lubridate::year(date)) %>% #not including it means summarisation was done together across all years
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      dplyr::summarise(
        val= if(all(is.na(val))) NA_real_ else sum(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>% #lag is recurcive (going backwards in time)
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  } else if (fun_summarize == "mean"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      summarise(
        val = if (all(is.na(val))) NA_real_ else mean(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }  else if (fun_summarize == "max"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      summarise(
        val = if (all(is.na(val))) NA_real_ else max(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }  else if (fun_summarize == "min"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      summarise(
        val = if (all(is.na(val))) NA_real_ else min(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }
  return(df_meteo_pieges2_summarize)
  
}
#end of func
pieges_data2 <- meteo_fix %>%
  dplyr::select(idpointdecapture, num_releve, date_releve, ID_SITE, date, ECO_CLI) 

# Fonction pour créer les lignes avec les différentes valeurs de lag
create_rows <- function(i) {
  map_df(0:lag_max, ~data.frame(
    idpointdecapture = pieges_data2$idpointdecapture[i],
    num_releve = pieges_data2$num_releve[i],
    ID_SITE = pieges_data2$ID_SITE[i],
    ECO_CLI = pieges_data2$ECO_CLI[i],
    date = as.character(as.Date(pieges_data2$date_releve[i] - .x)),
    lag_n = .x,
    stringsAsFactors = FALSE
  ))
}
#end of func 2
# Appliquer la fonction à toutes les lignes de pieges_data
df_meteo_pieges <- furrr::future_map_dfr(1:nrow(pieges_data2), create_rows)



# summarizing to weeks
df_meteo_pieges2 <- df_meteo_pieges %>%
  left_join(meteo_fix, by = c("date","ECO_CLI","ID_SITE")) %>%
  pivot_longer(!(idpointdecapture:lag_n), names_to = "var", values_to = 'val')

df_meteo_pieges_summ <- fun_summarize_week(df_meteo_pieges2,"TN","min","TN",7) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"SWV","mean","SWV",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"FG","mean","FG",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"NDVI","mean","NDVI",7))


## je ne sais pas pourquoi cela va jusque 8 parfois, mais on enleve pour avoir 7 (0+7, donc 8 en tout) semaines
df_meteo_pieges_summ <- df_meteo_pieges_summ %>% filter(lag_n<7)

# function to create the data.frame for CCM
fun_ccm_df <- function(df_timeseries, varr, function_to_apply){
  
  df_timeseries_wide <- df_timeseries %>%
    filter(var==varr) %>%
    dplyr::select(-c("date","var")) %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n, names_prefix = paste0(varr,"_"))
  
  max_col <- ncol(df_timeseries_wide)
  
  for(i in 2:(max_col-1)){
    for(j in (i+1):max_col){
      column_name <- paste0(colnames(df_timeseries_wide[i]),"_",(j-2))
      if(function_to_apply=="mean"){
        df_timeseries_wide[column_name] <- rowMeans(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="sum"){
        df_timeseries_wide[column_name] <- rowSums(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="max"){
        df_timeseries_wide[column_name] <- max(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="min"){
        df_timeseries_wide[column_name] <- min(df_timeseries_wide[,i:j], na.rm = T)
      }
    }
  }
  
  for(i in 2:max_col){
    colnames(df_timeseries_wide)[i] <- paste0(colnames(df_timeseries_wide)[i],"_",sub('.*\\_', '', colnames(df_timeseries_wide)[i]))
  }
  
  return(df_timeseries_wide)
  
}




df_meteo_pieges_summ_wide4 <- fun_ccm_df(df_meteo_pieges_summ,"TN","mean")
df_meteo_pieges_summ_wide5 <- fun_ccm_df(df_meteo_pieges_summ,"TX","mean")
df_meteo_pieges_summ_wide6 <- fun_ccm_df(df_meteo_pieges_summ,"TM","mean")
df_meteo_pieges_summ_wide7 <- fun_ccm_df(df_meteo_pieges_summ,"TPS","mean")
df_meteo_pieges_summ_wide8 <- fun_ccm_df(df_meteo_pieges_summ,"QQ","mean")
df_meteo_pieges_summ_wide9 <- fun_ccm_df(df_meteo_pieges_summ,"SWV","mean")
df_meteo_pieges_summ_wide10 <- fun_ccm_df(df_meteo_pieges_summ,"UM","mean")
df_meteo_pieges_summ_wide11 <- fun_ccm_df(df_meteo_pieges_summ,"PP","mean")
df_meteo_pieges_summ_wide12 <- fun_ccm_df(df_meteo_pieges_summ,"FG","mean")
df_meteo_pieges_summ_wide13 <- fun_ccm_df(df_meteo_pieges_summ,"NDVI","mean")
df_meteo_pieges_summ_wide14 <- fun_ccm_df(df_meteo_pieges_summ,"EVI","mean")




df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2) %>%
  left_join(df_meteo_pieges_summ_wide4) %>%
  left_join(df_meteo_pieges_summ_wide5) %>%
  left_join(df_meteo_pieges_summ_wide6) %>%
  left_join(df_meteo_pieges_summ_wide7) %>%
  left_join(df_meteo_pieges_summ_wide8) %>%
  left_join(df_meteo_pieges_summ_wide9) %>%
  left_join(df_meteo_pieges_summ_wide10) %>%
  left_join(df_meteo_pieges_summ_wide11) %>%
  left_join(df_meteo_pieges_summ_wide12) %>%
  left_join(df_meteo_pieges_summ_wide13) %>%
  left_join(df_meteo_pieges_summ_wide14)


# à l'échelle du piege/nuit de capture
df_to_model <- pieges_data %>%
  left_join(df_meteo_pieges_summ_wide_meteofrance) #%>%
#dplyr::select(-c("num_releve","idpointdecapture"))


# à l'échelle de la zone ecoclimatique-semaine de collecte :
# df_to_model_grouped <- df_to_model %>%
#   mutate(week = week(date), Year = year(date)) %>%
#   relocate(NBINDIV,.before = RR_0_0) %>%
#   group_by(ECO_CLI, Year,week) %>%
#   summarise_at(vars(NBINDIV:EVI_3_4), mean, na.rm = TRUE)


write.csv(df_to_model,file.path("../../updated_scripts/data","df_to_model_obscot.csv"), row.names = F)
#write.csv(df_to_model_grouped,file.path("data","df_to_model_grouped.csv"), row.names = F)



