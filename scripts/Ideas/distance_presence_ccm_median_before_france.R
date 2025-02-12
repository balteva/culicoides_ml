library(tidyverse)
library(furrr)
library(patchwork)
library(correlation)

# script 4  presence only (with median only for NBINDIV)
df_model <- read.csv(file.path("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/data/df_to_model_before.csv")) %>%
  select(-peak_date)%>%
  group_by( ECO_CLI, date) %>% 
  summarise(NBINDIV = median(effectif_jour_mean, na.rm = TRUE),
            across(RR_0_0:EVI_2_3, mean, na.rm = TRUE)) # mean for climatic variables

df_model <-  df_model %>%
  mutate(presence_culi = ifelse(NBINDIV<1,0,1)) %>% #since using median we can filter for less than 1 as absence
  relocate(NBINDIV, presence_culi, .after = date) %>% ##edited this spart
  filter(!is.na(presence_culi)) %>%
  as_tibble()

### Adding france entiere to  eco_clim column

df_model_allECO_CLIs <- df_model %>%
  mutate(ECO_CLI = "FRANCE")
df_model <- bind_rows(df_model,df_model_allECO_CLIs)

#function to include presence or absence indicator
fun_compute_correlation_univ <- function(df,indicator){
  
  if(indicator == "presence"){
    var_to_keep = "presence_culi"}
  
  #SPEARMANS CORRELATION function
  func <- function(x){
    df2 <- df %>% dplyr::select(var_to_keep,!!x)  #,ID_SITE
    ret <- correlation::correlation(df2,method = "distance", )##using distance correlation method   # multileve = TRUE
    return(ret)
  }
  
  possible_a <- possibly(func, otherwise = NA_real_)
  spearman_univs <- furrr::future_map(colnames(df[5:ncol(df)]), possible_a)
  spearman_univs <- do.call(rbind.data.frame, spearman_univs)
  spearman_univs$ECO_CLI <- unique(df$ECO_CLI)
  return(spearman_univs)
}


## presence/absence
corr_univ_presence <- df_model %>%
  group_split(ECO_CLI) %>%
  map_dfr(.,~fun_compute_correlation_univ(., "presence")) %>%
  as.tibble() %>%
  mutate(indicator = "presence")%>%
  mutate(r = ifelse(r<0,0,r))


################
## plotting (simple distance CCM)
##############

fun_ccm_plot2 <- function(correlation_df, var){
  
  if(length(unique(correlation_df$correlation))!=1){ # to deal with case all correlation values are NAs
    most_corr <- correlation_df %>% filter(correlation == max(correlation, na.rm = T))
    most_corr2 <- correlation_df %>% arrange(desc(correlation)) %>% filter(correlation >= most_corr$correlation * 0.9)
  } else {
    most_corr <- most_corr2 <- correlation_df[1,]
  }
  
  ccm_plot <- ggplot(data = correlation_df, aes(time_lag_1, time_lag_2, fill = correlation)) +
    geom_tile(color = "white", show.legend = TRUE, size = 0.05,aes(width=1, height=1)) +
    geom_tile(data = most_corr2 , color = "black", size = 0.2, show.legend = FALSE,aes(width=1, height=1)) +  # ,aes(width=1, height=1)
    geom_tile(data = most_corr , color = "deeppink3", size = 0.6, show.legend = FALSE,aes(width=1, height=1)) +  # ,aes(width=1, height=1)
    theme_minimal() +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          axis.title = element_text(size = 8),
          legend.key.size = unit(0.8, "cm"),
          legend.title=element_text(size=10),
          legend.position = "none"
    ) +
    ggtitle(var) +
    annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("r(",most_corr$time_lag_2,",",most_corr$time_lag_1,") = ",round(most_corr$correlation,2))) +
    coord_fixed() +
    ylab("time lag 1") +
    xlab("time lag 2") +
    scale_fill_gradient2(low = "white", high = "red", limit = c(0,1), space = "Lab", name = "Distance correlation", na.value = "grey")
  
  
  return(ccm_plot)
  
}





univ_spearman_temporal_mf <- corr_univ_presence %>%
  filter(!is.na(Parameter1),!is.na(Parameter2)) %>%
  mutate(var = sub('\\_.*', '', Parameter2)) %>%
  mutate(label = case_when(var == "RR" ~ "Rainfall",
                           var == "RRMAX" ~ "Maximum rainfall ",
                           var == "TN" ~ "Minimum temperature",
                           var == "TX" ~ "Maximum temperature",
                           var == "TM" ~ "Average temperature",
                           var == "UM" ~ "Relative humidity",
                           var == "TPS" ~ "Soil Temperature",
                           var == "QQ" ~ "Solar radiation",
                           var == "SWV" ~ "Water volume soil",
                           var == "PP" ~ "Photoperiod",
                           var == "FG" ~ "Wind speed",
                           var == "NDVI" ~ "NDVI",
                           var == "EVI" ~ "EVI")) %>%
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', Parameter2)))%>% 
  mutate(time_lag_2 = as.numeric(stringr::str_match(Parameter2, '([^_]+)(?:_[^_]+){1}$')[,2])) %>%
  arrange(ECO_CLI,var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = r) %>%
  mutate(correlation = ifelse(p<=0.2,correlation,NA)) %>%
  mutate(time_lag_1 = as.numeric(time_lag_1)+1, #changing display of time lags to start from 0 to 1
         time_lag_2 = as.numeric(time_lag_2)+1) %>%
  nest(-c(ECO_CLI,indicator,var))



plots_univ_spearman_temporal_mf <- univ_spearman_temporal_mf %>%
  arrange(rev(indicator),factor(var, levels = c("TM","TN","TX","UM","RR","RRMAX","TPS","QQ","SWV","PP","FG","NDVI","EVI")),factor(ECO_CLI, levels = c("FRANCE", "Continental", "Mediterranean" ,"Atlantic","Alpine"))) %>%
  mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot2(correlation_df = ..1, var = ..1$label[1]))) %>%
  nest(-c(ECO_CLI,indicator)) %>%
  mutate(univ_temporal = map(data, ~patchwork::wrap_plots(.x$univ_temporal, nrow = 1, ncol = 13))) %>%
  mutate(univ_temporal = pmap(list(univ_temporal,ECO_CLI), ~..1 + patchwork::plot_annotation(title = ..2))) %>%
  dplyr::select(-data)


#"FRANCE", "Continental", "Mediterranean" ,"Atlantic","Alpine"

p_meteo_presence_before <- patchwork::wrap_plots(plots_univ_spearman_temporal_mf$univ_temporal[1:5], ncol = 1, nrow = 5)

