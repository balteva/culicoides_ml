library(tidyverse)
library(furrr)
library(patchwork)
library(correlation)
library(ggplot2)



# open dataset containing the dependant and independent variables
df_model <- read.csv(file.path("../../updated_scripts/data","df_to_model_obscot.csv")) %>%
  select(-c(idpointdecapture, num_releve))%>%
  group_by(DATE) %>% 
  summarise_at(vars(NBINDIV:EVI_5_6), mean, na.rm = TRUE) %>% 
  ungroup()


########################
## modélisation bivariée
########################


df_model <-  df_model %>%
  mutate(PRESENCE_CUL = ifelse(NBINDIV<1,0,1),ECO_CLI = "FRANCE") %>% 
  relocate(PRESENCE_CUL, .after = NBINDIV) %>%
  filter(!is.na(PRESENCE_CUL)) %>%
  as_tibble()


fun_compute_correlation_univ <- function(df,indicator){

  if(indicator == "presence"){
    var_to_keep = "PRESENCE_CUL"
  } else if (indicator == "abundance"){
    var_to_keep = "NBINDIV"
  }

  func <- function(x){
    df2 <- df %>% dplyr::select(var_to_keep,!!x)
    ret <- correlation::correlation(df2,method = "distance")##using distance correlation method
    return(ret)
  }

  possible_a <- possibly(func, otherwise = NA_real_)

  spearman_univs <- furrr::future_map(colnames(df[6:ncol(df)]), possible_a)

  spearman_univs <- do.call(rbind.data.frame, spearman_univs)

  spearman_univs$ECO_CLI <- unique(df$ECO_CLI)

  return(spearman_univs)
}


# presence (long run...)
corr_univ_presence <- df_model %>%
  group_split(ECO_CLI) %>%
  map_dfr(.,~fun_compute_correlation_univ(., "presence")) %>%
  as.tibble() %>%
  mutate(indicator = "presence") %>%
  mutate(r = ifelse(r<0,0,r))

# abundance  (long run...)
corr_univ_abundance <- df_model %>%
  filter(NBINDIV>=1) %>%
  group_split(ECO_CLI) %>%
  map_dfr(.,~fun_compute_correlation_univ(., "abundance")) %>%
  as.tibble() %>%
  mutate(indicator = "abundance") %>%
  mutate(r = ifelse(r<0,0,r))


##############
## plotting
##############

# function to plot the CCM (simple plot : only the CCM)
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
    geom_tile(data = most_corr , color = "deeppink3", size = 0.8, show.legend = FALSE,aes(width=1, height=1)) +  # ,aes(width=1, height=1)
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
  bind_rows(corr_univ_abundance) %>%
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
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', Parameter2)), time_lag_2 = as.numeric(stringr::str_match( Parameter2, '([^_]+)(?:_[^_]+){1}$')[,2])) %>%
  arrange(var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = r) %>%
  mutate(correlation = ifelse(p<=0.2,correlation,NA)) %>%
  #mutate(time_lag_1=as.numeric(time_lag_1)+1, time_lag_2=as.numeric(time_lag_2)+1 )%>%
  nest(-c(ECO_CLI,indicator,var))



plots_univ_spearman_temporal_mf <- univ_spearman_temporal_mf %>%
  arrange(rev(indicator),factor(var, levels = c("TM","TN","TX","UM","RR","RRMAX","TPS","QQ","SWV","PP","FG","NDVI","EVI")),factor(ECO_CLI, levels = c("FRANCE"))) %>%
  mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot2(correlation_df = ..1, var = ..1$label[1]))) %>%
  nest(-indicator) %>%
  mutate(univ_temporal = map(data, ~patchwork::wrap_plots(.x$univ_temporal, nrow = 3, ncol = 5))) %>%
  mutate(univ_temporal = map(univ_temporal, ~.x + patchwork::plot_annotation(title = "France"))) %>%
  dplyr::select(-data)




# in the plots below, each row represents one eco-climatic zone (the first row is for whole France) "FRANCE", "Continental", "Mediterranean" ,"Atlantic","Alpine"

p_meteo_presence <- patchwork::wrap_plots(plots_univ_spearman_temporal_mf$univ_temporal[1], ncol = 1  , nrow = 1)
p_meteo_presence + plot_annotation(title="Distance CCM for Presence/Absence (France)",
                                    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))

p_meteo_abundance <- patchwork::wrap_plots(plots_univ_spearman_temporal_mf$univ_temporal[2], ncol = 1  , nrow = 1)
p_meteo_abundance + plot_annotation(title="Distance CCM for Abundance (France)",
                                   theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))





###saving the output in a csv file (if needed)

univ_spearman_temporal_mf_test <- corr_univ_abundance %>%
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
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', Parameter2)), time_lag_2 = as.numeric(stringr::str_match( Parameter2, '([^_]+)(?:_[^_]+){1}$')[,2])) %>%
  arrange(var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = r) %>%
  mutate(correlation =ifelse(correlation >= 0.2, correlation, NA))%>%
  mutate(correlation = ifelse(p <= 0.1,correlation,NA)) %>%
  mutate(choose = ifelse(correlation >= correlation*0.9, "YES", NA))
  #mutate(time_lag_1=as.numeric(time_lag_1)+1, time_lag_2=as.numeric(time_lag_2)+1 )

#write.csv(univ_spearman_temporal_mf_test, "../../updated_scripts/data/distance_ccm_output_presence.csv", row.names=F)



#write.csv(univ_spearman_temporal_mf_test, "../../updated_scripts/data/distance_ccm_output_abundance.csv", row.names=F)