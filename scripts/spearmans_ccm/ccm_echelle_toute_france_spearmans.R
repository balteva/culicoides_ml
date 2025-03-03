library(tidyverse)
library(furrr)
library(patchwork)
library(correlation)


df_model <- read.csv(file.path("../../data","df_to_model_fixed_obscot.csv")) %>%
  group_by(date) %>% 
  summarise_at(vars(NBINDIV:EVI_5_6), mean, na.rm = TRUE)
########################
## modélisation bivariée
########################

# open dataset containing the dependant and independent variables

df_model <-  df_model %>%
  mutate(presence_culi = ifelse(NBINDIV<1,0,1)) %>% #?why 3 and not 1 --considered in our case as an indicator of presence/absence
  relocate(presence_culi, .after = NBINDIV) %>%
  filter(!is.na(presence_culi)) %>%
  as_tibble()

#head(unique(sort(df_model$NBINDIV), 10))

df_model <- df_model %>%
  mutate(ECO_CLI = "FRANCE")




fun_compute_correlation_univ <- function(df,indicator){

  if(indicator == "presence"){
    var_to_keep = "presence_culi"
  } else if (indicator == "abundance"){
    var_to_keep = "NBINDIV"
  }

  func <- function(x){
    df2 <- df %>% dplyr::select(var_to_keep,!!x)
    ret <- correlation::correlation(df2,method = "spearman")##using distance correlation method
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
  mutate(indicator = "presence") 
 # mutate(r = ifelse(r<0,0,r))

# abundance  (long run...)
corr_univ_abundance <- df_model %>%
  filter(NBINDIV>=1) %>%
  group_split(ECO_CLI) %>%
  map_dfr(.,~fun_compute_correlation_univ(., "abundance")) %>%
  as.tibble() %>%
  mutate(indicator = "abundance") 
 # mutate(r = ifelse(r<0,0,r))





################v
## plotting
##############v

# function to plot the CCM (simple plot : only the CCM)
### Plot function
fun_ccm_plot2 <- function(correlation_df, var) {
  if(length(unique(correlation_df$correlation)) != 1) { #counts correlations values that are all the same(such as NA), if there is more than one unique value, if so, go to next line
    
    
    if(mean(correlation_df$correlation, na.rm = TRUE) > 0) { #checks if mean of all corr values is negative or positive
      most_corr <- correlation_df %>%
        filter(correlation == max(correlation[correlation > 0], na.rm = TRUE)) #if its positive, then saves largest correlated value
      most_corr2 <- correlation_df %>% arrange(desc(correlation)) %>% filter(correlation >= most_corr$correlation * 0.9) #sorts in descending order and filters rows where the values are at least 90% of the highest corr value
    } else {
      most_corr <- correlation_df %>%
        filter(correlation == min(correlation[correlation < 0], na.rm = TRUE)) #if negative, saves largest NEGATVE corr value
      most_corr2 <- correlation_df %>% arrange(correlation) %>% filter(correlation <= most_corr$correlation * 0.9) #sorts in ascending order and filters rows where the values are at least 90% of the highest corr value
    }
    
  } else {
    most_corr <- most_corr2 <- correlation_df[1,] #if theres only one unique corr value, choses the first one to represent
  }
  
  
  ccm_plot <- ggplot(data = correlation_df, aes(time_lag_1, time_lag_2, fill = correlation)) +
    geom_tile(color = "white", show.legend = TRUE, size = 0.05,aes(width=1, height=1)) +
    geom_tile(data = most_corr2 , color = "black", size = 0.2, show.legend = FALSE,aes(width=1, height=1)) +
    geom_tile(data = most_corr , color = "deeppink3", size = 0.8, show.legend = FALSE,aes(width=1, height=1)) +  
    theme_minimal() +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          axis.title = element_text(size = 8),
          legend.key.size = unit(0.8, "cm"),
          legend.title=element_text(size=10),
          legend.position = "none"
    ) +
    ggtitle(var) +
    annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("rho(",most_corr$time_lag_2,",",most_corr$time_lag_1,") = ",round(most_corr$correlation,2))) +#rho instead of r
    coord_fixed() +
    ylab("time lag 1") +
    xlab("time lag 2") +
    scale_fill_gradient2(low="blue", mid = "white", high = "red", limit = c(-1,1), space = "Lab", name = "Spearman's correlation", na.value = "grey")#changed name
  #changed limit to c(-1,1) instead of c(0,1) (for spearmans)
  #added mid value as white and changed low as blue
  
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
  rename(correlation = rho) %>%
  mutate(correlation = ifelse(p<=0.2,correlation,NA)) %>%
  mutate(time_lag_1=as.numeric(time_lag_1)+1, time_lag_2=as.numeric(time_lag_2)+1)%>%
  nest(-c(ECO_CLI,indicator,var))



plots_univ_spearman_temporal_mf <- univ_spearman_temporal_mf %>%
  arrange(rev(indicator),factor(var, levels = c("TM","TN","TX","UM","RR","RRMAX","TPS","QQ","SWV","PP","FG","NDVI","EVI")),factor(ECO_CLI, levels = c("FRANCE"))) %>%
  mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot2(correlation_df = ..1, var = ..1$label[1]))) %>%
  nest(-indicator) %>%
  mutate(univ_temporal = map(data, ~patchwork::wrap_plots(.x$univ_temporal, nrow = 3, ncol = 5))) %>%
  mutate(univ_temporal = map(univ_temporal, ~.x + patchwork::plot_annotation(title = "France"))) %>%
  dplyr::select(-data)






p_meteo_presence <- patchwork::wrap_plots(plots_univ_spearman_temporal_mf$univ_temporal[1], ncol = 1  , nrow = 1)
p_meteo_presence + plot_annotation(title="Spearman's CCM for Presence/Absence (France)",
                                   theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))

p_meteo_abundance <- patchwork::wrap_plots(plots_univ_spearman_temporal_mf$univ_temporal[2], ncol = 1  , nrow = 1)
p_meteo_abundance + plot_annotation(title="Spearman's CCM for Abundance (France)",
                                    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))

