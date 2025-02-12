library(tidyverse)
library(furrr)
library(patchwork)
library(correlation)

#this summary is for abundance

df_model <- read.csv(file.path("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/data/df_to_model_before.csv")) %>%
  select(-peak_date)%>%
  rename(NBINDIV=4)%>% #renaming the fourth column, for some reason cant do it with just mutate >;(
  group_by(  ECO_CLI, date) %>% #might be for the grouped analysis for the commented out code
  summarise_at(vars(NBINDIV:EVI_2_3), mean, na.rm = TRUE)%>%
  as_tibble()

### Adding france entiere to  eco_clim column

df_model_allECO_CLIs <- df_model %>%
  mutate(ECO_CLI = "FRANCE")
df_model <- bind_rows(df_model,df_model_allECO_CLIs)

#function to include presence or absence indicator(will not use in this script 'presence", still need to run it)
fun_compute_correlation_univ <- function(df,indicator){
  
  if(indicator == "abundance"){
    var_to_keep = "NBINDIV"
  }
  #SPEARMANS CORRELATION
  func <- function(x){
    df2 <- df %>% dplyr::select(var_to_keep,!!x)  #,ID_SITE
    ret <- correlation::correlation(df2,method = "spearman", )##using distance correlation method   # multileve = TRUE
    return(ret)
  }
  
  possible_a <- possibly(func, otherwise = NA_real_)
  
  spearman_univs <- furrr::future_map(colnames(df[5:ncol(df)]), possible_a)
  
  spearman_univs <- do.call(rbind.data.frame, spearman_univs)
  
  spearman_univs$ECO_CLI <- unique(df$ECO_CLI)
  
  return(spearman_univs)
}


# abundance  (long run...)
corr_univ_abundance <- df_model %>%
  filter(!NBINDIV==0) %>% #filtering out all 0 values to count correlation coeff. for ABUNDANCE ONLY
  group_split(ECO_CLI) %>%
  map_dfr(.,~fun_compute_correlation_univ(., "abundance")) %>%
  as.tibble() %>%
  mutate(indicator = "abundance") #%>% mutate(r = ifelse(r<0,0,r)) #removing this (needed only for distance corr)
  


  ################
  ## plotting
  ##############
  
  ##My code for spearman, no closely related correlated values (il n'y aura pas des carrées noires)
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
      geom_tile(data = most_corr , color = "deeppink3", size = 0.6, show.legend = FALSE,aes(width=1, height=1)) +  
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



univ_spearman_temporal_mf <- corr_univ_abundance %>%
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
  rename(correlation = rho) %>%
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

p_meteo_abundance_before_france <- patchwork::wrap_plots(plots_univ_spearman_temporal_mf$univ_temporal[1:5], ncol = 1, nrow = 5)

