library(tidyverse)
library(scales)
library(patchwork)
ecocli <- read.delim("./datapaper/data/measurementorfacts.txt", header = TRUE)%>%
  filter(measurementID=="ECO_CLI") %>%
  select(c(id,measurementValue))%>%
  rename("ECO_CLI"=measurementValue)
 

#loading collection data
data <- read.delim("./datapaper/data/occurrence.txt", header = TRUE) %>%
  select(id, individualCount, organismQuantityType, occurrenceStatus, scientificName)

#loading collection details (date, location, region)
event <- read.delim("./datapaper/data/event.txt", header = TRUE) %>%
  select(id, eventDate,startDayOfYear,month,year, habitat, locationID, stateProvince ,decimalLatitude, decimalLongitude)%>%
  mutate(date = as.Date(startDayOfYear - 1, origin = paste0(year, "-01-01")))

df_indiv <- data %>%
  left_join(event, by="id") %>%
  left_join(ecocli, by="id") %>%
  select(-c(eventDate, startDayOfYear)) %>%
  filter(!occurrenceStatus == "absent")%>%#i dont dilter by this but summarise based on species id site and date. however, need to filter out absent observations for median
  relocate(stateProvince, locationID,date,ECO_CLI, .after= id) %>%
  filter(organismQuantityType=="individuals") #select organism type of interest (i.e. all type, only female, female, only parous female etc)


most_abundant_species <- df_indiv %>%
  group_by(scientificName)%>%
  summarise(total=sum(individualCount))%>%
  arrange(desc(total))

# 1 obsoletus/scoticus 4186089
# 2 dewulfi             597361
# 3 imicola             521319
# 4 chiopterus          177757
# 5 newsteadi           163853
# 6 punctatus           139213
# 7 pulicaris           103284




#selecting species of veterinary interest 
species_of_interest <- c("obsoletus/scoticus", "dewulfi", "imicola", "chiopterus","newsteadi", "punctatus", "pulicaris" ) 


#renaming species not in species of interest list into 'other'
df_main_species <- df_indiv %>%
  mutate(scientificName = case_when( !(scientificName %in% species_of_interest) ~ "other species", TRUE ~ scientificName))%>%
  #filter(!scientificName=="other species") %>% #put this in for maps
  mutate(week=week(date), month=month(date))
# group_by(locationID, date,year, scientificName) %>%
#   summarise(count = sum(individualCount), .groups = "drop")%>%#not calculating the mean ,just renaming for functions


# #summarising into mean counts per trap for each species
# df_summary <- df_main_species%>%
#   group_by(scientificName, date, year)%>% #habitat
#   summarise(mean_trap =mean(individualCount), .groups = "drop")
# 
# #summarising into mean counts per trap for entire france 
# df_summary1<- df_main_species%>%
#   mutate(scientificName = "all species")%>%
#   group_by(scientificName, date, year)%>%
#   summarise(mean_trap = mean(individualCount), .groups = "drop")%>% #and upending df with mean counts per species
#   bind_rows(df_summary)
# #df_summary1 will be the df used for plotting species specific data visualisation + for all culicoides 
#  


df_summary_median <- df_main_species %>%
  group_by(scientificName) %>%#week, year or month
  summarise(
    mean_abundance = mean(individualCount),
    median_abundance = median(individualCount),
    q1 = quantile(individualCount, 0.25),
    q2 = quantile(individualCount, 0.75))
  #   .groups = "drop")%>%
  # mutate(date = ymd(paste(year, month, "01", sep = "-")))


## manual function
df_summary_median %>%
  #filter(scientificName == "obsoletus/scoticus") %>%
  ggplot(aes(x = date, y = median_abundance)) +
  geom_ribbon(aes(ymin = q1, ymax = q2, fill = "Central Range (25–75%)"), alpha = 0.6) +
  geom_point(aes(y = median_abundance), shape = 21, fill = "white", size = 1, stroke = 1, alpha=0.8) +
  geom_line(aes(y = median_abundance, color = "Median"), linewidth = 0.8, alpha = 0.9) +
  #scale_x_continuous(breaks=seq(1, 12, 1))+
  labs(title = paste0("Interannual  Culicoides spp.  population dynamics"),
       y = "Number of individuals per trap",
       x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        axis.title.x = element_text(size = 10, face="bold"),
        axis.title.y = element_text(size = 10, face="bold"),
        plot.title = element_text(hjust = 0.5, size= 12, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.text=element_text(size=10,face="bold"))+
  scale_fill_manual(values = c("Central Range (25–75%)" = "deepskyblue2")) +
  scale_color_manual(values = c("Median" = "deepskyblue4")) +
  facet_wrap(~scientificName, scales="free", ncol=3)

####function for temporal plots (new)
temporal_func <- function(df, var){
  
  df_summary <- df %>%
    group_by(scientificName, month,year) %>%
    summarise(mean_abundance = mean(individualCount),
              median_abundance = median(individualCount),
              q1 = quantile(individualCount, 0.25),
              q2 = quantile(individualCount, 0.75),
              .groups = "drop")%>%
    mutate(date = ymd(paste(year, month, "01", sep = "-")))
  
  plots <- list() 
  for (i in unique(df[[var]])) {  
    df_temp <- df %>%
      filter(.data[[var]] == i)
    
    df_temp_summary <- df_summary %>%
      filter(.data[[var]] == i)
    
    
    plot <- df_temp_summary%>%
      ggplot(aes(x = date, y = median_abundance)) +
      geom_ribbon(aes(ymin = q1, ymax = q2, fill = "Central Range (25–75%)"), alpha = 0.6) +
      geom_point(aes(y = median_abundance), shape = 21, fill = "darkblue", size = 1, stroke = 1, alpha=0.8) +
      #geom_point(aes(y = mean_abundance), shape = 21, fill = "red", size = 1, stroke = 1, alpha=0.8) +
      
      geom_line(aes(y = median_abundance, color = "Median"), linewidth = 0.8, alpha = 0.9) +
      labs(y = "Number of individuals per trap",
           x = "Study year") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(), 
            axis.title.x = element_text(size = 9, face="bold"),
            axis.title.y = element_text(size = 9, face="bold"),
            plot.title = element_text(hjust = 0.5, size= 10, face="bold"),
            axis.text.x = element_text(size=9),
            axis.text.y = element_text(size=9),
            strip.text=element_text(size=10,face="bold"))+
      scale_fill_manual(values = c("Central Range (25–75%)" = "deepskyblue2")) +
      scale_color_manual(values = c("Median" = "deepskyblue4")) +
      facet_wrap(~scientificName, scales="free", ncol=3)
    
    plots[[i]] <- plot
  }
  return(plots)
}
####function for temporal plots (old)
temporal_func_old <- function(df, var){
  
  df_summary <- df %>%
    group_by(scientificName, week, year) %>%
    summarise(mean_abundance = mean(individualCount),
              median_abundance = median(individualCount),
              q1 = quantile(individualCount, 0.25),
              q2 = quantile(individualCount, 0.75),
              .groups = "drop")
  
  plots <- list() 
  for (i in unique(df[[var]])) {  
    df_temp <- df %>%
      filter(.data[[var]] == i)
    
    df_temp_summary <- df_summary %>%
      filter(.data[[var]] == i)
    
    
    plot <- df_temp_summary%>%
      ggplot(aes(x = week, y = median_abundance)) +
      geom_ribbon(aes(ymin = q1, ymax = q2, fill = "Central Range (25–75%)"), alpha = 0.7) +
      geom_point(aes(y = mean_abundance, shape = "Mean"), size = 0., stroke = 0.8, color = "black", alpha = 0.9) +
      geom_line(aes(y = median_abundance, color = "Median"), linewidth = 0.8, alpha = 0.9) +
      scale_x_continuous(breaks=seq(2, 52, 6))+
      labs(title = paste0("Number of ", i, " caught per trap"),
           y = "Abundance per trap",
           x = "Week of the year") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(), 
            axis.title.x = element_text(size = 10, face="bold"),
            axis.title.y = element_text(size = 10, face="bold"),
            plot.title = element_text(hjust = 0.5, size= 12, face="bold"),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            strip.text=element_text(size=10,face="bold"))+
      scale_fill_manual(values = c("Central Range (25–75%)" = "deepskyblue2")) +
      scale_color_manual(values = c("Median" = "deepskyblue4")) +
      scale_shape_manual(values = c("Mean" = 1))+
      facet_wrap(~year, ncol=2)
    
    plots[[i]] <- plot
  }
  return(plots)
}

plots <- temporal_func(df_main_species, "scientificName")
plots_all <- patchwork::wrap_plots(plots, ncol = 3) + plot_layout(guides = "collect") +plot_annotation("Interannual Culicoides spp. population dynamics")


### PLOTS
#Temporal function (old)

#usage : df = dataframe with mean counts per, var = column with species identity

temporal_func <- function(df, var){
  plots <-  list()
  for (i in unique(df[[var]])) {  
    df_temp <- df %>%
      filter(df[[var]] == i)
    
    plot <- df_temp%>%
      ggplot(aes(x=date, y=mean_trap, color=year))+
      geom_line(aes(color=year),linewidth=0.8, alpha=0.2)+
      geom_point(aes(fill = year), size = 1, stroke = 1, alpha=0.8) +
      #scale_y_log10(labels = label_number()) +
      #facet_wrap(~scientificName, scales="fixed", ncol=3) +
      #facet_grid(~year, scales="fixed")+#, ncol=2) +
      labs(title = bquote("Mean counts per trap of C. " * italic(.(df_temp[[var]]))),
           x = "year",
           y = "indiv. counted"
      ) + #color = "species") +
      #y = "indiv. counted (log scale)", color = "species") +
      theme_minimal() +
      theme(axis.title.x = element_text(size = 10, face="bold"),
            axis.title.y = element_text(size = 10, face="bold"),
            plot.title = element_text(hjust = 0.5, size= 12, face="bold"),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            strip.text=element_text(size=10,face="bold"),
            legend.position="none")
    plots[[i]] <- plot
  }
  return(plots)
}


plots <- temporal_func(df_summary1, "scientificName")
plots_all <- patchwork::wrap_plots(plots, ncol = 3) + plot_layout(guides = "collect")


###If wanting to plot manually: 

ggplot(df_summary1, aes(x=date, y=mean_trap, color=scientificName))+
  geom_line(linewidth=0.8, alpha=0.2)+
  geom_point(shape = 21, fill = "white", size = 1, stroke = 1, alpha=0.8) +
  #scale_y_log10(labels = label_number()) +
  #facet_wrap(~scientificName, scales="fixed", ncol=3) +
  facet_wrap(~scientificName, scales="free", ncol=3) +
  labs(title = expression ("Mean counts per trap of " *italic("Culicoides spp. ")* "(France 2009 - 2012)"),
       x = "year",
       y = "indiv. counted", color = "species") +
       #y = "indiv. counted (log scale)", color = "species") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face="bold"),
        axis.title.y = element_text(size = 12, face="bold"),
        plot.title = element_text(hjust = 0.5, size= 14, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.text=element_text(size=12,face="bold"),
        legend.position="none")
  
##boxplots function to show seasonal variability
#usage : df = dataframe with mean counts per, var = column with species identity

boxplot_func <- function(df, var){
  plots <-  list()
  for (i in unique(df[[var]])) {  
    df_temp <- df %>%
      filter(df[[var]] == i)
    
    plot <- df_temp%>%
      ggplot( aes(x=as.factor(month(date)), y=individualCount)) +
      geom_boxplot(outliers= F, fill="deepskyblue3")  + #removing outliers
      theme_minimal() +
      theme(
        legend.position="none",
        plot.title = element_text(size=13, hjust = 0.5, face="bold")) + 
      xlab("Month") +
      ylab("Number of individuals per trap") +
      labs(title = df_temp[[var]]) +
      theme(plot.title = element_text(hjust = 0.5, face="bold", size=9),
            axis.text.x = element_text(size=9),
            axis.text.y = element_text(size=9),
            axis.title.x = element_text(size=8, face="bold"),
            axis.title.y = element_text(size=8, face="bold"),
            strip.text=element_text(size=8,face="bold"))#+

      #facet_wrap(~year,scales="fixed")
    
    plots[[i]] <- plot
  }
  return(plots)
}
plots <- boxplot_func(df_main_species, "scientificName")
plots_all <- patchwork::wrap_plots(plots, ncol = 3) + plot_layout(guides = "collect") + plot_annotation('Seasonal variation of Culicoides spp. populations')

# manual boxplot. Will show mean count distribution counts during the enitire year (and not per month)



  #######raw counts box plot per specific year
  
  df_main_species%>%
  ggplot(aes(x=as.factor(month(date)), y=individualCount, fill= scientificName)) +
    geom_boxplot(outliers= F)  + #rempving outliers
    theme_minimal() +
    theme(
      legend.position="none",
      plot.title = element_text(size=13, hjust = 0.5, face="bold")) + 
    ggtitle("Actual count per trap per capture session (2012)") +
    xlab("month") +
    ylab("indiv. counted") +
    labs(fill="species")+
    scale_fill_brewer(palette = "Dark2")+
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          axis.text.x = element_text(size=10, face="bold"),
          axis.text.y = element_text(size=10, face="bold"),
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
          strip.text=element_text(size=12,face="bold"))+
    facet_wrap(~scientificName,scales="free_y", ncol=3)
  #histograms are good to show seasonal variability
  #temporal series are for interannual variabimity
#########
  #manual histogram plots for raw counts during entire period (confounded years)
  df_main_species%>%
    ggplot(aes(individualCount)) +
    geom_histogram(col = "black", alpha= 0.7, fill="deepskyblue")+
    geom_vline(aes(xintercept = mean(individualCount), color = "mean"), linetype = "solid", size = 1)+ #adding descriptive stats
    geom_vline(aes(xintercept = median(individualCount), color = "median"), linetype = "solid", size = 1)+
    scale_x_log10(labels = label_number(), breaks=breaks_log(n=6, base=10))+
    theme_minimal()+
    
    ggtitle(expression("Distribution of" *italic(" Culicoides spp. ")* "capture values (France 2009 - 2012)"))+
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          axis.text.x = element_text(size=10, face="bold"),
          axis.text.y = element_text(size=10, face="bold"),
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
          strip.text=element_text(size=12,face="bold"),
          legend.position = "bottom")+
    labs(x="Individuals caught in one trapping session (log scale)",
         y="Frequency", color = "Statistics")+
    scale_color_manual(values=c("mean"="darkred", "median"="darkblue"))
   
  
#histogram function. if running on raw counts, we use df_main_species1 ( df_main_species2 without outliers). Otherwise, for mean counts we use df_summary1
  df_main_species1 <- df_main_species %>%
    mutate(scientificName ="all species")%>%
    rbind(df_main_species) %>%
   # rename("mean_trap"= individualCount)


## If we want to remove outliers, we run the block below

  Q1 <- quantile(df_main_species1$mean_trap, 0.25)
  Q3 <- quantile(df_main_species1$mean_trap, 0.75)
  IQR <- Q3 - Q1
  lower_limit <- Q1 - 1.5*IQR
  upper_limit <- Q3 + 1.5*IQR

  df_main_species2<- df_main_species1 %>%
    filter(mean_trap >= lower_limit & mean_trap<= upper_limit)


  
hist_func <- function(df, var){
    plots <-  list()
    
    for (i in unique(df[[var]])) {  
      df_temp <- df %>%
        filter(df[[var]] == i)
      
     plot <- df_temp%>%
        ggplot(aes(mean_trap)) +
        geom_histogram(fill="deepskyblue3", alpha= 0.7, binwidth=5)+
        geom_vline(aes(xintercept = mean(mean_trap), color = "mean"), linetype = "solid", size = 1)+ #adding descriptive stats
        geom_vline(aes(xintercept = median(mean_trap), color = "median"), linetype = "solid", size = 1)+
        #scale_x_log10(labels = label_number()) +  #when not excluding outliers
        #facet_wrap(~year, scales="free_x")+ 
        theme_minimal()+
       ggtitle(paste0("Histogram of ", df_temp[[var]], " counts")) + #mean if needed mean
        theme(plot.title = element_text(hjust = 0.5, size=10),
              axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9),
              axis.title.x = element_text(size=9, face="bold"),
              axis.title.y = element_text(size=9, face="bold"),
              strip.text=element_text(size=9,face="bold"),
              legend.position = "bottom")+
        labs(x="individuals counted",
             y="Frequency")+
       scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"))
   
       plots[[i]] <- plot
    }
    return(plots)
  }
  

plots <- hist_func(df_main_species2, "scientificName")
plots_all <- patchwork::wrap_plots(plots, ncol = 3) + plot_layout(guides = "collect")

############################

#ICI C'est PACHKA !'
df_main_species <- df_indiv %>%
  mutate(scientificName = case_when( !(scientificName %in% species_of_interest) ~ "other species", TRUE ~ scientificName))%>%
  #filter(!scientificName=="other species") %>% #put this in for maps
  mutate(week=week(date), month=month(date))


############# MAPS

#grouping by french regions
df_region <- df_main_species %>%  
  group_by(scientificName, year,stateProvince ) %>% #
  summarise(sum_trap = sum(individualCount), .groups = "drop")

df_region_wide <- pivot_wider(df_region, names_from = scientificName, values_from = sum_trap, values_fill = 0)

#loading france shape file
library(sf)
france <- st_read("./datapaper/france/france_regions_modified.shp")
france_centroids <- st_centroid(france)

france_species <- left_join(france_centroids, df_region_wide, by = c("region" = "stateProvince")) #joining culicoides df with geometry

coords <- st_coordinates(france_species)
france_species$lon <- coords[,1]
france_species$lat <- coords[,2]

#species of interest that i wanna map
species_cols <- c("obsoletus/scoticus", "pulicaris","punctatus", "newsteadi", "dewulfi", "imicola", "chiopterus", "other species")

library(scatterpie)
###manual plot for maps
france_species_2011 <- france_species%>%
  filter(year == 2011) #change year 

ggplot() +
  geom_sf(data = france, fill = "grey95", color = "black") +
  geom_scatterpie(
    aes(x = lon, y = lat, group = region),
    data = st_drop_geometry(france_species), #change year
    cols = species_cols, alpha=0.88, pie_scale = 3.5) +
  # geom_text(data = st_centroid(france),
  #           aes(x = (st_coordinates(geometry)[,1],
  #               y = (st_coordinates(geometry)[,2],
  #               label = region),
  #           size = 2.5, color = "black")+
  coord_sf() +
  theme_minimal()+
  scale_fill_brewer(palette = "Set2", name="Species")+
  theme(legend.position="bottom",
        plot.title = element_text(size=13, hjust = 0.5, face="bold"),
        axis.title.x=element_blank(),
        axis.title.y = element_blank()) + 
  ggtitle("Culicoides spp. population composition per region (2009 - 2012)")+ #change year+
guides(fill = guide_legend(nrow = 2, byrow = TRUE))

################ making a function


#usage : map = shapefile, df = joined species dataframe with geometry. var = year column, species_cols = vector with species of interest
map_func <- function(map, df, var, species_cols){
  
  plots <-  list()
  plot_index <- 1
  for (i in unique(df[[var]])) {  
    df_temp <- df %>%
      filter(df[[var]] == i)
    
    plot <- ggplot() +
      geom_sf(data = map, fill = "grey95", color = "black") +
      geom_scatterpie(
        aes(x = lon, y = lat, group = region),
        data = st_drop_geometry(df_temp),
        cols = species_cols, alpha=0.88, pie_scale = 3.5) +
      coord_sf() +
      theme_minimal()+
      scale_fill_brewer(palette = "Set2", name="Species")+
      labs(title=paste0('Species composition per region in ', as.character(df_temp[[var]])))+
      theme(legend.position="none",#"bottom"
            plot.title = element_text(size=9, hjust = 0.5, face="bold"))
    
    plots[[plot_index]] <- plot  # numeric index for each plot(usual approach didnt work)
    plot_index <- plot_index + 1  # Increment index
    
  }
  return(plots)
}


plots <- map_func(france, france_species, "year", species_cols)
plots_all <- patchwork::wrap_plots(plots, ncol = 2)# + plot_layout(guides = "collect")



