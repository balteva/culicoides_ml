library(tidyverse)
library(scales)
library(patchwork)
#Altitude <- read.delim("./data/measurementorfacts.txt", header = TRUE)
 

#loading collection data
data <- read.delim("./data/occurrence.txt", header = TRUE) %>%
  select(id, individualCount, organismQuantityType, sex, reproductiveCondition, occurrenceStatus, scientificName, taxonRank)
#loading collection details (date, location, region)
event <- read.delim("./data/event.txt", header = TRUE) %>%
  select(id, eventDate,startDayOfYear,month,year, habitat, locationID, stateProvince ,decimalLatitude, decimalLongitude)%>%
  mutate(date = as.Date(startDayOfYear - 1, origin = paste0(year, "-01-01")))

df_indiv <- data %>%
  left_join(event, by="id") %>%
  select(-c(eventDate, startDayOfYear)) %>%
  #filter(!occurrenceStatus == "absent")%>%#i dont dilter by this but summarise based on species id site and date
  relocate(stateProvince, locationID,date, .after= id) %>%
  filter(organismQuantityType=="individuals")%>% #select organism type of interest (i.e. all type, only female, female, only parous female etc)
  select(-c(sex, reproductiveCondition))


#selecting species of veterinary interest 
species_of_interest <- c("obsoletus/scoticus", "punctatus", "pulicaris", "newsteadi", "dewulfi", "imicola", "chiopterus") 


#renaming species not in species of interest list into 'other'
df_main_species <- df_indiv %>%
  mutate(scientificName = case_when( !(scientificName %in% species_of_interest) ~ "other species", TRUE ~ scientificName))%>%
  mutate(scientificName = recode(scientificName, "punctatus" = "pulicaris and punctatus",
                                 "pulicaris" = "pulicaris and punctatus")) 

#summarising into mean counts per trap for each species
df_summary <- df_main_species%>%
  group_by(scientificName, date, year)%>% #habitat
  summarise(mean_trap =mean(individualCount), .groups = "drop")

#summarising into mean counts per trap for entire france 
df_summary1<- df_main_species%>%
  mutate(scientificName = "all species")%>%
  group_by(scientificName, date, year)%>%
  summarise(mean_trap = mean(individualCount), .groups = "drop")%>% #and upending df with mean counts per species
  bind_rows(df_summary) 
#df_summary1 will be the df used for plotting species specific data visualisation + for all culicoides 
 
### PLOTS
#Temporal function

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
      labs(title = paste0("Mean counts per trap of ",  df_temp[[var]]),
           x = "year",
           y = "indiv. counted") + #color = "species") +
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
  
##boxplots function
#usage : df = dataframe with mean counts per, var = column with species identity

boxplot_func <- function(df, var){
  plots <-  list()
  for (i in unique(df[[var]])) {  
    df_temp <- df %>%
      filter(df[[var]] == i)
    
    plot <- df_temp%>%
      ggplot( aes(x=as.factor(month(date)), y=mean_trap, fill= year)) +
      geom_boxplot(outliers= F)  + #removing outliers
      theme_minimal() +
      theme(
        legend.position="none",
        plot.title = element_text(size=13, hjust = 0.5, face="bold")) + 
      xlab("month") +
      ylab("indiv. counted") +
      labs(title = paste0("Boxplot of mean counts per trap of ",df_temp[[var]])) +
      theme(plot.title = element_text(hjust = 0.5, face="bold"),
            axis.text.x = element_text(size=9),
            axis.text.y = element_text(size=9),
            axis.title.x = element_text(size=10, face="bold"),
            axis.title.y = element_text(size=10, face="bold"),
            strip.text=element_text(size=10,face="bold"))+
      facet_wrap(~year,scales="fixed")
    
    plots[[i]] <- plot
  }
  return(plots)
}
plots <- boxplot_func(df_summary1, "scientificName")
plots_all <- patchwork::wrap_plots(plots, ncol = 3) + plot_layout(guides = "collect")

# manual boxplot. Will show mean count distribution counts during the enitire year (and not per month)
  ggplot(df_summary1, aes(x=scientificName, y=mean_trap, fill= scientificName)) +
  geom_boxplot(outliers= F)  + #rempving outliers
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=13, hjust = 0.5, face="bold")) + 
  ggtitle("Mean count per trap per capture session") +
  xlab("species") +
  ylab("indiv. counted") +
  labs(fill="species")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        axis.text.x = element_text(size=10, face="bold"),
        axis.text.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        strip.text=element_text(size=12,face="bold"))+
  facet_wrap(~year,scales="free")


  #######raw counts box plot per specific year
  
  df_main_species%>%
   filter(year==2012) %>%
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
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          axis.text.x = element_text(size=10, face="bold"),
          axis.text.y = element_text(size=10, face="bold"),
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
          strip.text=element_text(size=12,face="bold"))+
    facet_wrap(~scientificName,scales="free_y", ncol=3)
  
#########
  #manual histogram plots for raw counts during entire period (confounded years)
  df_main_species%>%
    ggplot(aes(individualCount, fill=scientificName)) +
    geom_histogram(col = "black", alpha= 0.7)+
    # geom_vline(aes(xintercept = mean(mean_trap), color = "mean"), linetype = "solid", size = 1)+ #adding descriptive stats
    # geom_vline(aes(xintercept = median(NBINDIV_per_trap), color = "median"), linetype = "solid", size = 1)+
    scale_x_log10(labels = label_number()) +
    facet_wrap(~scientificName, scales="free_x")+ 
    theme_minimal()+
    ggtitle(expression("Distribution of raw " *italic("Culicoides spp. ")* "counts per trap (France 2009 - 2012)"))+
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          axis.text.x = element_text(size=10, face="bold"),
          axis.text.y = element_text(size=10, face="bold"),
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
          strip.text=element_text(size=12,face="bold"),
          legend.position = "none")+
    labs(x="individuals counted",
         y="Frequency")
   
  
#histogram function. if running on raw counts, we use df_main_species1 ( df_main_species2 without outliers). Otherwise, for mean counts we use df_summary1
  df_main_species1 <- df_main_species %>%
    mutate(scientificName ="all species")%>%
    rbind(df_main_species) %>%
    rename("mean_trap"= individualCount)

  
## If we want to remove outliers, we run the block below 
  
  # Q1 <- quantile(df_main_species1$mean_trap, 0.25)
  # Q3 <- quantile(df_main_species1$mean_trap, 0.75)
  # IQR <- Q3 - Q1
  # lower_limit <- Q1 - 1.5*IQR
  # upper_limit <- Q3 + 1.5*IQR
  # 
  # df_main_species2<- df_main_species1 %>% 
  #   filter(mean_trap >= lower_limit & mean_trap<= upper_limit)  

 
  
hist_func <- function(df, var){
    plots <-  list()
    
    for (i in unique(df[[var]])) {  
      df_temp <- df %>%
        filter(df[[var]] == i)
      
     plot <- df_temp%>%
        ggplot(aes(mean_trap)) +
        geom_histogram(fill="darkblue", alpha= 0.7, binwidth=5)+
        geom_vline(aes(xintercept = mean(mean_trap), color = "mean"), linetype = "solid", size = 1)+ #adding descriptive stats
        geom_vline(aes(xintercept = median(mean_trap), color = "median"), linetype = "solid", size = 1)+
        #scale_x_log10(labels = label_number()) +  #when not excluding outliers
        facet_wrap(~year, scales="free_x")+ 
        theme_minimal()+
       ggtitle(paste0("Histogram of ", df_temp[[var]], " counts")) + #mean if needed mean
        theme(plot.title = element_text(hjust = 0.5, face="bold"),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              axis.title.x = element_text(size=10, face="bold"),
              axis.title.y = element_text(size=10, face="bold"),
              strip.text=element_text(size=10,face="bold"),
              legend.position = "bottom")+
        labs(x="individuals counted",
             y="Frequency")+
       scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"))
   
       plots[[i]] <- plot
    }
    return(plots)
  }
  

plots <- hist_func(df_summary1, "scientificName")
plots_all <- patchwork::wrap_plots(plots, ncol = 3) + plot_layout(guides = "collect")

############################


############# MAPS

#grouping by french regions
df_region_mean <- df_main_species %>%  
  group_by(scientificName, year, stateProvince ) %>%
  summarise(sum_trap = sum(individualCount), .groups = "drop")

df_region_wide <- pivot_wider(df_region_mean, names_from = scientificName, values_from = sum_trap, values_fill = 0)

#loading france shape file
library(sf)
france <- st_read("./france/france_regions_modified.shp")
france_centroids <- st_centroid(france)

france_species <- left_join(france_centroids, df_region_wide, by = c("region" = "stateProvince")) #joining culicoides df with geometry

coords <- st_coordinates(france_species)
france_species$lon <- coords[,1]
france_species$lat <- coords[,2]

#species of interest that i wanna map
species_cols <- c("obsoletus/scoticus", "pulicaris and punctatus", "newsteadi", "dewulfi", "imicola", "chiopterus", "other species")

library(scatterpie)
###manual plot for maps
france_species_2011 <- france_species%>%
  filter(year == 2011) #change year 

ggplot() +
  geom_sf(data = france, fill = "grey95", color = "black") +
  geom_scatterpie(
    aes(x = lon, y = lat, group = region),
    data = st_drop_geometry(france_species_2009), #change year
    cols = species_cols, alpha=0.8, pie_scale = 3.5) +
  coord_sf() +
  theme_minimal()+
  scale_fill_brewer(palette = "Dark2", name="Species")+
  theme(legend.position="bottom",
        plot.title = element_text(size=13, hjust = 0.5, face="bold")) + 
  ggtitle("Species composition per region (2011)") #change year

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
        cols = species_cols, alpha=0.8, pie_scale = 3.5) +
      coord_sf() +
      theme_minimal()+
      scale_fill_brewer(palette = "Dark2", name="Species")+
      labs(title=paste0("Species composition per region in ", as.character(df_temp[[var]])))+
      theme(legend.position="bottom",
            plot.title = element_text(size=13, hjust = 0.5, face="bold"))
    
    plots[[plot_index]] <- plot  # Use numeric index for each plot
    plot_index <- plot_index + 1  # Increment index
    
  }
  return(plots)
}


plots <- map_func(france, france_species, "year", species_cols)
plots_all <- patchwork::wrap_plots(plots, ncol = 2) + plot_layout(guides = "collect")



