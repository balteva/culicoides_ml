


df_indiv <- data %>%
  left_join(event, by="id") %>%
  select(-c(eventDate, startDayOfYear)) %>%
  filter(!occurrenceStatus == "absent")%>%
  relocate(stateProvince, locationID,date, .after= id)

session_summary <- df_indiv %>%
  group_by(year)%>%
  summarise(
  mean = mean(individualCount, na.rm = TRUE),
median = median(individualCount, na.rm = TRUE))

yr2009 <- session_summary%>%
  filter(year==2009)
# 2 460296 for 2009
# 3 491115 for 2010
# 2 461477 for 2011
# 0 161950 for 2012
trap_info <- df_indiv %>%
  select(year, date, locationID, habitat, decimalLatitude, decimalLongitude)%>%
  group_by(year, date, locationID, habitat, decimalLatitude, decimalLongitude)%>%
  slice_head(n = 1) %>%  
  ungroup()%>%
  mutate(week=week(date))%>%
  group_by(year, week)%>%
  summarise(trap_count=n(), .groups="drop")



ggplot(trap_info, aes(x=week, y=trap_count, fill=as.factor(year)))+
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  #geom_point(shape = 21, fill = "white", size = 1, stroke = 1, alpha=0.8) +
  #facet_wrap(~year, scales="fixed", ncol=2) +
  scale_x_continuous(breaks = seq(1, 52, 4)) +
  labs(title = ("Calendar of trap numbers employed per week"),
       x = "week of the year",
       y = "number of traps at national scale", fill = "year") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 11, face="bold"),
        axis.title.y = element_text(size = 11, face="bold"),
        plot.title = element_text(hjust = 0.5, size= 14, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.text=element_text(size=12,face="bold"),
        legend.position="bottom",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10))+
  scale_fill_brewer(palette = "Dark2") 


trap_yearly <- df_indiv %>%
  select(year, date, locationID, habitat, decimalLatitude, decimalLongitude)%>%
  group_by(year,locationID, habitat, decimalLatitude, decimalLongitude)%>%
  slice_head(n = 1) %>%  
  ungroup()%>%
  group_by(year)%>%
  summarise(trap_total=n(), .groups="drop")


n_distinct(df_indiv$locationID)
#[1] 209
n_distinct(data$id)
sum(df_indiv$organismQuantity)
#[1] 19209261
 

trap_farms <- df_indiv %>%
  select(year, date, locationID, habitat, decimalLatitude, decimalLongitude)%>%
  group_by(decimalLatitude, decimalLongitude, habitat)%>%
  slice_head(n = 1) %>%  
  ungroup()%>%
  group_by(habitat)%>%
  summarise(farm_type = n(), .groups = "drop") %>%
  mutate(percentage = round((farm_type / sum(farm_type)) * 100, 1))

catches_py <-france_species%>%
  as.data.frame()%>%
  select(-c(region, lon, lat, geometry))%>%
  as.data.frame()%>%
  mutate()

sum_catches_py <- catches_py %>%
  group_by(year) %>%
  summarise(across(`chiopterus`:`pulicaris and punctatus`, sum, na.rm = TRUE)) %>%  # Summing relevant columns
  ungroup()





# Convert to long format
long_df <- sum_catches_py %>%
  pivot_longer(cols = `chiopterus`:`pulicaris and punctatus`, 
               names_to = "species", 
               values_to = "abundance")


long_df %>%
  ggplot(aes(x=species, y=abundance, fill=species))+
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  labs(title = ("total counts of species"),
       x = "species",
       y = "count", fill = "year")+
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()+
  facet_wrap(~year)
