library(tidyverse)
library(scales)
library(patchwork)
ecocli <- read.delim("./datapaper/data/measurementorfacts.txt", header = TRUE)%>%
  filter(measurementID=="ECO_CLI") %>%
  select(c(id,measurementValue))%>%
  rename("ECO_CLI"=measurementValue)



data <- read.delim("./datapaper/data/occurrence.txt", header = TRUE) %>%
  select(id, individualCount, organismQuantityType, occurrenceStatus,occurrenceID, organismQuantity, scientificName)



#loading collection details (date, location, region)
event <- read.delim("./datapaper/data/event.txt", header = TRUE) %>%
  select(id, eventDate,startDayOfYear,month,year, habitat, locationID, stateProvince)%>%
  mutate(date = as.Date(startDayOfYear - 1, origin = paste0(year, "-01-01")))

df_indiv <- data %>%
  left_join(event, by="id") %>%
  left_join(ecocli, by="id") %>%
  select(-c(eventDate, startDayOfYear)) %>%
  filter(!occurrenceStatus == "absent")%>%#i dont filter by this but summarise based on species id site and date. however, need to filter out absent observations for median
  relocate(stateProvince, locationID,date,ECO_CLI, .after= id) %>%
  filter(organismQuantityType=="individuals") #select organism type of interest (i.e. all type, only female, female, only parous female etc)
#sum without filtering for organismQuantityType = 19209261

df_indiv%>%
  setDT()
#sum with filter == "individuals" = [1] 6340177 #this is the correct one!
df_indiv[organismQuantityType == 'individuals', sum(organismQuantity), by = id] %>% .[V1 > 0,] %>% summary()

#farms
farm_counts <- df_indiv %>%
  select(c(locationID, habitat))%>%
  group_by(locationID, habitat)%>%
  unique()

summary_farms <- farm_counts %>%
  group_by(habitat)%>%
  summarise(total_sites=n())%>%
  mutate(percent=total_sites/210*100, 
  total_count=sum(total_sites))
  #obsoletus   scoticus  -> remove  , keep only complex species

unique_trap_counts <- df_indiv %>%
  select(c(locationID, year))%>%
  group_by(locationID, year)%>%
  unique()%>%
  group_by(year)%>%
  summarise(total_sites=n())
  



most_abundant_species <- df_indiv %>%
  group_by(scientificName)%>%
  summarise(total=sum(individualCount))%>%
  arrange(desc(total))

main_species_metrics <-  most_abundant_species %>%
  slice_head(n = 7) %>%  
  summarise(total_mainspecies=sum(total))%>%
  mutate(percentage=total_mainspecies/6340177*100)

# 1 obsoletus/scoticus 4186089
# 2 dewulfi             597361
# 3 imicola             521319
# 4 chiopterus          177757
# 5 newsteadi           163853
# 6 punctatus           139213
# 7 pulicaris           103284
# 8 achrayi              51401
# 9 brunnicans           49644
# 10 deltus               49536

# scientificName       total percentage_of_all
# <chr>                <int>             <dbl>
#   1 obsoletus/scoticus 4186089             66.0 
# 2 dewulfi             597361              9.42
# 3 imicola             521319              8.22
# 4 chiopterus          177757              2.80
# 5 newsteadi           163853              2.58
# 6 punctatus           139213              2.20
# 7 pulicaris           103284              1.63

# total_mainspecies percentage
# <int>      <dbl>
#   1           5888876       92.9


#selecting species of veterinary interest 
species_of_interest <- c("obsoletus/scoticus", "dewulfi", "imicola", "chiopterus","newsteadi", "punctatus", "pulicaris" ) 


#renaming species not in species of interest list into 'other'
df_main_species <- df_indiv %>%
  #mutate(scientificName = case_when( !(scientificName %in% species_of_interest) ~ "other species", TRUE ~ scientificName))%>%
  #filter(!scientificName=="other species") %>% #put this in for maps
  mutate(week=week(date), month=month(date))
# group_by(locationID, date,year, scientificName) %>%
#   summarise(count = sum(individualCount), .groups = "drop")%>%#not calculating the mean ,just renaming for functions




df_summary_median <- df_main_species %>%
  #group_by(scientificName) %>%#week, year or month
  summarise(#should be groyp by event
    mean_abundance = mean(individualCount),
    median_abundance = median(individualCount),
    q1 = quantile(individualCount, 0.25),
    q2 = quantile(individualCount, 0.75))
#   .groups = "drop")%>%
# mutate(date = ymd(paste(year, month, "01", sep = "-")))

