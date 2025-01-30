library(fst)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(dplyr)

# output_file <- ("C:/Users/ibalt/OneDrive/Desktop/df_short.csv")
# write.csv(df_short ,file=output_file, row.names = FALSE)

---------------------------------------------------------------------------------------------------------------
                                      ##INFO

# df_short -> df without NA values but with individual species counts
  
# df_MY -> df with summed up species per trap per day (summarised counts for each site per day)
---------------------------------------------------------------------------------------------------------------
  
  
#dd <- read.fst("covariates_dict.fst")

data_na <- read.fst("../covariates.fst")

#removing NA values (introduced by adding climate data)
NBINDIV_na <- is.na(data_na$NBINDIV)
data <- subset(data_na, !NBINDIV_na)

#selecting columns of interest
df_short <- as.data.frame(data) %>%
  select(ID_SITE, DATE, NBINDIV,ESPECE,ECO_CLI, longitude, latitude) %>%
  filter(NBINDIV != 0) %>%
  mutate(DATE = as.Date(DATE))


#sum(df_short$NBINDIV) #total number of culicoides in the dataset 6404081 for 2009-2012 period

#adding up all species per day per site 

df_MY<- df_short%>%
  group_by(DATE,ID_SITE) %>% #,ECO_CLI) %>%
  summarize(NBINDIV_per_trap=sum(NBINDIV), .groups="drop")
---------------------------------------------------------------------------------------------------------------


#will exclude outlier values (some traps collected >160000/day)
#because outliers skew the presentation

#Calculating interquantile range
#geom_histogram doesnt have the option for removing outliers like geom_boxplot

----------------------------------------------------------------------------------------------------------------
        #histograms
----------------------------------------------------------------------------------------------------------------
Q1 <- quantile(df_MY$NBINDIV_per_trap, 0.25)
Q3 <- quantile(df_MY$NBINDIV_per_trap, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5*IQR
upper_limit <- Q3 + 1.5*IQR

df_MY_no<- df_MY %>% 
  filter(NBINDIV_per_trap >= lower_limit & NBINDIV_per_trap <= upper_limit)

df_MY_no$month <- as.factor(month(df_MY_no$DATE))
#jpeg("../../hist_pertrap_pernight_with_stats_cornflower_2009_2012.jpeg")
##simple histogram for all years as counts per night per trap
df_MY_no%>%
  ggplot(aes(NBINDIV_per_trap)) +
  geom_histogram(fill="cornflowerblue", binwidth = 50, col = "black", alpha= 0.7)+
  geom_vline(aes(xintercept = mean(NBINDIV_per_trap), color = "mean"), linetype = "solid", size = 1)+ #adding descriptive stats
  geom_vline(aes(xintercept = median(NBINDIV_per_trap), color = "median"), linetype = "solid", size = 1)+
#  facet_grid(~year(DATE))+ 
  ggtitle("Histogram of culicoides spp. counts per trap per night 2009-2012\n (outliers excluded)") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="indiv. per trap per night",
       y="Frequency")+
  scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"))+
  theme_minimal()

dev.off()



###############################counting median and mean for each year to include stat bars in the plot
stats_df <- df_MY_no %>%
  group_by(year = year(DATE)) %>%
  summarise(mean_value = mean(NBINDIV_per_trap),
            median_value = median(NBINDIV_per_trap))

df_MY_no$year <- as.factor(year(df_MY_no$DATE))
#jpeg("../../hist_pertrap_pernight_with_stats_cornflower_indiv_years.jpeg")
df_MY_no%>%
  ggplot(aes(NBINDIV_per_trap)) +
  geom_histogram(fill="cornflowerblue", binwidth = 50, col = "black", alpha= 0.7)+
  geom_vline(data = stats_df, aes(xintercept = mean_value, color = "mean"), linetype = "solid", size = 0.5)+ #adding descriptive stats
  geom_vline(data = stats_df, aes(xintercept = median_value, color = "median"), linetype = "solid", size = 0.5)+
  facet_wrap(~year)+ 
  ggtitle("Histogram of culicoides spp. counts per trap per night 2009-2012\n (outliers excluded)") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="indiv. per trap per night",
       y="Frequency")+
  scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"))+
  theme_minimal()

#dev.off()


##with eco_clim

#df_MY_no$year <- as.factor(year(df_MY_no$DATE))
# jpeg("../../hist_pertrap_pernight_with_stats_cornflower_indiv_years_eco_cli.jpeg")
# df_MY_no%>%
#   ggplot(aes(NBINDIV_per_trap)) +
#   geom_histogram(aes(fill=ECO_CLI), binwidth = 50, col = "black", alpha= 0.7)+
# #  geom_vline(data = stats_df, aes(xintercept = mean_value, color = "mean"), linetype = "solid", size = 0.5)+ #adding descriptive stats
# #  geom_vline(data = stats_df, aes(xintercept = median_value, color = "median"), linetype = "solid", size = 0.5)+
#   facet_wrap(~year)+ 
#   ggtitle("Histogram of culicoides spp. counts per trap per night 2009-2012\n (outliers excluded)") +
#   theme(plot.title = element_text(hjust = 0.5))+
#   labs(x="indiv. per trap per night",
#        y="Frequency",
#        fill="Ecoclimatic Zone")+
#   scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"))+
#   theme_minimal()
# 
# dev.off()

################
----------------------------------------------------------------------------------------------------------------
#temporal plots (essai 1)
----------------------------------------------------------------------------------------------------------------
  
df_MY_sum <- df_MY %>%
  group_by(DATE)%>%#,ECO_CLI) %>%
  summarise(NBINDIV_per_trap=sum(NBINDIV_per_trap), .groups="drop")
library(ggplot2) 

#jpeg("../../temporal_simple_cumulative.jpeg")

#temporal changes - cumulative daily samples (culicoides spp combined)
ggplot(df_MY_sum, aes(x=DATE, y=NBINDIV_per_trap))+ #,color=ECO_CLI)) +
  geom_line() +
#  facet_wrap(~ECO_CLI) +
  labs(title = "Cumulative Culicoides spp. counts 2009-2012 in France",
       x = "Timeline",
       y = "indiv. counted") +
  theme(plot.title = element_text(hjust = 0.5))+
  guides(color="none")+
  theme_minimal()
#dev.off()



-----------------
  df_MY_sum <- df_MY %>%
  group_by(DATE,ID_SITE)%>%#,ECO_CLI) %>%
  summarise(NBINDIV_per_trap=mean(NBINDIV_per_trap), .groups="drop")%>%
  group_by(DATE)%>%#,ECO_CLI) %>%
  summarise(samples=n(), NBINDIV_per_trap=sum(NBINDIV_per_trap)/samples, .groups="drop")

#jpeg("../../temporal_simple_per_trap_per_day_mean.jpeg")

#temporal changes - all trap average for each day
ggplot(df_MY_sum, aes(x=DATE, y=NBINDIV_per_trap))+ #,color=ECO_CLI)) +
  geom_line() +
  labs(title = "Trap sample mean of Culicoides counts/trap/day\n2009-2012 in France",
       x = "Timeline",
       y = "indiv. counted") +
  theme(plot.title = element_text(hjust = 0.5))+
  guides(color="none")+
  theme_minimal()
#dev.off()





  
----------------------------------------------------------------------------------------------------------------
####         Temporal plots average per trap (mean captures per individual trap per month, and the averaged across all sites)
----------------------------------------------------------------------------------------------------------------
df_short_avg <- df_MY %>%
  mutate(year = year(DATE),
        month = month(DATE)) %>%
 group_by(ID_SITE, year, month) %>% #ECO_CLI
summarise(n_collection_pm = n(), Avg_pt_pm = sum(NBINDIV_per_trap)/n_collection_pm, .groups="drop") %>% 
  group_by(year,month)%>%#this would give monthly avg. per unique trap per night #ECO_CLI
summarise(Avg_pt_pm = mean(Avg_pt_pm), .groups="drop") #this would give monthly avg. per trap per night (all traps combined)


#jpeg("../../temporal_month_avg_per_trap_bars.jpeg")
#df_short_avg$month <- as.numeric(df_short_avg$month)

ggplot(df_short_avg, aes(x = month, y = Avg_pt_pm))+ #color = ECO_CLI)) +    
 geom_line()+
#  geom_bar(stat = "identity", position = "stack", width = 0.9) +    # if i want bar chart
  facet_wrap(~year)+  
  labs(title = "Indiv. caught per trap per night (month's average)",
       x = "Month",
       y = "indiv./trap/night")+ #fill = "Ecoclimatic Zone")+
  scale_x_continuous(breaks = 1:12)
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
dev.off()
  


----------------------------------------------------------------------------------------------------------------
              ##making timeseries with ecocli avg per site per trap for the entire month 
 ----------------------------------------------------------------------------------------------------------------
  
df_MY<- df_short%>%
  group_by(DATE,ID_SITE,ECO_CLI) %>% 
  summarize(NBINDIV_per_trap=sum(NBINDIV), .groups="drop")
  
df_short_avg <- df_MY %>%
  mutate(year = year(DATE),
         month = month(DATE)) %>%
  group_by(ID_SITE, year, month,ECO_CLI) %>% 
  summarise(n_collection_pm = n(), Avg_pt_pm = sum(NBINDIV_per_trap)/n_collection_pm, .groups="drop") %>% 
  group_by(year,month,ECO_CLI)%>%
  summarise(Avg_pt_pm = mean(Avg_pt_pm), .groups="drop") 

ggplot(df_short_avg, aes(x = month, y = Avg_pt_pm, fill = ECO_CLI)) +    
#  geom_line()+
  geom_bar(stat = "identity", position = "stack", width = 0.9) +    
  facet_wrap(~year)+  
  labs(title = "Indiv. caught per trap per night (month's average)",
       x = "Month",
       y = "indiv./trap/night",
       fill ="Ecoclimatic Zone")+ 
  scale_x_continuous(breaks = 1:12)
theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()


  
  
  
  

