library(fst)
library(tidyverse)
library(lubridate)




data_na <- read.fst("../covariates.fst")

#removing NA values (introduced by adding climate data)
NBINDIV_na <- is.na(data_na$NBINDIV)
data <- subset(data_na, !NBINDIV_na)

#selecting columns of interest, resulting df has counts per espece de culicoides!
df_short <- as.data.frame(data) %>%
  select(ID_SITE, DATE, NBINDIV,ESPECE, ECO_CLI, longitude, latitude) %>%
#  filter(NBINDIV != 0) %>%
  mutate(DATE = as.Date(DATE))



#selecting columns of interest
df_short_all_species <- df_short %>%
  group_by(DATE,ID_SITE,ECO_CLI)%>%
  summarise(NBINDIV_all_sp=sum(NBINDIV), .groups="drop")#summing up all the different species collected per trap site to display as one sp

df_short_all_species$month <- month(df_short_all_species$DATE)
#jpeg("../../cul_boxplot_years_summarised_simple.jpeg")
##counts per trap per night during each month for different years with ecoclimatic zone
df_short_all_species %>%
  ggplot( aes(x=as.factor(month), y=NBINDIV_all_sp, fill= ECO_CLI)) +
  geom_boxplot(outliers= F)  +
  theme_minimal() +
  theme(
    legend.position="top",
    plot.title = element_text(size=13, hjust = 0.5)) + 
  ggtitle("Boxplot of Culicoides counts per trap per night\n (all species)") +
  xlab("month") +
  ylab("n of culicoides") +
  labs(fill="Ecoclimatic Zone")+
  facet_grid(~year(DATE) ~ECO_CLI)

#dev.off()
##############
#jpeg("../../cul_boxplot_years_separate_eco_cli.jpeg")
##counts per trap per night during each month for different years without ecoclimatic zone
df_short_all_species %>%
  ggplot( aes(x=factor(month), y=NBINDIV_all_sp, fill=ECO_CLI))+ 
  geom_boxplot(outliers= F)  +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=13, hjust = 0.5)) + 
  ggtitle("Boxplot of Culicoides counts per trap per night\n (all species)") +
  xlab("month") +
  ylab("n of culicoides") +
  labs(fill="Ecoclimatic Zone")+
 facet_grid(~ECO_CLI~year(DATE))

#dev.off()


