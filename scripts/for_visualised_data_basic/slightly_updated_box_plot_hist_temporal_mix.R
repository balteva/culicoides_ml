#selecting columns of interest
df_short_all_species <- df_short %>%
  group_by(DATE,ID_SITE,ECO_CLI)%>%
  summarise(NBINDIV_all_sp=sum(NBINDIV), .groups="drop")#summing up all the different species collected per trap site to display as one sp
#write_csv(df_short_all_species, "../../df_short_all_species.csv")


df_short_all_species$month <- month(df_short_all_species$DATE)

##counts per trap per night during each month for different years with ecoclimatic zone
df_short_all_species %>%
  ggplot( aes(x=as.factor(year(DATE)), y=NBINDIV_all_sp, fill= ECO_CLI)) +
  geom_boxplot(outliers= F)  +
  theme_minimal() +
  theme(
    legend.position="top",
    plot.title = element_text(size=13, hjust = 0.5)) + 
  ggtitle("Boxplot of Culicoides counts per trap per night\n (all species)") +
  xlab("year") +
  ylab("n of culicoides") +
  labs(fill="Ecoclimatic Zone")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))#+
#  facet_wrap(~year(DATE), scales="free")

#ggsave("../../boxplot_ptpm_simple_year_separate_ecocli.jpeg", dpi = 300, width = 10, height = 7, units = "in")


----------------------------------------------------------------------------------------------------------------
  #temporal plots (essai 1)
  ----------------------------------------------------------------------------------------------------------------
  
  df_MY_sum <- df_short_all_species %>%
  group_by(DATE) %>%
  summarise(NBINDIV_per_trap=sum(NBINDIV_all_sp), .groups="drop")




#temporal changes - cumulative daily samples (culicoides spp combined)
ggplot(df_MY_sum, aes(x=DATE, y=NBINDIV_per_trap))+#color=ECO_CLI)) +
geom_line() +
  #  facet_wrap(~ECO_CLI) +
  labs(title = "Cumulative daily Culicoides spp. counts 2009-2012 in France",
       x = "Timeline",
       y = "indiv. counted") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  guides(color="none")+
  theme_minimal()
ggsave("../../temporal_simple_cumulative.jpeg", dpi = 300, width = 10, height = 7, units = "in")




-----------------
  df_MY_sum <- df_short_all_species %>%
  group_by(DATE,ID_SITE,ECO_CLI) %>%
  summarise(NBINDIV_per_trap=mean(NBINDIV_all_sp), .groups="drop")%>%
  group_by(DATE,ECO_CLI) %>%
  summarise(samples=n(), NBINDIV_per_trap=sum(NBINDIV_per_trap)/samples, .groups="drop")






#temporal changes - all trap average for each day
ggplot(df_MY_sum, aes(x=DATE, y=NBINDIV_per_trap,color=ECO_CLI)) +
  geom_line(linewidth=0.5) +
  labs(title = "Culicoides counts/trap/day\n2009-2012 in France",
       x = "Timeline",
       y = "indiv. counted") +
  guides(color="none")+
  facet_wrap(~ECO_CLI)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

#ggsave("../../temporal_eco_clim_average.jpeg", dpi = 300, width = 10, height = 7, units = "in")






----------------------------------------------------------------------------------------------------------------
  ####         Temporal plots average per trap (mean captures per individual trap per month, and the averaged across all sites)
  ----------------------------------------------------------------------------------------------------------------
  df_short_avg <- df_short_all_species %>%
  mutate(year = year(DATE),
         month = month(DATE)) %>%
  group_by(ECO_CLI,ID_SITE, year, month) %>% #ECO_CLI
  summarise(n_collection_pm = n(), Avg_pt_pm = sum(NBINDIV_all_sp)/n_collection_pm, .groups="drop") %>% 
  group_by(ECO_CLI,year,month)%>%#this would give monthly avg. per unique trap per night #ECO_CLI
  summarise(Avg_pt_pm = mean(Avg_pt_pm), .groups="drop") #this would give monthly avg. per trap per night (all traps combined)


#jpeg("../../temporal_month_avg_per_trap_eco_cli_bars.jpeg")
df_short_avg$month <- as.numeric(df_short_avg$month)

ggplot(df_short_avg, aes(x = month, y = Avg_pt_pm, fill = ECO_CLI)) +    
  # geom_line(linewidth=0.6)+
  geom_bar(stat = "identity", position = "stack", width = 0.9) +    # if i want bar chart
  labs(title = "Indiv. caught per trap per night (month's average)",
       x = "Month",
       y = "indiv./trap/night",fill = "Ecoclimatic Zone")+
  scale_x_continuous(breaks = 1:12)+
  facet_wrap(~year)+
  theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          legend.position="top")
     
  ggsave("../../temporal_month_avg_per_trap_eco_cli_temporal_bars_top_wrap.jpeg", dpi = 300, width = 10, height = 7, units = "in")




----------------------------------------------------------------------------------------------------------------
  ##making timeseries with ecocli avg per site per trap for the entire month 
  ----------------------------------------------------------------------------------------------------------------
  
  df_MY<- df_short_all_species%>%
  group_by(DATE,ID_SITE,ECO_CLI) %>% 
  summarize(NBINDIV_per_trap=sum(NBINDIV_all_sp), .groups="drop")

df_short_avg <- df_MY %>%
  mutate(year = year(DATE),
         month = month(DATE)) %>%
  group_by(ID_SITE, year, month,ECO_CLI) %>% 
  summarise(n_collection_pm = n(), Avg_pt_pm = sum(NBINDIV_per_trap)/n_collection_pm, .groups="drop") %>% 
  group_by(year,month,ECO_CLI)%>%
  summarise(Avg_pt_pm = mean(Avg_pt_pm), .groups="drop") 

ggplot(df_short_avg, aes(x = month, y = Avg_pt_pm, color = ECO_CLI)) +    
  geom_line()+
  #  geom_bar(stat = "identity", position = "stack", width = 0.9) +    
  facet_wrap(~year)+  
  labs(title = "Indiv. caught per trap per night (month's average)",
       x = "Month",
       y = "indiv./trap/night",
       color ="Ecoclimatic Zone")+ 
  scale_x_continuous(breaks = 1:12)
theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
dev.off()






