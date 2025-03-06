
###
plot_eval_presence_model_LLO <- df_cv_presence_LLO %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE), week=lubridate::week(DATE))%>%
  #filter(week >)%>%
  dplyr::group_by(ECO_CLI,year,week) %>%   
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>% ## to sum up, grouping by trap, location and num session 
  mutate(Prediction= if_else(pred >= 0.5, "Presence", "Absence"))%>%
  mutate(Observation= if_else(obs == 1, "Presence", "Absence"))%>%
  as_tibble() %>%
  pivot_longer(cols=c('Prediction','Observation'), names_to="Type", values_to = "PresenceStatus") %>%
  ggplot(aes(x=week, y = year, fill = PresenceStatus)) +
  geom_tile(aes(width = 0.5, height=0.5)) +
  facet_wrap(~ECO_CLI, scales = "free") + 
  theme_bw() + 
  scale_fill_manual(values=c("Presence"="red","Absence"= "blue"), labels = c("Presence", "Absence"), name = "Presence Status")+
  xlab("week") +
  ylab("year") + 
  labs(color='Predictions of Culicoides Obsoletus/Scoticus Presebce by ecoclimatic zone (LLO CV)') + 
  theme(legend.position="bottom") + 
  ggtitle('Presence models : observed vs. predicted values')




#############
 df_cv_presence_LLO %>%
  mutate(DATE=as.Date(DATE))%>%
  mutate(year=lubridate::year(DATE), week=lubridate::week(DATE))%>%
  dplyr::group_by(ECO_CLI,year,week) %>% 
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>% ## to sum up, grouping by trap, location and num session 
  mutate(Prediction = if_else(pred >= 0.5, "1", "0"), Observation = if_else(obs >= 0.5, "1", "0"))%>%
  as_tibble() %>%
  pivot_longer(cols=c('Prediction','Observation'), names_to="Type", values_to = "PresenceStatus") %>%
  ggplot(aes(x=week, y = Type, fill = PresenceStatus)) +
  geom_tile()+
  coord_fixed(ratio =4) +
  facet_grid(~ECO_CLI~year, scales="fixed") + 
  theme_minimal() + 
  scale_x_continuous(breaks = seq(2, 52, 4)) +
  scale_fill_manual(values = c("0" = "#4682B4", "1" = "#FF6347"), labels =c("Absence", "Presence"))+
  xlab("week") +
  labs(fill="Status") + 
  theme(legend.position="bottom") + 
  ggtitle("Predictions of Culicoides Obsoletus/Scoticus presence by ecoclimatic zone (LLO CV)")




