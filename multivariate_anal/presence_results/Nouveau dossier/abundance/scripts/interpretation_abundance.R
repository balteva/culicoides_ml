library(tidyverse) 
library(iml) 
library(patchwork) 
library(precrec) 
library(lubridate)


#forgot to add eco_cli column, doing it now

grid <- read.csv("../../data/culicoides_point_locations_grid_150_150.csv")%>%
  select(ID_SITE, ECO_CLI)

#cv with location LLO
multiv_model_abundance_LLO <- readRDS("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/abundance_results/res_multiv_model_abundance_LLO.rds")


#CV with LTO
multiv_model_abundance_LTO <- readRDS("C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/abundance_results/res_multiv_model_abundance_LTO.rds")


#results of LLO
model_abundance_LLO <- multiv_model_abundance_LLO[[1]] #### sum up of presence model
df_cv_abundance_LLO <- multiv_model_abundance_LLO[[2]] %>% #### data frame with prediction 
  left_join(grid)
df_mod_abundance_LLO<- multiv_model_abundance_LLO[[3]] #### data frame which was used to build the model

##results of LTO
df_cv_abundance_LTO <- multiv_model_abundance_LTO[[2]] %>% #### data frame with prediction 
  left_join(grid)

# ploting with LLO CV
plot_eval_abundance_model <- df_cv_abundance_LLO %>%
  mutate(obs=exp(obs),pred=exp(pred)) %>%
  mutate(year=year(as.Date(DATE)), week=week(as.Date(DATE)))%>%
  dplyr::group_by(ECO_CLI, week, year) %>%    ## to sum up, grouping by trap, location and num session 
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
  ggplot(aes(x=week, y = value, color = name)) +
  #geom_point() + 
  geom_line(linewidth=1) + 
  facet_grid(.~ECO_CLI~year, scales = "free_y") + 
  theme_bw() + 
  scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F) + 
  #scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  xlab("week") +
  ylab("mean abundance") + 
  labs(color='Number of C. Obsoletux/Scoticus') + 
  theme(legend.position="bottom") + 
  ggtitle('Abundance models : observed vs. predicted values (LLO CV)')

ggsave(filename = "C:/Users/ibalt/OneDrive/Desktop/uni/M2 stage/Cullicoides_data/CIRAD/culicoides_ml/multivariate_anal/abundance_results/abundance_evaluation_LLO.pdf",plot =plot_eval_abundance_model, device = "pdf", width = 11, height = 8) ## to save
