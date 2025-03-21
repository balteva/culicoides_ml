
#### Second step: Model validation plots: ROC 
roc_plot_func <- function(df, var){
  
  roc_plots <-  list()
  
  df_new <- df %>%
    mutate(!!sym(var) := "ALL ZONES") %>%
    rbind(df)  
  
  
  for (i in unique(df_new[[var]])) {  
    df_temp <- df_new %>%
      filter(df_new[[var]] == i)
    
    LLTO_scores <- precrec::join_scores(df_temp$pred, df_temp$pred_year)
    
    i_mmdat <- mmdata(LLTO_scores, df_temp$obs, modnames=c("LLO", "LTO"))
    AUC_i_LLO <- MLmetrics::AUC(df_temp$pred, df_temp$obs)
    AUC_i_LTO <- MLmetrics::AUC(df_temp$pred_year, df_temp$obs)
    i_obj <- precrec::evalmod(i_mmdat)
    roc_plot <- autoplot(i_obj, curvetype= c("ROC"))+
      geom_line(linewidth=1)+
      ggtitle(paste0("Presence model ROC curve for ", i, "\n LLO AUC = ", round(AUC_i_LLO, 2), "\n LTO AUC = ", round(AUC_i_LTO, 2), "")) + 
      theme(axis.title.x = element_text(size = 10, face="bold"),
            axis.title.y = element_text(size = 10, face="bold"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5, size= 10, face='bold'))
    roc_plots[[i]] <- roc_plot
  }
  
  return(roc_plots)
}
roc_plots <- roc_plot_func(df_fin_cv, "ECO_CLI")
roc_plots_all <- patchwork::wrap_plots(roc_plots, ncol = 3) + plot_annotation(title = "Presence Model Evaluation Per Ecoregion")


