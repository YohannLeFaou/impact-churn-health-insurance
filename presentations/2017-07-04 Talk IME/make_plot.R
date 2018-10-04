
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  ## fonction qui permet de faire des images avec plusieurs graphiques avec ggplot2
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


make_plot_results = function(data_results){
  
  #Concordance
  stat_concordance = data_results %>% 
    group_by(algo, type_weights) %>%
    slice(which.max(mean_concordance))
  stat_concordance$model = gsub(" NA","",paste(stat_concordance$algo, stat_concordance$type_weights))
  
  plot_concordance = ggplot(data = data.frame(stat_concordance), aes(x = model, y = mean_concordance, group = 1)) +
    geom_line() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 10)) +
    ylab("Concordance")
  
  
  #R2
  stat_KM_R2 = data_results %>% 
    group_by(algo, type_weights) %>%
    slice(which.max(mean_KM_R2))
  stat_KM_R2$model = gsub(" NA","",paste(stat_KM_R2$algo, stat_KM_R2$type_weights))
  stat_KM_R2$mean_R2 = stat_KM_R2$mean_KM_R2
  stat_KM_R2$Weights = "KM"
  
  stat_Cox_R2 = data_results %>% 
    group_by(algo, type_weights) %>%
    slice(which.max(mean_Cox_R2))
  stat_Cox_R2$model = gsub(" NA","",paste(stat_Cox_R2$algo, stat_Cox_R2$type_weights))
  stat_Cox_R2$mean_R2 = stat_Cox_R2$mean_Cox_R2
  stat_Cox_R2$Weights = "Cox"
  
  stat_RSF_R2 = data_results %>% 
    group_by(algo, type_weights) %>%
    slice(which.max(mean_RSF_R2))
  stat_RSF_R2$model = gsub(" NA","",paste(stat_RSF_R2$algo, stat_RSF_R2$type_weights))
  stat_RSF_R2$mean_R2 = stat_RSF_R2$mean_RSF_R2
  stat_RSF_R2$Weights = "RSF"
  
  plot_R2 = ggplot( data = rbind(stat_KM_R2[,c("model","mean_R2","Weights")],
                                 stat_Cox_R2[,c("model","mean_R2","Weights")],
                                 stat_RSF_R2[,c("model","mean_R2","Weights")]) , 
                    aes(x = model, y = mean_R2, group = Weights, colour = Weights)) +
    geom_line() +
    ylab("R2") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, size = 10, vjust = 0.6),
          axis.title.x = element_blank())
  
  return(list(plot_concordance = plot_concordance, plot_R2 = plot_R2))
}


library(ggplot2)
library(dplyr)
setwd("~/Google Drive/missions forsides/Santiane 2016/weighted_RF_survival/présentations/2017-07-04Talk IME")

res_santiane_fct_cash_flow_2000obs_summary = read.csv("output_2017-07-01/res_santiane_fct_cash_flow_2000obs_summary.csv")
res_santiane_fct_cash_flow_10000obs_summary = read.csv("output_2017-07-01/res_santiane_fct_cash_flow_10000obs_summary.csv")
res_transplant_identite_summary = read.csv("output_2017-06-30/res_transplant_identite_summary.csv")

list_plots_santiane_fct_cash_flow = make_plot_results(data_results = res_santiane_fct_cash_flow_2000obs_summary)
list_plots_santiane_fct_cash_flow$plot_concordance
list_plots_santiane_fct_cash_flow$plot_R2

list_plots_santiane_fct_cash_flow_10000obs = make_plot_results(
  data_results = res_santiane_fct_cash_flow_10000obs_summary[-grep("w_gam",res_santiane_fct_cash_flow_10000obs_summary$algo),])
list_plots_santiane_fct_cash_flow_10000obs$plot_concordance + 
  theme(axis.title.y = element_text(size = 20)) +
  geom_line(size = 2)
list_plots_santiane_fct_cash_flow_10000obs$plot_R2 + 
  theme(axis.text.x = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  geom_line(size = 2)


list_plots_transplant_identite = make_plot_results(
  data_results = res_transplant_identite_summary[-grep("w_gam",res_transplant_identite_summary$algo),])
list_plots_transplant_identite$plot_concordance + 
  theme(axis.title.y = element_text(size = 20)) +
  geom_line(size = 2)
list_plots_transplant_identite$plot_R2 + 
  theme(axis.text.x = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  geom_line(size = 2)







