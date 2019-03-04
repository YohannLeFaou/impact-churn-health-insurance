
library(ggplot2)
library(reshape2)
library(xtable)
library(dplyr)

setwd("~/Google Drive/GitHub/impact-churn-health-insurance/test-simulated-data-review")

# ----------------------------------------------------------------
#                   Functions
# ----------------------------------------------------------------

make_plot_results_simul = function(data_results, prop_censored, plot_title = "",
                                   criteria, list_models, data_ranks = NULL){

 # data_results_one = data_results[(data_results$prop_censored == prop_censored &
#                                     data_results$target_R2_C == target_R2_C, ]
  # to_plot = data_results_one[(is.na(data_results_one$maxdepth) |
  #                               (data_results_one$maxdepth == maxdepth) ) &
  #                              ( (data_results_one$max_w_mod == max_w_mod) |
  #                                  is.na(data_results_one$max_w_mod)) , ]
  to_plot = data_results[data_results$prop_censored == prop_censored, ]
  to_plot$type_w_RF = ifelse(to_plot$algo == "w_RF1", 1,
                             ifelse( (to_plot$algo == "w_RF2") & (to_plot$type_pred == "weights_tree"), 2,
                                     ifelse((to_plot$algo == "w_RF2") & (to_plot$type_pred == "KM_local"), 3, "")))
  to_plot$model = paste0(sapply(X = as.character(to_plot$algo),
                                FUN = function(x){
                                  if (x == "w_RF1") x = "wRF"
                                  if (x == "w_RF2") x = "wRF"
                                  if (x == "RF_bench") x = "RF"
                                  return(x)
                                }),
                         as.character(to_plot$type_w_RF),
                         ifelse(is.na(as.character(to_plot$type_w)), "", paste0("_",as.character(to_plot$type_w)))
  )

  to_plot$model = factor(x = to_plot$model,
                         levels = c("RF",
                                    "wRF1_KM", "wRF1_Cox", "wRF1_RSF", "wRF1_theo",
                                    "wRF2_KM", "wRF2_Cox", "wRF2_theo",
                                    "wRF3_KM", "wRF3_Cox", "wRF3_theo",
                                    "rsf_reg", "cox_reg",
                                    "rrt_reg",
                                    "rlt_reg", "rlt_reg_no_RL"),
                         labels = c("RF",
                                    "swRF11", "swRF12", "swRF13", "swRF14",
                                    "swRF21", "swRF22", "swRF24",
                                    "swRF31", "swRF32", "swRF34",
                                    "RSFr", "Cr",
                                    "RRTr",
                                    "RLTr", "nRLTr"))

  to_plot = to_plot[to_plot$model %in% list_models, ]

  to_plot$col_criteria = to_plot[,criteria]
  to_plot = to_plot[to_plot$minleaf != 300, ]
  to_plot$minleaf_maxdepth = factor(paste0(to_plot$minleaf, ", ", to_plot$maxdepth),
                                    levels = c("10, 10", "20, 10", "50, 10",
                                               "50, 4", "100, 10", "200, 10"),
                                    labels = c("ms=10, md=10", "20, 10", "50, 10",
                                               "50, 4", "100, 10", "200, 10"))

  if (length(list_models) == 5){
    shape_values = 1:5
  } else{
    shape_values = 6:9
  }

  if (is.null(data_ranks)){
    plot = ggplot(data = to_plot, aes(x = minleaf_maxdepth, y = col_criteria, #interaction(maxdepth, minleaf)
                                      group = model)) + #interaction(target_R2_C, prop_censored)
      geom_line(size = 1) +
      geom_point(aes(shape = model), size = 6) + #interaction(target_R2_C, prop_censored)
      ylab(criteria) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 30, size = 24, vjust = 0.6),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 24),
            axis.title.y = element_text(size = 24),
            title = element_text(size = 27),
            legend.title = element_text(size = 27),
            legend.text = element_text(size=24),
            legend.key.size = unit(1,"cm")
      ) +
      ggtitle(plot_title) +
      scale_shape_manual("model",
                         values=shape_values)
  #   +
  #     scale_x_discrete(labels = c("ms=10, md=10", "20, 10", "50, 4",
  #                                 "50, 10", "100, 10", "200, 10"))
    }
  return(list(plot = plot, to_plot = to_plot))
}



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



# ------------------------------------------------------------
#              Graphics
# ------------------------------------------------------------


## make plots

# plot1
res_simul_weibull_log_1000_param_sensi_100_iter_summary_new = read.csv2("output_2019-02-20_param/res_simul_weibull_log_1000_param_sensi_100_iter_summary_new.csv")

plot1 = make_plot_results_simul(data_results = res_simul_weibull_log_1000_param_sensi_100_iter_summary_new,
                                plot_title = "q = 0.1",
                                criteria = "mean_mse",
                                prop_censored = 0.1,
                                list_models = c("RF", "swRF13", "swRF32", "RSFr", "RRTr")
                                )

plot2 = make_plot_results_simul(data_results = res_simul_weibull_log_1000_param_sensi_100_iter_summary_new,
                                plot_title = "q = 0.3",
                                criteria = "mean_mse",
                                prop_censored = 0.3,
                                list_models = c("RF", "swRF13", "swRF32", "RSFr", "RRTr")
)

plot3 = make_plot_results_simul(data_results = res_simul_weibull_log_1000_param_sensi_100_iter_summary_new,
                                plot_title = "q = 0.5",
                                criteria = "mean_mse",
                                prop_censored = 0.5,
                                list_models = c("RF", "swRF13", "swRF32", "RSFr", "RRTr")
)

plot4 = make_plot_results_simul(data_results = res_simul_weibull_log_1000_param_sensi_100_iter_summary_new,
                                plot_title = "q = 0.1",
                                criteria = "mean_mse",
                                prop_censored = 0.1,
                                list_models = c("swRF11", "swRF22", "swRF34", "RLTr")
)

plot5 = make_plot_results_simul(data_results = res_simul_weibull_log_1000_param_sensi_100_iter_summary_new,
                                plot_title = "q = 0.3",
                                criteria = "mean_mse",
                                prop_censored = 0.3,
                                list_models = c("swRF11", "swRF22", "swRF34", "RLTr")
)

plot6 = make_plot_results_simul(data_results = res_simul_weibull_log_1000_param_sensi_100_iter_summary_new,
                                plot_title = "q = 0.5",
                                criteria = "mean_mse",
                                prop_censored = 0.5,
                                list_models = c("swRF11", "swRF22", "swRF34", "RLTr")
)


multiplot(plot1$plot + ylab("MSE") + theme(legend.position = "None") + ylim(c(1.397, 1.46)),#
          plot2$plot + ylab("MSE") + theme(legend.position = "None") + ylim(c(1.406, 1.484)),
          plot3$plot + ylab("MSE") + theme(legend.position = "None") + ylim(c(1.4, 1.63)),
          plot4$plot + ylab("MSE") + theme(legend.position = "None") + ylim(c(1.397, 1.46)),
          plot5$plot + ylab("MSE") + theme(legend.position = "None") + ylim(c(1.406, 1.484)),
          plot6$plot + ylab("MSE") + theme(legend.position = "None") + ylim(c(1.4, 1.63)),
          cols = 2)

