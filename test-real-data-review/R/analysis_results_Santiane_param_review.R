
library(ggplot2)
library(reshape2)

setwd("~/Google Drive/GitHub/impact-churn-health-insurance/test-real-data-review")


print.myDF <- function(x, abbr = TRUE, minlength = 10, ...) {
  if (abbr) {
    names(x) <- abbreviate(names(x), minlength = minlength)
  }
  print.data.frame(x, ...)
}


make_plot_results_real = function(data_results, max_w_mod, #minleaf, maxdepth
                                  plot_title1 = "", plot_title2 = "", criteria, list_models){

  to_plot = data_results
  to_plot$type_w_RF = ifelse(to_plot$algo == "w_RF1", 1,
                             ifelse( (to_plot$algo == "w_RF2") & (to_plot$type_pred == "weights_tree"), 2,
                                     ifelse((to_plot$algo == "w_RF2") & (to_plot$type_pred == "KM_local"), 3, "")))
  to_plot$model = paste0(sapply(X = as.character(to_plot$algo),
                                FUN = function(x){
                                  if (x == "w_RF1") x = "wRF"
                                  if (x == "w_RF2") x = "wRF"
                                  return(x)
                                }),
                         as.character(to_plot$type_w_RF),
                         ifelse(is.na(as.character(to_plot$type_w)), "", paste0("_",as.character(to_plot$type_w)))
  )

  to_plot$model = factor(x = to_plot$model,
                         levels = c("wRF1_KM", "wRF1_Cox", "wRF1_RSF",
                                    "wRF2_KM", "wRF2_Cox",
                                    "wRF3_KM", "wRF3_Cox",
                                    "rsf_reg", "cox_reg",
                                    "rrt_reg",
                                    "rlt_reg", "rlt_reg_no_RL"),
                         labels = c("swRF11", "swRF12",
                                    "swRF13",
                                    "swRF21", "swRF22",
                                    "swRF31", "swRF32",
                                    "RSFr", "Cr",
                                    "RRTr",
                                    "RLTr", "nRLTr"))

  to_plot = to_plot[to_plot$model %in% list_models, ]
  to_plot$col_criteria = to_plot[,criteria]

  to_plot$minleaf_maxdepth = factor(paste0(to_plot$minleaf, ", ", to_plot$maxdepth),
                                    levels = c("50, 10", "100, 10", "100, 5",
                                               "200, 10", "500, 10"),
                                    labels = c("ms=50, md=10", "100, 10", "100, 5",
                                               "200, 10", "500, 10"))

  if (length(list_models) == 4){
    shape_values = 1:4
  } else{
    shape_values = 5:7
  }

  #shape_values = 1:length(list_models)

  plot_criteria = ggplot(data = to_plot,
                         aes(x = minleaf_maxdepth, y = col_criteria, group = model)) +
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
    scale_shape_manual("model", values=shape_values)

  plot_C_index = ggplot(data = to_plot,
                        aes(x = minleaf_maxdepth, y = mean_concordance, group = model)) +
    geom_line(size = 1) +
    geom_point(aes(shape = model), size = 6) +
    ylab("C-index") +
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
    scale_shape_manual("model", values=shape_values)

  return(list(plot_criteria = plot_criteria,
              plot_C_index = plot_C_index,
              to_plot = to_plot))
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


res_santiane_fct_cash_flow_5000_summary = read.csv2("output_2018-12-12/res_santiane_fct_cash_flow_5000_summary.csv")

res_santiane_fct_cash_flow_param_5000_summary = read.csv2("output_2019_03_27_param/res_santiane_fct_cash_flow_param_5000_summary.csv")

res_santiane_fct_cash_flow_param_summary_tot =
  rbind(res_santiane_fct_cash_flow_5000_summary[is.na(res_santiane_fct_cash_flow_5000_summary$max_w_mod) |
                                                  (res_santiane_fct_cash_flow_5000_summary$max_w_mod == 50), ],
        res_santiane_fct_cash_flow_param_5000_summary)




list_plot_santiane_fct_cash_flow1 =
  make_plot_results_real(data_results = res_santiane_fct_cash_flow_param_summary_tot,
                         plot_title1 = "phi = churn factor (Fig. 2)",
                         plot_title2 = "",
                         criteria = "mean_RSF_mse",
                         list_models = c("swRF11", "swRF13","swRF22"))



list_plot_santiane_fct_cash_flow2 =
  make_plot_results_real(data_results = res_santiane_fct_cash_flow_param_summary_tot,
                         plot_title1 = "phi = churn factor (Fig. 2)",
                         plot_title2 = "",
                         criteria = "mean_RSF_mse",
                         list_models = c("swRF32", "RSFr", "RRTr", "RLTr"))


multiplot(list_plot_santiane_fct_cash_flow1$plot_criteria + theme(legend.position = "None") +
            ylab("MSE") + ylim(c(0.043, 0.0457)),
          list_plot_santiane_fct_cash_flow1$plot_C_index + theme(legend.position = "bottom") + ylim(c(0.529, 0.5635)),
          list_plot_santiane_fct_cash_flow2$plot_criteria + theme(legend.position = "None") + ylab("MSE") + ylim(c(0.043, 0.0457)),
          list_plot_santiane_fct_cash_flow2$plot_C_index + theme(legend.position = "bottom") + ylim(c(0.529, 0.5635)),
          cols = 2)


#rbind(list_plot_santiane_fct_cash_flow1$to_plot, list_plot_santiane_fct_cash_flow2$to_plot)
