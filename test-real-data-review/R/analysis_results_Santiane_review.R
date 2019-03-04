
library(ggplot2)
library(reshape2)

setwd("~/Google Drive/GitHub/impact-churn-health-insurance/test-real-data-review")


print.myDF <- function(x, abbr = TRUE, minlength = 10, ...) {
  if (abbr) {
    names(x) <- abbreviate(names(x), minlength = minlength)
  }
  print.data.frame(x, ...)
}


make_plot_results_real = function(data_results, minleaf, maxdepth, max_w_mod,
                             plot_title1 = "", plot_title2 = "", criteria){


  to_plot = data_results[(is.na(data_results$minleaf) |
                                (data_results$minleaf == minleaf) ) &
                           (is.na(data_results$maxdepth) |
                              (data_results$maxdepth == maxdepth) ) &
                               ( (data_results$max_w_mod == max_w_mod) |
                                   is.na(data_results$max_w_mod)) &
                           (data_results$algo != "w_gam"), ]
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

  to_plot = to_plot[to_plot$model %in% c("swRF11", "swRF13","swRF22", "swRF32", "RSFr", "Cr",
                                         "RRTr", "RLTr"), ]

  to_plot2 = melt(data = to_plot, id.vars = "model", measure.vars = paste0(c("mean_KM_",
                                                                             "mean_Cox_",
                                                                             "mean_RSF_"
                                                                             #"mean_0_1_"
                                                                             ), criteria))

  #data.frame(to_plot2)


  plot_criteria = ggplot(data = to_plot2, aes(x = model, y = value,
                                     group = variable)) +
    geom_line(size = 1, aes(linetype = variable)) +
    geom_point(aes(shape = variable), size = 6) +
    ylab(criteria) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, size = 24, vjust = 0.6),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 24),
          axis.title.y = element_text(size = 24),
          title = element_text(size = 27),
          legend.title = element_text(size = 27),
          legend.text = element_text(size=24),
          legend.key.size = unit(1,"cm")
    ) +
    ggtitle(plot_title1) +
    scale_shape_discrete("", labels = c("KM weights", "Cox weights", "RSF weights")) +
    scale_linetype_discrete("", labels = c("KM weights", "Cox weights", "RSF weights"))

  plot_C_index = ggplot(data = to_plot,
                        aes(x = model, y = mean_concordance, group = 1)) +
    geom_line(size = 1) +
    ylab("C-index") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, size = 24, vjust = 0.6),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 24),
          axis.title.y = element_text(size = 24),
          title = element_text(size = 27),
          legend.title = element_text(size = 27),
          legend.text = element_text(size=24),
          legend.key.size = unit(1,"cm")
    ) +
    ggtitle(plot_title2)

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



make_plot_detail_results_real = function(data_results, maxdepth, max_w_mod,
                                         plot_title = "", criteria){
  to_plot1 = data_results[(data_results$algo != "w_gam"), ]
  to_plot = to_plot1[(is.na(to_plot1$maxdepth) |
                        (to_plot1$maxdepth == maxdepth) ) &
                       ( (to_plot1$max_w_mod == max_w_mod) |
                           is.na(to_plot1$max_w_mod)) , ]
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
                         levels = c("wRF1_KM", "wRF1_Cox",
                                    "wRF1_RSF",
                                    "wRF2_KM", "wRF2_Cox",
                                    "wRF3_KM", "wRF3_Cox",
                                    "rsf_reg", "cox_reg",
                                    "rrt_reg", "rlt_reg",
                                    "rlt_reg_no_RL"),
                         labels = c("swRF11", "swRF12", "swRF13",
                                    "swRF21", "swRF22",
                                    "swRF31", "swRF32",
                                    "RSFr", "Cr", "RRTr",
                                    "RLTr", "nRLTr"))

  to_plot = to_plot[to_plot$model %in% c("swRF11", "swRF13", "swRF22", "swRF32", "RSFr", "Cr",
                                         "RRTr", "RLTr"), ]

  to_plot$col_criteria = to_plot[,criteria]

  scatter_plot = ggplot(data = to_plot, aes(x = model, y = col_criteria,
                                            group = iter)) +
    geom_line(size = 1, alpha = 0.3) +
    #geom_point(aes(shape = interaction(target_R2_C, prop_censored)), size = 4) +
    ylab(criteria) +
    theme(#legend.position = "bottom",
      axis.text.x = element_text(angle = 45, size = 10, vjust = 0.6),
      axis.title.x = element_blank()) +
    ggtitle(plot_title)
  # scale_shape_discrete("Parameters (prop.censored, R2_C)",
  #                      labels  = c("0.3, 0.05", "0.3, 0.1",
  #                                  "0.5, 0.05", "0.5, 0.1")
  #                      # labels  = c("0.1, 0.05", "0.1, 0.2",
  #                      #             "0.3, 0.05", "0.3, 0.2",
  #                      #             "0.5, 0.05", "0.5, 0.2")
  # )

  boxplot = ggplot(data = to_plot, aes(x = model, y = col_criteria,
                                       group = model)) +
    geom_boxplot(outlier.size = 6) +
    #geom_line(size = 1, alpha = 0.3) +
    #geom_point(aes(shape = interaction(target_R2_C, prop_censored)), size = 4) +
    ylab(criteria) +
    theme(axis.text.x = element_text(angle = 45, size = 24, vjust = 0.6),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 24),
          axis.title.y = element_text(size = 24),
          title = element_text(size = 27)) +
    ggtitle(plot_title)

  return(list(scatter_plot = scatter_plot,
              boxplot = boxplot))
}




make_rank = function(data_results, minleaf, maxdepth, max_w_mod,
                     criteria, minimise){

  to_plot = data_results[(is.na(data_results$minleaf) |
                            (data_results$minleaf == minleaf) ) &
                           (is.na(data_results$maxdepth) |
                              (data_results$maxdepth == maxdepth) ) &
                           ( (data_results$max_w_mod == max_w_mod) |
                               is.na(data_results$max_w_mod)) &
                           (data_results$algo != "w_gam"), ]

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
                         levels = c("wRF1_KM", "wRF1_Cox",
                                    "wRF1_RSF",
                                    "wRF2_KM", "wRF2_Cox",
                                    "wRF3_KM", "wRF3_Cox",
                                    "rsf_reg", "cox_reg",
                                    "rrt_reg", "rlt_reg",
                                    "rlt_reg_no_RL"),
                         labels = c("swRF11", "swRF12", "swRF13",
                                    "swRF21", "swRF22",
                                    "swRF31", "swRF32",
                                    "RSFr", "Cr", "RRTr",
                                    "RLTr", "nRLTr"))

  to_plot$col_criteria = to_plot[,criteria]

  if (minimise){
    to_plot = to_plot %>% group_by(iter) %>% mutate(rank_model = rank(col_criteria, ties.method = 'first'))
  } else {
    to_plot = to_plot %>% group_by(iter) %>% mutate(rank_model = rank(-col_criteria, ties.method = 'first'))
  }

  results = to_plot %>%
    group_by(model) %>%
    summarise(mean_rank = mean(rank_model))

  return(data.frame(results))
}




res_santiane_fct_cash_flow_5000_summary = read.csv2("output_2018-12-12/res_santiane_fct_cash_flow_5000_summary.csv")
res_santiane_log_5000_summary = read.csv2("output_2018-12-12/res_santiane_log_5000_summary.csv")
res_santiane_identite_5000_summary = read.csv2("output_2018-12-12/res_santiane_identite_5000_summary.csv")
res_santiane_indicatrice380_5000_summary = read.csv2("output_2018-12-12/res_santiane_indicatrice380_5000_summary.csv")



maxdepth = 5
minleaf = 100
max_w_mod = 50




list_plot_santiane_fct_cash_flow =
  make_plot_results_real(data_results = res_santiane_fct_cash_flow_5000_summary,
                  minleaf = minleaf,
                  maxdepth = maxdepth,
                  max_w_mod = max_w_mod,
                  plot_title1 = "phi = churn factor (Fig. 2)",
                  plot_title2 = "",
                  criteria = "mse")

list_plot_res_santiane_log =
  make_plot_results_real(data_results = res_santiane_log_5000_summary,
                    minleaf = minleaf,
                    maxdepth = maxdepth,
                    max_w_mod = max_w_mod,
                    plot_title1 = "phi(t) = log(t+1)",
                    plot_title2 = "",
                    criteria = "mse")

list_plot_res_santiane_identite =
  make_plot_results_real(data_results = res_santiane_identite_5000_summary,
                    minleaf = minleaf,
                    maxdepth = maxdepth,
                    max_w_mod = max_w_mod,
                    plot_title1 = "phi(t) = t",
                    plot_title2 = "",
                    criteria = "mse")

list_plot_res_santiane_indicatrice380 =
  make_plot_results_real(data_results = res_santiane_indicatrice380_5000_summary,
                    minleaf = minleaf,
                    maxdepth = maxdepth,
                    max_w_mod = max_w_mod,
                    plot_title1 = "phi(t) = 1_{t > 380}",
                    plot_title2 = "",
                    criteria = "mse")

# multiplot(list_plot_res_santiane_indicatrice375$plot_criteria,
#           list_plot_res_santiane_indicatrice375$plot_C_index,
#           cols=1)


multiplot(list_plot_santiane_fct_cash_flow$plot_criteria + theme(legend.position = "None") + ylab("MSE"),
          list_plot_res_santiane_log$plot_criteria + theme(legend.position = "None") + ylab("MSE"),
          list_plot_res_santiane_identite$plot_criteria + theme(legend.position = "None") + ylab("MSE"),
          list_plot_res_santiane_indicatrice380$plot_criteria + theme(legend.position = "None") + ylab("MSE"),#
          list_plot_santiane_fct_cash_flow$plot_C_index,
          list_plot_res_santiane_log$plot_C_index,
          list_plot_res_santiane_identite$plot_C_index,
          list_plot_res_santiane_indicatrice380$plot_C_index,
          cols = 2)


# --------------------------------------------------------------------------------------
#                          scatter plot of the results
# --------------------------------------------------------------------------------------

res_santiane_fct_cash_flow_5000 = read.csv2("output_2018-12-12/res_santiane_fct_cash_flow_5000.csv")
#res_santiane_log_5000 = read.csv2("output_2017-10-24/res_santiane_log_5000.csv")
#res_santiane_identite_5000 = read.csv2("output_2017-10-24/res_santiane_identite_5000.csv")



list_detail_plot_fct_cash_flow = make_plot_detail_results_real(data_results = res_santiane_fct_cash_flow_5000,
                                              maxdepth = 5,
                                              max_w_mod = 50,
                                              plot_title = "phi = churn factor (Fig. 2)",
                                              criteria = "criteria_weighted.RSF_mse")

# plot_indiv2 = make_scatter_plot_results_real(data_results = res_santiane_log_5000,
#                                               maxdepth = 5,
#                                               max_w_mod = 50,
#                                               plot_title = "res_santiane_log_5000",
#                                               criteria = "criteria_weighted.RSF_mse")
#
# plot_indiv3 = make_scatter_plot_results_real(data_results = res_santiane_identite_5000,
#                                               maxdepth = 5,
#                                               max_w_mod = 50,
#                                               plot_title = "res_santiane_identite_5000",
#                                               criteria = "criteria_weighted.RSF_mse")

multiplot(list_detail_plot_fct_cash_flow$boxplot ,
          list_detail_plot_fct_cash_flow$scatter_plot,
          cols = 2)

list_detail_plot_fct_cash_flow$boxplot + ylab("MSE") + geom_boxplot(outlier.size = 2) + geom_boxplot(lwd=1) + ggtitle("")

# ------------------------------------------------------------
#              ranks of the models
# ------------------------------------------------------------

res_santiane_rank_models_fct_cash_flow_5000 =
  make_rank(data_results = res_santiane_fct_cash_flow_5000,
            maxdepth = 5,
            minleaf = 100,
            max_w_mod = 50,
            criteria = "criteria_weighted.RSF_mse",
            minimise = T)

write.csv2(x = res_santiane_rank_models_fct_cash_flow_5000,
          file = "images_2018-12-12/res_santiane_rank_models_fct_cash_flow_5000_new.csv",
          row.names = F)

