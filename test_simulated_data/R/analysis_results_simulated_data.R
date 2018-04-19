
library(ggplot2)
library(reshape2)
library(xtable)

setwd("~/Google Drive/GitHub/resultats_sword")

print.myDF <- function(x, abbr = TRUE, minlength = 10, ...) {
  if (abbr) {
    names(x) <- abbreviate(names(x), minlength = minlength)
  }
  print.data.frame(x, ...)
}



make_plot_results_simul = function(data_results, maxdepth, max_w_mod, plot_title = "", criteria, data_ranks = NULL){

  to_plot = do.call(rbind,
                    lapply(X = 1:dim(unique(data_results[,c("prop_censored", "target_R2_C")]))[1],
                           FUN = function(i){
                             prop_censored = unique(data_results[,c("prop_censored", "target_R2_C")])[i,"prop_censored"]
                             target_R2_C = unique(data_results[,c("prop_censored", "target_R2_C")])[i,"target_R2_C"]

                             data_results_one = data_results[(data_results$prop_censored == prop_censored &
                                                                data_results$target_R2_C == target_R2_C &
                                                                data_results$algo != "w_gam"), ]
                             to_plot = data_results_one[(is.na(data_results_one$maxdepth) |
                                                           (data_results_one$maxdepth == maxdepth) ) &
                                                          ( (data_results_one$max_w_mod == max_w_mod) |
                                                              is.na(data_results_one$max_w_mod)) , ]
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
                             return(to_plot)
                           }))
  to_plot$model = factor(x = to_plot$model,
                         levels = c("RF", "wRF1_KM", "wRF1_Cox", "wRF1_RSF",
                                    "wRF2_KM", "wRF2_Cox",
                                    "wRF3_KM", "wRF3_Cox",
                                    "rsf_reg", "cox_reg"),
                         labels = c("RF", "swRF11", "swRF12", "swRF13",
                                    "swRF21", "swRF22",
                                    "swRF31", "swRF32",
                                    "RSFr", "Cr"))

  # ggplot(data = to_plot, aes(x = model, y = mean_R2,
  #                            group = interaction(target_R2_C, prop_censored),
  #                            colour = interaction(target_R2_C, prop_censored))) +
  #   geom_line(size = 1.5) +
  #   ylab("R2") +
  #   #labs(fill = "legend title") +
  #   #guide_legend(title = "Prop. censored, R2 C") +
  #   theme(legend.position = "bottom",
  #
  #         # legend.title = element_text(),
  #         axis.text.x = element_text(angle = 45, size = 10, vjust = 0.6),
  #         axis.title.x = element_blank()) +
  #   ggtitle("blabla") +
  #   scale_colour_discrete("Parameters (prop.censored, R2_C)",
  #                         labels  = c("0.1, 0.05", "0.1, 0.2",
  #                                     "0.3, 0.05", "0.3, 0.2",
  #                                     "0.5, 0.05", "0.5, 0.2"))
  to_plot$col_criteria = to_plot[,criteria]

  if (is.null(data_ranks)){
    plot = ggplot(data = to_plot, aes(x = model, y = col_criteria,
                                      group = interaction(target_R2_C, prop_censored))) +
      geom_line(size = 1) +
      geom_point(aes(shape = interaction(target_R2_C, prop_censored)), size = 6) +
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
      ggtitle(plot_title) +
      # scale_x_discrete(breaks = levels(to_plot$model),
      #                  labels = paste0(levels(to_plot$model), "\n(",
      #                                  sapply(X = levels(to_plot$model), FUN = function(x){paste0(to_plot[to_plot$model == x,"mean_rank"], collapse = ", ")}), ")")) +
      # scale_x_discrete(breaks = levels(to_plot$model),
      #                  labels = paste0(levels(to_plot$model), "\n(",
      #                                  sapply(X = levels(to_plot$model), FUN = function(x){paste0(to_plot[to_plot$model == x,"mean_rank"][c(1,3,5)], collapse = ", ")}),"\n",
      #                                  sapply(X = levels(to_plot$model), FUN = function(x){paste0(to_plot[to_plot$model == x,"mean_rank"][c(2,4,6)], collapse = ", ")}), ")")) +
      scale_shape_discrete("",
                           # labels  = c("0.3, 0.05", "0.3, 0.1",
                           #             "0.5, 0.05", "0.5, 0.1")
                           labels  = c("q = 0.1, R2_C = 0.05", "0.1, 0.1",
                                       "0.3, 0.05", "0.3, 0.1",
                                       "0.5, 0.05", "0.5, 0.1")
      )

  }

  if (!is.null(data_ranks)){
    data_ranks$mean_rank = round(x = data_ranks$mean_rank, digits = 1)
    to_plot = merge(x = to_plot, y = data_ranks, by = c("prop_censored", "target_R2_C", "model"))

    to_plot = to_plot[order(to_plot$prop_censored, to_plot$target_R2_C, to_plot$model),]

    plot = ggplot(data = to_plot, aes(x = model, y = col_criteria,
                                      group = interaction(target_R2_C, prop_censored))) +
      geom_line(size = 1) +
      geom_point(aes(shape = interaction(target_R2_C, prop_censored)), size = 4) +
      ylab(criteria) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, size = 6, vjust = 0.6),
            axis.title.x = element_blank()) +
      ggtitle(plot_title) +
      # scale_x_discrete(breaks = levels(to_plot$model),
      #                  labels = paste0(levels(to_plot$model), "\n(",
      #                                  sapply(X = levels(to_plot$model), FUN = function(x){paste0(to_plot[to_plot$model == x,"mean_rank"], collapse = ", ")}), ")")) +
      scale_x_discrete(breaks = levels(to_plot$model),
                       labels = paste0(levels(to_plot$model), "\n(",
                                       sapply(X = levels(to_plot$model), FUN = function(x){paste0(to_plot[to_plot$model == x,"mean_rank"][c(1,3,5)], collapse = ", ")}),"\n",
                                       sapply(X = levels(to_plot$model), FUN = function(x){paste0(to_plot[to_plot$model == x,"mean_rank"][c(2,4,6)], collapse = ", ")}), ")")) +
      scale_shape_discrete("Parameters (prop.censored, R2_C)",
                           # labels  = c("0.3, 0.05", "0.3, 0.1",
                           #             "0.5, 0.05", "0.5, 0.1")
                           labels  = c("0.1, 0.05", "0.1, 0.1",
                                       "0.3, 0.05", "0.3, 0.1",
                                       "0.5, 0.05", "0.5, 0.1")
      )
  }
  return(list(plot = plot, to_plot = to_plot))
}


make_correlation_simul = function(data_results, minleaf, maxdepth, max_w_mod){

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
                                  if (x == "RF_bench") x = "RF"
                                  return(x)
                                }),
                         as.character(to_plot$type_w_RF),
                         ifelse(is.na(as.character(to_plot$type_w)), "", paste0("_",as.character(to_plot$type_w)))
  )

  to_plot$model = factor(x = to_plot$model, levels = c("RF", "wRF1_KM", "wRF1_Cox", "wRF1_RSF",
                                                       "wRF2_KM", "wRF2_Cox",
                                                       "wRF3_KM", "wRF3_Cox",
                                                       "rsf_reg", "cox_reg"))

  d = melt(data = to_plot,
           id.vars = c("prop_censored", "target_R2_C", "iter", "model"),
           measure.vars = c("criteria_non_censored.mse",
                            "criteria_weighted.RSF_mse",
                            "criteria_weighted.Cox_mse",
                            "criteria_weighted.KM_mse",
                            "criteria_weighted.unif_mse"))

  d2 = dcast(data = d, formula =  model ~ prop_censored + target_R2_C + iter + variable, value.var = "value")

  result_cor = do.call(rbind, lapply(X = c("0.1_0.05_", "0.1_0.1_", "0.3_0.05_", "0.3_0.1_", "0.5_0.05_", "0.5_0.1_"),
                                     FUN = function(x){
                                       mcor = do.call(rbind, lapply(X = 1:100,
                                                                    FUN = function(y){
                                                                      cor(x = d2[,grep(pattern = paste0(x,y,"_"), x = colnames(d2))], method = "spearman")[,1]
                                                                    }))
                                       colnames(mcor) = gsub(pattern = paste0(x,y,"_"), replacement = "", x = colnames(mcor))
                                       return(mcor)
                                     }))

  result_cor = data.frame(result_cor)
  result_cor$prop_censored = rep(c(0.1, 0.3, 0.5), times = c(200, 200, 200))
  result_cor$target_R2_C = rep( c(rep(0.05, 100), rep(0.1, 100)), 3)

  result_cor_summary = result_cor %>% group_by(prop_censored, target_R2_C) %>% summarise(criteria_non_censored.mse = mean(criteria_non_censored.mse),
                                                                                         criteria_weighted.RSF_mse = mean(criteria_weighted.RSF_mse),
                                                                                         criteria_weighted.Cox_mse = mean(criteria_weighted.Cox_mse),
                                                                                         criteria_weighted.KM_mse = mean(criteria_weighted.KM_mse),
                                                                                         criteria_weighted.unif_mse = mean(criteria_weighted.unif_mse))
  return(data.frame(result_cor_summary))
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




## make results
maxdepth = 4
max_w_mod = 50

res_simul_weibull_log_1000_summary = read.csv("output_2017-10-24/res_simul_weibull_log_1000_summary.csv")
res_simul_weibull_identite_1000_summary = read.csv("output_2017-10-24/res_simul_weibull_identite_1000_summary.csv")
res_simul_mix_indep_log_1000_summary = read.csv("output_2017-10-24/res_simul_mix_indep_log_1000_summary.csv")
res_simul_mix_indep_identite_1000_summary = read.csv("output_2017-10-24/res_simul_mix_indep_identite_1000_summary.csv")
res_simul_mix_dep_log_1000_summary = read.csv("output_2017-10-24/res_simul_mix_dep_log_1000_summary.csv")
res_simul_mix_dep_identite_1000_summary = read.csv("output_2017-10-24/res_simul_mix_dep_identite_1000_summary.csv")


# res_simul_weibull_log_1000_summary$mean_rmse = sqrt(res_simul_weibull_log_1000_summary$mean_mse)
# res_simul_weibull_identite_1000_summary$mean_rmse = sqrt(res_simul_weibull_identite_1000_summary$mean_mse)
# res_simul_mix_indep_log_1000_summary$mean_rmse = sqrt(res_simul_mix_indep_log_1000_summary$mean_mse)
# res_simul_mix_indep_identite_1000_summary$mean_rmse = sqrt(res_simul_mix_indep_identite_1000_summary$mean_mse)
# res_simul_mix_dep_log_1000_summary$mean_rmse = sqrt(res_simul_mix_dep_log_1000_summary$mean_mse)
# res_simul_mix_dep_identite_1000_summary$mean_rmse = sqrt(res_simul_mix_dep_identite_1000_summary$mean_mse)


plot1 = make_plot_results_simul(data_results = res_simul_weibull_identite_1000_summary,
                                maxdepth = maxdepth,
                                max_w_mod = max_w_mod,
                                plot_title = "case 1 : weibull, phi(x) = x",
                                criteria = "mean_mse"
)
plot2 = make_plot_results_simul(data_results = res_simul_mix_indep_identite_1000_summary,
                                maxdepth = maxdepth,
                                max_w_mod = max_w_mod,
                                plot_title = "case 2 : indep. mix., phi(x) = x",
                                criteria = "mean_mse"
)
plot3 = make_plot_results_simul(data_results = res_simul_mix_dep_identite_1000_summary,
                                maxdepth = maxdepth,
                                max_w_mod = max_w_mod,
                                plot_title = "case 3 : dep. mix., phi(x) = x",
                                criteria = "mean_mse"
)
plot4 = make_plot_results_simul(data_results = res_simul_weibull_log_1000_summary,
                                maxdepth = maxdepth,
                                max_w_mod = max_w_mod,
                                plot_title = "case 1 : weibull, phi(x) = log(x+1)",
                                criteria = "mean_mse"
)
plot5 = make_plot_results_simul(data_results = res_simul_mix_indep_log_1000_summary,
                                maxdepth = maxdepth,
                                max_w_mod = max_w_mod,
                                plot_title = "case 2 : indep. mix., phi(x) = log(x+1)",
                                criteria = "mean_mse"
)
plot6 = make_plot_results_simul(data_results = res_simul_mix_dep_log_1000_summary,
                                maxdepth = maxdepth,
                                max_w_mod = max_w_mod,
                                plot_title = "case 3 : dep. mix., phi(x) = log(x+1)",
                                criteria = "mean_mse"
)


multiplot(plot1$plot + ylab("mse") + theme(legend.position = "None"),
          plot2$plot + ylab("mse") + theme(legend.position = "None"),
          plot3$plot + ylab("mse") + theme(legend.position = "None"),
          plot4$plot + ylab("mse") + theme(legend.position = "None"),
          plot5$plot + ylab("mse") + theme(legend.position = "None"),
          plot6$plot + ylab("mse") + theme(legend.position = "None"),
          cols = 2)


# -------------------------------------------------
#             Correlation
# -------------------------------------------------

# we should select some lines in order to compute correlations


res_simul_weibull_log_1000 = read.csv("output_2017-10-24/res_simul_weibull_log_1000.csv")
res_simul_weibull_identite_1000 = read.csv("output_2017-10-24/res_simul_weibull_identite_1000.csv")
res_simul_mix_indep_log_1000 = read.csv("output_2017-10-24/res_simul_mix_indep_log_1000.csv")
res_simul_mix_indep_identite_1000 = read.csv("output_2017-10-24/res_simul_mix_indep_identite_1000.csv")
res_simul_mix_dep_log_1000 = read.csv("output_2017-10-24/res_simul_mix_dep_log_1000.csv")
res_simul_mix_dep_identite_1000 = read.csv("output_2017-10-24/res_simul_mix_dep_identite_1000.csv")



res_simul_weibull_log_1000_correlation = make_correlation_simul(data_results = res_simul_weibull_log_1000,
                                                                maxdepth = 4,
                                                                minleaf = 50,
                                                                max_w_mod = 50)

res_simul_weibull_identite_1000_correlation = make_correlation_simul(data_results = res_simul_weibull_identite_1000,
                                                                     maxdepth = 4,
                                                                     minleaf = 50,
                                                                     max_w_mod = 50)

res_simul_mix_indep_log_1000_correlation = make_correlation_simul(data_results = res_simul_mix_indep_log_1000,
                                                                  maxdepth = 4,
                                                                  minleaf = 50,
                                                                  max_w_mod = 50)

res_simul_mix_indep_identite_1000_correlation = make_correlation_simul(data_results = res_simul_mix_indep_identite_1000,
                                                                       maxdepth = 4,
                                                                       minleaf = 50,
                                                                       max_w_mod = 50)

res_simul_mix_dep_log_1000_correlation = make_correlation_simul(data_results = res_simul_mix_dep_log_1000,
                                                                maxdepth = 4,
                                                                minleaf = 50,
                                                                max_w_mod = 50)

res_simul_mix_dep_identite_1000_correlation = make_correlation_simul(data_results = res_simul_mix_dep_identite_1000,
                                                                     maxdepth = 4,
                                                                     minleaf = 50,
                                                                     max_w_mod = 50)

res_simul_mean_correlation = (res_simul_weibull_log_1000_correlation +
                                res_simul_weibull_identite_1000_correlation +
                                res_simul_mix_indep_log_1000_correlation +
                                res_simul_mix_indep_identite_1000_correlation +
                                res_simul_mix_dep_log_1000_correlation +
                                res_simul_mix_dep_identite_1000_correlation) / 6

write.csv(x = res_simul_mean_correlation,
          file = "output_2017-10-24/res_simul_mean_correlation.csv",
          row.names = F)

xtable(x = res_simul_mean_correlation[,-3], digits = 2)


