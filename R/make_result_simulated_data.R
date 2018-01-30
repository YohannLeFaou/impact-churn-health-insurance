

setwd("~/Google Drive/GitHub/resultats_sword")

packages <- c(#"ggplot2",
  #"survival",
  #"randomForestSRC",
  #"mgcv",
  #"ranger",
  "doParallel",
  "copula",
  #"VineCopula",
  #"MASS",
  #"corpcor",
  "dplyr"
  #"caret",
  #"Matrix"
)

has <- packages %in% rownames(installed.packages())
if(any(!has)) install.packages(packages[!has])
for(package in packages){ library(package, character.only = TRUE)}

options(max.print = 10000)

print.myDF <- function(x, abbr = TRUE, minlength = 10, ...) {
  if (abbr) {
    names(x) <- abbreviate(names(x), minlength = minlength)
  }
  print.data.frame(x, ...)
}

#source("R/make_results_simulated_data_functions.R")



# ------------------------------------------------------------------------
#               Avec fonction log(x+1)
# ------------------------------------------------------------------------

#setwd("~/Google Drive/GitHub/resultats_sword/")

#######  cas weibull
t1 = Sys.time()
res_simul_weibull_log = make_result_simulated_data(v_n_simul = 2000,
                                                   v_n_vars = 6,
                                                   v_prop_censored = c(0.1, 0.3, 0.5), #
                                                   v_target_R2_C = c(0.05, 0.1),#
                                                   v_censoring_threshold = 1000,
                                                   v_mixture_T = F,
                                                   v_mixture_T_type = NULL,
                                                   v_mixture_T_n_groups = NULL,
                                                   lambdaT = 100,
                                                   shapeT = 1,
                                                   shapeC = 0.8,

                                                   v_w_RF_mode1_type_w = c("KM","Cox","RSF"), #
                                                   v_w_RF_mode2_type_w = c("KM","Cox"), #
                                                   v_w_gam_type_w = c("KM", "Cox", "RSF"), #

                                                   v_w_RF_mode1_minleaf = c(50),
                                                   v_w_RF_mode2_minleaf = c(50),
                                                   v_RSF_reg_minleaf = c(50),
                                                   v_RF_minleaf = c(50),

                                                   v_w_RF_mode1_max_w_mod = c(10, 50),#
                                                   v_w_RF_mode2_max_w_mod = c(10, 50),#, 50
                                                   v_w_gam_max_w_mod = c(10, 50),#, 50

                                                   v_RF_maxdepth = 4,
                                                   v_w_RF_mode1_maxdepth = 4,
                                                   v_w_RF_mode2_maxdepth = 4,
                                                   v_RSF_maxdepth = 4,

                                                   types_w_ev = c("KM","Cox","RSF", "unif"),
                                                   max_w_ev = 1000,

                                                   ntree = 100,
                                                   n_repet = 100,#
                                                   prop_train = 0.5,
                                                   phi = function(x){log(x+1)},
                                                   phi.args = list(),
                                                   ev_methods = c("concordance", "group", "weighted"),
                                                   bandwidths = c(20,50),
                                                   seed = 0
)
Sys.time() - t1

write.csv(res_simul_weibull_log,
          file = "output_2017-10-24/res_simul_weibull_log_1000.csv",
          row.names = F)

res_simul_weibull_log_summary = res_simul_weibull_log %>%
  group_by(n_simul, n_vars, prop_censored, target_R2_C, censoring_threshold, mixture_T, mixture_T_type, mixture_T_n_groups,
           type_pred, algo, type_w, minleaf, maxdepth, max_w_mod) %>%
  summarise(mean_R2 = round(mean(criteria_non_censored.R2),5), sd_R2 = round(sd(criteria_non_censored.R2),5),
            mean_Kendall = round(mean(criteria_non_censored.Kendall),5), sd_Kendall = round(sd(criteria_non_censored.Kendall),5),
            mean_concordance = round(mean(concordance.concordant),5), sd_concordance = round(sd(concordance.concordant),5),
            mean_KM_R2 = round(mean(criteria_weighted.KM_R2),5), sd_KM_R2 = round(sd(criteria_weighted.KM_R2),5),
            mean_Cox_R2 = round(mean(criteria_weighted.Cox_R2),5), sd_Cox_R2 = round(sd(criteria_weighted.Cox_R2),5),
            mean_RSF_R2 = round(mean(criteria_weighted.RSF_R2),5), sd_RSF_R2 = round(sd(criteria_weighted.RSF_R2),5),
            mean_mse = round(mean(criteria_non_censored.mse),5), sd_mse = round(sd(criteria_non_censored.mse),5),
            mean_KM_mse = round(mean(criteria_weighted.KM_mse),5), sd_KM_mse = round(sd(criteria_weighted.KM_mse),5),
            mean_Cox_mse = round(mean(criteria_weighted.Cox_mse),5), sd_Cox_mse = round(sd(criteria_weighted.Cox_mse),5),
            mean_RSF_mse = round(mean(criteria_weighted.RSF_mse),5), sd_RSF_mse = round(sd(criteria_weighted.RSF_mse),5),
            mean_R2_20 = round(mean(criteria_group.R2_20),5), sd_R2_20 = round(sd(criteria_group.R2_20),5),
            mean_Kendall_20 = round(mean(criteria_group.Kendall_20),5), sd_Kendall_20 = round(sd(criteria_group.Kendall_20),5))

# print.myDF(res_simul_weibull_log_summary[,c("prop_censored", "target_R2_C", "censoring_threshold", "type_pred", "algo",
#                                             "type_w", "minleaf", "max_w_mod", "mean_R2", "mean_Kendall")])


write.csv(x = res_simul_weibull_log_summary,
          file = "output_2017-10-24/res_simul_weibull_log_1000_summary.csv",
          row.names = F)


###### cas mixture dependant

t1 = Sys.time() # 45 min
res_simul_mix_dep_log = make_result_simulated_data(v_n_simul = 2000,
                                                   v_n_vars = 6,
                                                   v_prop_censored = c(0.1, 0.3, 0.5),#
                                                   v_target_R2_C = c(0.05, 0.1),#
                                                   v_censoring_threshold = 1000,
                                                   v_mixture_T = T,
                                                   v_mixture_T_type = "dependant",
                                                   v_mixture_T_n_groups = NULL,
                                                   lambdaT = 100,
                                                   shapeT = 1,
                                                   shapeC = 0.8,

                                                   v_w_RF_mode1_type_w = c("KM","Cox","RSF"),
                                                   v_w_RF_mode2_type_w = c("KM","Cox"),
                                                   v_w_gam_type_w = c("KM", "Cox", "RSF"),

                                                   v_w_RF_mode1_minleaf = c(50),
                                                   v_w_RF_mode2_minleaf = c(50),
                                                   v_RSF_reg_minleaf = c(50),
                                                   v_RF_minleaf = c(50),

                                                   v_w_RF_mode1_max_w_mod = c(10, 50),#
                                                   v_w_RF_mode2_max_w_mod = c(10, 50),#, 50
                                                   v_w_gam_max_w_mod = c(10, 50),#, 50

                                                   v_RF_maxdepth = 4,
                                                   v_w_RF_mode1_maxdepth = 4,
                                                   v_w_RF_mode2_maxdepth = 4,
                                                   v_RSF_maxdepth = 4,

                                                   types_w_ev = c("KM","Cox","RSF", "unif"),
                                                   max_w_ev = 1000,

                                                   ntree = 100,
                                                   n_repet = 100,#
                                                   prop_train = 0.5,
                                                   phi = function(x){log(x+1)},
                                                   phi.args = list(),
                                                   ev_methods = c("concordance", "group", "weighted"),
                                                   bandwidths = c(20,50),
                                                   seed = 0
)
Sys.time() - t1

write.csv(res_simul_mix_dep_log,
          file = "output_2017-10-24/res_simul_mix_dep_log_1000.csv",
          row.names = F)

#res_simul_mix_dep_log = read.csv(file = "output_2017-09-05/res_simul_mix_dep_log_1000.csv")

res_simul_mix_dep_log_summary = res_simul_mix_dep_log %>%
  group_by(n_simul, n_vars, prop_censored, target_R2_C, censoring_threshold, mixture_T, mixture_T_type, mixture_T_n_groups,
           type_pred, algo, type_w, minleaf, maxdepth, max_w_mod) %>%
  summarise(mean_R2 = round(mean(criteria_non_censored.R2),5), sd_R2 = round(sd(criteria_non_censored.R2),5),
            mean_Kendall = round(mean(criteria_non_censored.Kendall),5), sd_Kendall = round(sd(criteria_non_censored.Kendall),5),
            mean_concordance = round(mean(concordance.concordant),5), sd_concordance = round(sd(concordance.concordant),5),
            mean_KM_R2 = round(mean(criteria_weighted.KM_R2),5), sd_KM_R2 = round(sd(criteria_weighted.KM_R2),5),
            mean_Cox_R2 = round(mean(criteria_weighted.Cox_R2),5), sd_Cox_R2 = round(sd(criteria_weighted.Cox_R2),5),
            mean_RSF_R2 = round(mean(criteria_weighted.RSF_R2),5), sd_RSF_R2 = round(sd(criteria_weighted.RSF_R2),5),
            mean_mse = round(mean(criteria_non_censored.mse),5), sd_mse = round(sd(criteria_non_censored.mse),5),
            mean_KM_mse = round(mean(criteria_weighted.KM_mse),5), sd_KM_mse = round(sd(criteria_weighted.KM_mse),5),
            mean_Cox_mse = round(mean(criteria_weighted.Cox_mse),5), sd_Cox_mse = round(sd(criteria_weighted.Cox_mse),5),
            mean_RSF_mse = round(mean(criteria_weighted.RSF_mse),5), sd_RSF_mse = round(sd(criteria_weighted.RSF_mse),5),
            mean_R2_20 = round(mean(criteria_group.R2_20),5), sd_R2_20 = round(sd(criteria_group.R2_20),5),
            mean_Kendall_20 = round(mean(criteria_group.Kendall_20),5), sd_Kendall_20 = round(sd(criteria_group.Kendall_20),5))

# print.myDF(res_simul_mix_dep_log_summary[,c("prop_censored", "target_R2_C", "censoring_threshold", "type_pred", "algo", "type_w",
#                                             "minleaf", "max_w_mod", "mean_R2", "mean_Kendall")])

write.csv(x = res_simul_mix_dep_log_summary,
          file = "output_2017-10-24/res_simul_mix_dep_log_1000_summary.csv",
          row.names = F)


# print.myDF(res_simul_mix_dep_log_summary[c(5,13,21,35,83,92,99,113,122,130,138,152),
#                                      c("prop_censored", "target_R2_C", "censoring_threshold","algo", "type_w", "minleaf", "max_w_mod",
#                                        "mean_R2", "mean_Kendall", "mean_KM_R2", "mean_Cox_R2", "mean_RSF_R2")])


####### cas mixture independant

t1 = Sys.time()
res_simul_mix_indep_log = make_result_simulated_data(v_n_simul = 2000,
                                                     v_n_vars = 6,
                                                     v_prop_censored = c(0.1, 0.3, 0.5),#
                                                     v_target_R2_C = c(0.05, 0.1),#
                                                     v_censoring_threshold = 1000,
                                                     v_mixture_T = T,
                                                     v_mixture_T_type = "independant",
                                                     v_mixture_T_n_groups = 4,
                                                     lambdaT = 100,
                                                     shapeT = 1,
                                                     shapeC = 0.8,

                                                     v_w_RF_mode1_type_w = c("KM","Cox","RSF"),
                                                     v_w_RF_mode2_type_w = c("KM","Cox"),
                                                     v_w_gam_type_w = c("KM", "Cox", "RSF"),

                                                     v_w_RF_mode1_minleaf = c(50),
                                                     v_w_RF_mode2_minleaf = c(50),
                                                     v_RSF_reg_minleaf = c(50),
                                                     v_RF_minleaf = c(50),

                                                     v_w_RF_mode1_max_w_mod = c(10, 50),#
                                                     v_w_RF_mode2_max_w_mod = c(10, 50),#, 50
                                                     v_w_gam_max_w_mod = c(10, 50),#, 50

                                                     v_RF_maxdepth = 4,
                                                     v_w_RF_mode1_maxdepth = 4,
                                                     v_w_RF_mode2_maxdepth = 4,
                                                     v_RSF_maxdepth = 4,

                                                     types_w_ev = c("KM","Cox","RSF", "unif"),
                                                     max_w_ev = 1000,

                                                     ntree = 100,
                                                     n_repet = 100,#
                                                     prop_train = 0.5,
                                                     phi = function(x){log(x+1)},
                                                     phi.args = list(),
                                                     ev_methods = c("concordance", "group", "weighted"),
                                                     bandwidths = c(20,50),
                                                     seed = 0
)
Sys.time() - t1


res_simul_mix_indep_log_summary = res_simul_mix_indep_log %>%
  group_by(n_simul, n_vars, prop_censored, target_R2_C, censoring_threshold, mixture_T, mixture_T_type, mixture_T_n_groups,
           type_pred, algo, type_w, minleaf, maxdepth, max_w_mod) %>%
  summarise(mean_R2 = round(mean(criteria_non_censored.R2),5), sd_R2 = round(sd(criteria_non_censored.R2),5),
            mean_Kendall = round(mean(criteria_non_censored.Kendall),5), sd_Kendall = round(sd(criteria_non_censored.Kendall),5),
            mean_concordance = round(mean(concordance.concordant),5), sd_concordance = round(sd(concordance.concordant),5),
            mean_KM_R2 = round(mean(criteria_weighted.KM_R2),5), sd_KM_R2 = round(sd(criteria_weighted.KM_R2),5),
            mean_Cox_R2 = round(mean(criteria_weighted.Cox_R2),5), sd_Cox_R2 = round(sd(criteria_weighted.Cox_R2),5),
            mean_RSF_R2 = round(mean(criteria_weighted.RSF_R2),5), sd_RSF_R2 = round(sd(criteria_weighted.RSF_R2),5),
            mean_mse = round(mean(criteria_non_censored.mse),5), sd_mse = round(sd(criteria_non_censored.mse),5),
            mean_KM_mse = round(mean(criteria_weighted.KM_mse),5), sd_KM_mse = round(sd(criteria_weighted.KM_mse),5),
            mean_Cox_mse = round(mean(criteria_weighted.Cox_mse),5), sd_Cox_mse = round(sd(criteria_weighted.Cox_mse),5),
            mean_RSF_mse = round(mean(criteria_weighted.RSF_mse),5), sd_RSF_mse = round(sd(criteria_weighted.RSF_mse),5),
            mean_R2_20 = round(mean(criteria_group.R2_20),5), sd_R2_20 = round(sd(criteria_group.R2_20),5),
            mean_Kendall_20 = round(mean(criteria_group.Kendall_20),5), sd_Kendall_20 = round(sd(criteria_group.Kendall_20),5) )

# print.myDF(res_simul_mix_indep_log_summary[,c("prop_censored", "target_R2_C", "censoring_threshold","algo", "type_w", "minleaf", "max_w_mod", "mean_R2", "mean_Kendall")])
#
# print.myDF(res_simul_mix_indep_log_summary[c(2,7,15,23,29,31,33,36,39,80,87,95,103,107,109,111,115,117,119,124,134,140,146,148,150,154,156),
#                                    c("prop_censored", "target_R2_C", "censoring_threshold","algo", "type_w", "minleaf", "max_w_mod",
#                                          "mean_R2", "mean_Kendall", "mean_KM_R2", "mean_Cox_R2", "mean_RSF_R2")])


write.csv(res_simul_mix_indep_log,
          file = "output_2017-10-24/res_simul_mix_indep_log_1000.csv",
          row.names = F)

write.csv(res_simul_mix_indep_log_summary,
          file = "output_2017-10-24/res_simul_mix_indep_log_1000_summary.csv",
          row.names = F)

# ------------------------------------------------------------------------
#               Avec fonction identite
# ------------------------------------------------------------------------


#######  cas weibull
t1 = Sys.time() # 45 min
res_simul_weibull_identite = make_result_simulated_data(v_n_simul = 2000,
                                                        v_n_vars = 6,
                                                        v_prop_censored = c(0.1, 0.3, 0.5), #0.1
                                                        v_target_R2_C = c(0.05, 0.1),#
                                                        v_censoring_threshold = 1000,
                                                        v_mixture_T = F,
                                                        v_mixture_T_type = NULL,
                                                        v_mixture_T_n_groups = NULL,
                                                        lambdaT = 100,
                                                        shapeT = 1,
                                                        shapeC = 0.8,

                                                        v_w_RF_mode1_type_w = c("KM","Cox","RSF"),
                                                        v_w_RF_mode2_type_w = c("KM","Cox"),
                                                        v_w_gam_type_w = c("KM", "Cox", "RSF"),

                                                        v_w_RF_mode1_minleaf = c(50),
                                                        v_w_RF_mode2_minleaf = c(50),
                                                        v_RSF_reg_minleaf = c(50),
                                                        v_RF_minleaf = c(50),

                                                        v_w_RF_mode1_max_w_mod = c(10, 50),#
                                                        v_w_RF_mode2_max_w_mod = c(10, 50),#, 50
                                                        v_w_gam_max_w_mod = c(10, 50),#, 50

                                                        v_RF_maxdepth = 4,
                                                        v_w_RF_mode1_maxdepth = 4,
                                                        v_w_RF_mode2_maxdepth = 4,
                                                        v_RSF_maxdepth = 4,

                                                        types_w_ev = c("KM","Cox","RSF", "unif"),
                                                        max_w_ev = 1000,

                                                        ntree = 100,
                                                        n_repet = 100,#
                                                        prop_train = 0.5,
                                                        phi = function(x){x},
                                                        phi.args = list(),
                                                        ev_methods = c("concordance", "group", "weighted"),
                                                        bandwidths = c(20,50),
                                                        seed = 0
)
Sys.time() - t1

write.csv(res_simul_weibull_identite,
          file = "output_2017-10-24/res_simul_weibull_identite_1000.csv",
          row.names = F)

#res_simul_weibull_identite = read.csv(file = "output_2017-09-05/res_simul_weibull_identite_1000.csv")


res_simul_weibull_identite_summary = res_simul_weibull_identite %>%
  group_by(n_simul, n_vars, prop_censored, target_R2_C, censoring_threshold, mixture_T, mixture_T_type, mixture_T_n_groups,
           type_pred, algo, type_w, minleaf, maxdepth, max_w_mod) %>%
  summarise(mean_R2 = round(mean(criteria_non_censored.R2),5), sd_R2 = round(sd(criteria_non_censored.R2),5),
            mean_Kendall = round(mean(criteria_non_censored.Kendall),5), sd_Kendall = round(sd(criteria_non_censored.Kendall),5),
            mean_concordance = round(mean(concordance.concordant),5), sd_concordance = round(sd(concordance.concordant),5),
            mean_KM_R2 = round(mean(criteria_weighted.KM_R2),5), sd_KM_R2 = round(sd(criteria_weighted.KM_R2),5),
            mean_Cox_R2 = round(mean(criteria_weighted.Cox_R2),5), sd_Cox_R2 = round(sd(criteria_weighted.Cox_R2),5),
            mean_RSF_R2 = round(mean(criteria_weighted.RSF_R2),5), sd_RSF_R2 = round(sd(criteria_weighted.RSF_R2),5),
            mean_mse = round(mean(criteria_non_censored.mse),5), sd_mse = round(sd(criteria_non_censored.mse),5),
            mean_KM_mse = round(mean(criteria_weighted.KM_mse),5), sd_KM_mse = round(sd(criteria_weighted.KM_mse),5),
            mean_Cox_mse = round(mean(criteria_weighted.Cox_mse),5), sd_Cox_mse = round(sd(criteria_weighted.Cox_mse),5),
            mean_RSF_mse = round(mean(criteria_weighted.RSF_mse),5), sd_RSF_mse = round(sd(criteria_weighted.RSF_mse),5),
            mean_R2_20 = round(mean(criteria_group.R2_20),5), sd_R2_20 = round(sd(criteria_group.R2_20),5),
            mean_Kendall_20 = round(mean(criteria_group.Kendall_20),5), sd_Kendall_20 = round(sd(criteria_group.Kendall_20),5))

# print.myDF(res_simul_weibull_identite_summary[,c("prop_censored", "target_R2_C", "censoring_threshold", "type_pred", "algo",
#                                                  "type_w", "minleaf", "max_w_mod", "mean_R2", "mean_Kendall")])

write.csv(res_simul_weibull_identite_summary,
          file = "output_2017-10-24/res_simul_weibull_identite_1000_summary.csv",
          row.names = F)



# cas mixture dependant

t1 = Sys.time()
res_simul_mix_dep_identite = make_result_simulated_data(v_n_simul = 2000,
                                                        v_n_vars = 6,
                                                        v_prop_censored = c(0.1, 0.3, 0.5), #
                                                        v_target_R2_C = c(0.05, 0.1),#
                                                        v_censoring_threshold = 1000,
                                                        v_mixture_T = T,
                                                        v_mixture_T_type = "dependant",
                                                        v_mixture_T_n_groups = NULL,
                                                        lambdaT = 100,
                                                        shapeT = 1,
                                                        shapeC = 0.8,

                                                        v_w_RF_mode1_type_w = c("KM","Cox","RSF"),
                                                        v_w_RF_mode2_type_w = c("KM","Cox"),
                                                        v_w_gam_type_w = c("KM", "Cox", "RSF"),

                                                        v_w_RF_mode1_minleaf = c(50),
                                                        v_w_RF_mode2_minleaf = c(50),
                                                        v_RSF_reg_minleaf = c(50),
                                                        v_RF_minleaf = c(50),

                                                        v_w_RF_mode1_max_w_mod = c(10, 50),#
                                                        v_w_RF_mode2_max_w_mod = c(10, 50),#, 50
                                                        v_w_gam_max_w_mod = c(10, 50),#,50

                                                        v_RF_maxdepth = 4,
                                                        v_w_RF_mode1_maxdepth = 4,
                                                        v_w_RF_mode2_maxdepth = 4,
                                                        v_RSF_maxdepth = 4,

                                                        types_w_ev = c("KM","Cox","RSF", "unif"),
                                                        max_w_ev = 1000,

                                                        ntree = 100,
                                                        n_repet = 100,#
                                                        prop_train = 0.5,
                                                        phi = function(x){x},
                                                        phi.args = list(),
                                                        ev_methods = c("concordance", "group", "weighted"),
                                                        bandwidths = c(20,50),
                                                        seed = 0
)
Sys.time() - t1

write.csv(res_simul_mix_dep_identite,
          file = "output_2017-10-24/res_simul_mix_dep_identite_1000.csv",
          row.names = F)

#res_simul_mix_dep_identite = read.csv(file = "output_2017-09-05/res_simul_mix_dep_identite_1000.csv")


res_simul_mix_dep_identite_summary = res_simul_mix_dep_identite %>%
  group_by(n_simul, n_vars, prop_censored, target_R2_C, censoring_threshold, mixture_T, mixture_T_type, mixture_T_n_groups,
           type_pred, algo, type_w, minleaf, maxdepth, max_w_mod) %>%
  summarise(mean_R2 = round(mean(criteria_non_censored.R2),5), sd_R2 = round(sd(criteria_non_censored.R2),5),
            mean_Kendall = round(mean(criteria_non_censored.Kendall),5), sd_Kendall = round(sd(criteria_non_censored.Kendall),5),
            mean_concordance = round(mean(concordance.concordant),5), sd_concordance = round(sd(concordance.concordant),5),
            mean_KM_R2 = round(mean(criteria_weighted.KM_R2),5), sd_KM_R2 = round(sd(criteria_weighted.KM_R2),5),
            mean_Cox_R2 = round(mean(criteria_weighted.Cox_R2),5), sd_Cox_R2 = round(sd(criteria_weighted.Cox_R2),5),
            mean_RSF_R2 = round(mean(criteria_weighted.RSF_R2),5), sd_RSF_R2 = round(sd(criteria_weighted.RSF_R2),5),
            mean_mse = round(mean(criteria_non_censored.mse),5), sd_mse = round(sd(criteria_non_censored.mse),5),
            mean_KM_mse = round(mean(criteria_weighted.KM_mse),5), sd_KM_mse = round(sd(criteria_weighted.KM_mse),5),
            mean_Cox_mse = round(mean(criteria_weighted.Cox_mse),5), sd_Cox_mse = round(sd(criteria_weighted.Cox_mse),5),
            mean_RSF_mse = round(mean(criteria_weighted.RSF_mse),5), sd_RSF_mse = round(sd(criteria_weighted.RSF_mse),5),
            mean_R2_20 = round(mean(criteria_group.R2_20),5), sd_R2_20 = round(sd(criteria_group.R2_20),5),
            mean_Kendall_20 = round(mean(criteria_group.Kendall_20),5), sd_Kendall_20 = round(sd(criteria_group.Kendall_20),5))

# print.myDF(res_simul_mix_dep_identite_summary[,c("prop_censored", "target_R2_C", "censoring_threshold",
#                                                  "type_pred", "algo", "type_w",
#                                                  "minleaf", "max_w_mod",
#                                                  "mean_R2", "mean_Kendall")])

write.csv(x = res_simul_mix_dep_identite_summary,
          file = "output_2017-10-24/res_simul_mix_dep_identite_1000_summary.csv",
          row.names = F)

#
# print.myDF(res_simul_mix_dep_log_summary[c(5,13,21,35,83,92,99,113,122,130,138,152),
#                                      c("prop_censored", "target_R2_C", "censoring_threshold","algo", "type_w", "minleaf", "max_w_mod",
#                                        "mean_R2", "mean_Kendall", "mean_KM_R2", "mean_Cox_R2", "mean_RSF_R2")])


####### cas mixture independant

t1 = Sys.time()
res_simul_mix_indep_identite = make_result_simulated_data(v_n_simul = 2000,
                                                          v_n_vars = 6,
                                                          v_prop_censored = c(0.1, 0.3, 0.5), #
                                                          v_target_R2_C = c(0.05, 0.1),#
                                                          v_censoring_threshold = 1000,
                                                          v_mixture_T = T,
                                                          v_mixture_T_type = "independant",
                                                          v_mixture_T_n_groups = 4,
                                                          lambdaT = 100,
                                                          shapeT = 1,
                                                          shapeC = 0.8,

                                                          v_w_RF_mode1_type_w = c("KM","Cox","RSF"),
                                                          v_w_RF_mode2_type_w = c("KM","Cox"),
                                                          v_w_gam_type_w = c("KM", "Cox", "RSF"),

                                                          v_w_RF_mode1_minleaf = c(50),
                                                          v_w_RF_mode2_minleaf = c(50),
                                                          v_RSF_reg_minleaf = c(50),
                                                          v_RF_minleaf = c(50),

                                                          v_w_RF_mode1_max_w_mod = c(10, 50),#
                                                          v_w_RF_mode2_max_w_mod = c(10, 50),#, 50
                                                          v_w_gam_max_w_mod = c(10, 50),#, 50

                                                          v_RF_maxdepth = 4,
                                                          v_w_RF_mode1_maxdepth = 4,
                                                          v_w_RF_mode2_maxdepth = 4,
                                                          v_RSF_maxdepth = 4,

                                                          types_w_ev = c("KM","Cox","RSF", "unif"),
                                                          max_w_ev = 1000,

                                                          ntree = 100,
                                                          n_repet = 100,#
                                                          prop_train = 0.5,
                                                          phi = function(x){x},
                                                          phi.args = list(),
                                                          ev_methods = c("concordance", "group", "weighted"),
                                                          bandwidths = c(20,50),
                                                          seed = 0
)
Sys.time() - t1


res_simul_mix_indep_identite_summary = res_simul_mix_indep_identite %>%
  group_by(n_simul, n_vars, prop_censored, target_R2_C, censoring_threshold, mixture_T, mixture_T_type, mixture_T_n_groups,
           type_pred, algo, type_w, minleaf, maxdepth, max_w_mod) %>%
  summarise(mean_R2 = round(mean(criteria_non_censored.R2),5), sd_R2 = round(sd(criteria_non_censored.R2),5),
            mean_Kendall = round(mean(criteria_non_censored.Kendall),5), sd_Kendall = round(sd(criteria_non_censored.Kendall),5),
            mean_concordance = round(mean(concordance.concordant),5), sd_concordance = round(sd(concordance.concordant),5),
            mean_KM_R2 = round(mean(criteria_weighted.KM_R2),5), sd_KM_R2 = round(sd(criteria_weighted.KM_R2),5),
            mean_Cox_R2 = round(mean(criteria_weighted.Cox_R2),5), sd_Cox_R2 = round(sd(criteria_weighted.Cox_R2),5),
            mean_RSF_R2 = round(mean(criteria_weighted.RSF_R2),5), sd_RSF_R2 = round(sd(criteria_weighted.RSF_R2),5),
            mean_mse = round(mean(criteria_non_censored.mse),5), sd_mse = round(sd(criteria_non_censored.mse),5),
            mean_KM_mse = round(mean(criteria_weighted.KM_mse),5), sd_KM_mse = round(sd(criteria_weighted.KM_mse),5),
            mean_Cox_mse = round(mean(criteria_weighted.Cox_mse),5), sd_Cox_mse = round(sd(criteria_weighted.Cox_mse),5),
            mean_RSF_mse = round(mean(criteria_weighted.RSF_mse),5), sd_RSF_mse = round(sd(criteria_weighted.RSF_mse),5),
            mean_R2_20 = round(mean(criteria_group.R2_20),5), sd_R2_20 = round(sd(criteria_group.R2_20),5),
            mean_Kendall_20 = round(mean(criteria_group.Kendall_20),5), sd_Kendall_20 = round(sd(criteria_group.Kendall_20),5) )

# print.myDF(res_simul_mix_indep_identite_summary[,c("prop_censored", "target_R2_C", "censoring_threshold","algo", "type_w", "minleaf", "max_w_mod", "mean_R2", "mean_Kendall")])
#
# print.myDF(res_simul_mix_indep_log_summary[c(2,7,15,23,29,31,33,36,39,80,87,95,103,107,109,111,115,117,119,124,134,140,146,148,150,154,156),
#                                    c("prop_censored", "target_R2_C", "censoring_threshold","algo", "type_w", "minleaf", "max_w_mod",
#                                          "mean_R2", "mean_Kendall", "mean_KM_R2", "mean_Cox_R2", "mean_RSF_R2")])

write.csv(res_simul_mix_indep_identite,
          file = "output_2017-10-24/res_simul_mix_indep_identite_1000.csv",
          row.names = F)

write.csv(res_simul_mix_indep_identite_summary,
          file = "output_2017-10-24/res_simul_mix_indep_identite_1000_summary.csv",
          row.names = F)

