
setwd("~/Google Drive/GitHub/impact-churn-health-insurance/test-simulated-data-review/")


packages <- c(
  #"sword", # the methods of the article are implemented in the package sword
  "doParallel",
  "copula",
  "dplyr"
)

has <- packages %in% rownames(installed.packages())
if(any(!has)) install.packages(packages[!has])
for(package in packages){ library(package, character.only = TRUE)}

options(max.print = 10000)


# functions for the data simulation and comparison of the different algorithms
source("R/make_results_simulated_data_functions_review.R")


# ------------------------------------------------------------------------
#               Avec fonction log(x+1)
# ------------------------------------------------------------------------

#######  cas weibull
# je propose de faire cette sensi uniquement avec ce cas parmi les 6
# besoin beaucoup cut dans les parametres pour que ça ne soit pas trop long
# pour voir ce que donnent les autre setting de données simul, je peux éliminer minleaf 5 10 qui performent
# mal et qui sont les plus longs
t1 = Sys.time()
res_simul_weibull_log = make_result_simulated_data(v_n_simul = 2000, #
                                                   v_n_vars = 6,
                                                   v_prop_censored = c(0.1, 0.3, 0.5), #0.1, 0.3, 0.5
                                                   v_target_R2_C = c(0.1),# 0.05,
                                                   v_censoring_threshold = 1000,
                                                   v_mixture_T = F,
                                                   v_mixture_T_type = NULL,
                                                   v_mixture_T_n_groups = NULL,
                                                   lambdaT = 100,
                                                   shapeT = 1,
                                                   shapeC = 0.8,

                                                   v_sw_RF_mode1_type_w = c("KM", "Cox", "RSF"), #"KM", "Cox", theo", "RSF",
                                                   v_sw_RF_mode2_type_w = c("Cox", "theo"), #"KM", "theo",
                                                   v_sw_gam_type_w = c("Cox"), #"KM", "theo", "RSF",

                                                   v_sw_RF_mode1_minleaf = c(10, 20, 50, 100, 200, 300), #50
                                                   v_sw_RF_mode2_minleaf = c(10, 20, 50, 100, 200, 300),
                                                   v_rsf_reg_minleaf = c(10, 20, 50, 100, 200, 300),
                                                   v_RF_minleaf = c(10, 20, 50, 100, 200, 300),
                                                   v_rrt_reg_minleaf = c(10, 20, 50, 100, 200, 300),
                                                   v_rlt_reg_minleaf = c(10, 20, 50, 100),
                                                   v_rlt_reg_no_RL_minleaf = c(10, 20, 50, 100, 200, 300),

                                                   v_sw_RF_mode1_max_w_mod = c(50),#
                                                   v_sw_RF_mode2_max_w_mod = c(50),#, 50
                                                   v_sw_gam_max_w_mod = c(50),#, 50

                                                   v_RF_maxdepth = 10, #4
                                                   v_sw_RF_mode1_maxdepth = 10,
                                                   v_sw_RF_mode2_maxdepth = 10,
                                                   v_rsf_reg_maxdepth = 10,
                                                   v_rrt_reg_maxdepth = 10,
                                                   v_rlt_reg_maxdepth = 10,
                                                   v_rlt_reg_no_RL_maxdepth = 10,

                                                   types_w_ev = c("KM", "Cox", "RSF", "unif", "theo"),
                                                   max_w_ev = 1000,

                                                   ntree = 100,#
                                                   n_repet = 100,# 100
                                                   prop_train = 0.5,
                                                   phi = function(x){log(x+1)},
                                                   phi.args = list(),
                                                   ev_methods = c("concordance", "weighted"),
                                                   bandwidths = c(20,50),
                                                   seed = 0
)
Sys.time() - t1

write.csv2(res_simul_weibull_log,
          file = "output_2019-02-20_param/res_simul_weibull_log_1000_param_sensi_100_iter.csv",
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
            mean_theo_R2 = round(mean(criteria_weighted.theo_R2),5), sd_theo_R2 = round(sd(criteria_weighted.theo_R2),5),
            mean_mse = round(mean(criteria_non_censored.mse),5), sd_mse = round(sd(criteria_non_censored.mse),5),
            mean_KM_mse = round(mean(criteria_weighted.KM_mse),5), sd_KM_mse = round(sd(criteria_weighted.KM_mse),5),
            mean_Cox_mse = round(mean(criteria_weighted.Cox_mse),5), sd_Cox_mse = round(sd(criteria_weighted.Cox_mse),5),
            mean_RSF_mse = round(mean(criteria_weighted.RSF_mse),5), sd_RSF_mse = round(sd(criteria_weighted.RSF_mse),5),
            mean_theo_mse = round(mean(criteria_weighted.theo_mse),5), sd_theo_mse = round(sd(criteria_weighted.theo_mse),5)
            #mean_R2_20 = round(mean(criteria_group.R2_20),5), sd_R2_20 = round(sd(criteria_group.R2_20),5),
            #mean_Kendall_20 = round(mean(criteria_group.Kendall_20),5), sd_Kendall_20 = round(sd(criteria_group.Kendall_20),5)
            )


write.csv2(x = res_simul_weibull_log_summary,
          file = "output_2019-02-20_param/res_simul_weibull_log_1000_param_sensi_100_iter_summary.csv",
          row.names = F)

