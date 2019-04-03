
packages <- c(#"ggplot2",
  "survival",
  "randomForestSRC",
  #"ranger",
  #"caret",
  #"MASS",
  #"reshape",
  "dplyr",
  "doParallel",
  "mgcv"
)

has   <- packages %in% rownames(installed.packages())
if(any(!has)) install.packages(packages[!has])
for(i in packages){ library(i,character.only = TRUE)}

setwd("~/Google Drive/GitHub/impact-churn-health-insurance/test-real-data-review")

################################################
##############  chargement des données

load("/Users/yohann/Google Drive/missions_forsides/Santiane 2016/data/resiliation_2016.RData")

source("R/make_results_Santiane_function_review.R")

######################## parametres generaux
#################################################################################

jours_par_mois_bissextile_deb_juillet = c(31,31,30,31,30,31,31,29,31,30,31,30)
jours_par_mois_normale_deb_juillet = c(31,31,30,31,30,31,31,28,31,30,31,30)


echeancier = c(0, cumsum( c(jours_par_mois_bissextile_deb_juillet,
                            jours_par_mois_normale_deb_juillet,
                            jours_par_mois_normale_deb_juillet,
                            jours_par_mois_normale_deb_juillet)) + 2)
## pour ajouter des annees, ajouter des cycles bi_norm_norm_norm


scenario_moyen = list(N_mois_precompte = 12,
                      N_mois_debut_lineaire = 13,
                      pourcentage_precompte = 0.5,
                      pourcentage_lineaire = 0.1,
                      revalo_annuelle = 0.08)

#################################################
########## Construction de la base
#################################################

base_etude = resiliation_2016
base_etude$y = base_etude$y_from_janv_2016
base_etude$delta = base_etude$delta_from_janv_2016

base_etude$regimeTitulaire_bis2 = as.character(base_etude$regimeTitulaire_bis)
base_etude$regimeTitulaire_bis2[base_etude$regimeTitulaire_bis2 == "Alsace Moselle" |
                                  base_etude$regimeTitulaire_bis2 == "Fonctionnaire"] = "Salarie"
base_etude$regimeTitulaire_bis2 = as.factor(as.character(base_etude$regimeTitulaire_bis2))

base_etude$compagnie2 = as.character(base_etude$compagnie)
base_etude$compagnie2[base_etude$compagnie2 %in% c("APICIL","Auxia","AXA","FMA","GFM SMO",
                                                   "MMC MUTUELLE","MUTUELLE BLEUE",
                                                   "SOLLY AZAR","THELEM","UGIP")] = "AUTRE"
base_etude$compagnie2[is.na(base_etude$compagnie2)] = "AUTRE"
base_etude$compagnie2 = as.factor(base_etude$compagnie2)

base_etude$duree_avt_effet_quali2 = as.character(base_etude$duree_avt_effet_quali)
base_etude$duree_avt_effet_quali2[base_etude$duree_avt_effet_quali2 %in% c("moins de -30","-30;-8","-7;-1")] = "moins de 0"
base_etude$duree_avt_effet_quali2 = as.factor(base_etude$duree_avt_effet_quali2)



var7 = c("NiveauGamme","civilite","zoneGeo_bis",
         "nbEnfants_quali","regimeTitulaire_bis2","ageAssure_quali"
) #"groupeCanalAcquisition_bis"


complete_cases = complete.cases(base_etude[,c("y","delta",var7)])
base_etude = base_etude[complete_cases,]



######################################## Results

# 1h30 de calcul pour chaque cas (6h en tout). Je peux enlever le r_max = 10 ça peut baisser le temps
#### fct_cash_flow

t1 = Sys.time()
res_santiane_fct_cash_flow = make_result_real_data(y_var = "y",
                                                   delta_var = "delta",
                                                   x_vars = var7,
                                                   data = base_etude[,c("y","delta",var7)],
                                                   n_train = 5000, #5000
                                                   n_test = 5000, #5000
                                                   max_time = 1465,
                                                   phi = calcul_facteurs_one,
                                                   phi.args = list(echeancier = echeancier,
                                                                   scenario = scenario_moyen,
                                                                   type = 2),
                                                   ev_methods = c("concordance", "weighted"),
                                                   types_w_ev = c("KM","Cox","RSF", "unif"),
                                                   max_w_ev = 1000,
                                                   bandwidths = c(20, 50),

                                                   v_sw_RF_mode1_type_w = c("KM","RSF"), #,"Cox"
                                                   v_sw_RF_mode2_type_w = c("Cox"), #"KM",
                                                   v_sw_gam_type_w = c("KM","Cox", "RSF"),

                                                   v_sw_RF_mode1_minleaf = c(50, 100, 200, 500),
                                                   v_sw_RF_mode2_minleaf = c(50, 100, 200, 500),
                                                   v_rsf_reg_minleaf = c(50, 100, 200, 500),
                                                   v_rrt_reg_minleaf = c(50, 100, 200, 500),
                                                   v_rlt_reg_minleaf = c(50, 100, 200, 500),

                                                   v_sw_RF_mode1_max_w_mod = c(50),#, 10, 50
                                                   v_sw_RF_mode2_max_w_mod = c(50),#, 10, 50
                                                   v_sw_gam_max_w_mod = c(50),#, 10, 50

                                                   v_sw_RF_mode1_maxdepth = 10,
                                                   v_sw_RF_mode2_maxdepth = 10,
                                                   v_rsf_reg_maxdepth = 10,
                                                   v_rrt_reg_maxdepth = 10,
                                                   v_rlt_reg_maxdepth = 10,

                                                   ntree = 100,
                                                   n_repet = 100,#50
                                                   seed = 0)
Sys.time() - t1

#res_santiane_fct_cash_flow = read.csv(file = "output_2017-10-24/res_santiane_fct_cash_flow_5000.csv")

res_santiane_fct_cash_flow_summary = res_santiane_fct_cash_flow %>%
  group_by(type_pred, algo, type_w, minleaf, maxdepth, max_w_mod) %>%
  summarise(mean_concordance = round(mean(concordance.concordant),5), sd_concordance = round(sd(concordance.concordant),5),
            #mean_R2_20 = round(mean(criteria_group.R2_20),5), sd_R2_20 = round(sd(criteria_group.R2_20),5),
            mean_KM_R2 = round(mean(criteria_weighted.KM_R2),5), sd_KM_R2 = round(sd(criteria_weighted.KM_R2),5),
            mean_Cox_R2 = round(mean(criteria_weighted.Cox_R2),5), sd_Cox_R2 = round(sd(criteria_weighted.Cox_R2),5),
            mean_RSF_R2 = round(mean(criteria_weighted.RSF_R2),5), sd_RSF_R2 = round(sd(criteria_weighted.RSF_R2),5),
            mean_unif_R2 = round(mean(criteria_weighted.unif_R2),5), sd_unif_R2 = round(sd(criteria_weighted.unif_R2),5),
            mean_KM_mse = round(mean(criteria_weighted.KM_mse),5), sd_KM_mse = round(sd(criteria_weighted.KM_mse),5),
            mean_Cox_mse = round(mean(criteria_weighted.Cox_mse),5), sd_Cox_mse = round(sd(criteria_weighted.Cox_mse),5),
            mean_RSF_mse = round(mean(criteria_weighted.RSF_mse),5), sd_RSF_mse = round(sd(criteria_weighted.RSF_mse),5),
            mean_unif_mse = round(mean(criteria_weighted.unif_mse),5), sd_unif_mse = round(sd(criteria_weighted.unif_mse),5))

#data.frame(res_santiane_fct_cash_flow_summary)

write.csv2(res_santiane_fct_cash_flow,
           file = "output_2019_03_27_param/res_santiane_fct_cash_flow_param_5000.csv",
           row.names = F)

write.csv2(res_santiane_fct_cash_flow_summary,
           file = "output_2019_03_27_param/res_santiane_fct_cash_flow_param_5000_summary.csv",
           row.names = F)



