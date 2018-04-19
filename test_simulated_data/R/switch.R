
res_simul_weibull_log_1000 = read.csv("output_2017-10-24/res_simul_weibull_log_1000.csv")
res_simul_weibull_identite_1000 = read.csv("output_2017-10-24/res_simul_weibull_identite_1000.csv")
res_simul_mix_indep_log_1000 = read.csv("output_2017-10-24/res_simul_mix_indep_log_1000.csv")
res_simul_mix_indep_identite_1000 = read.csv("output_2017-10-24/res_simul_mix_indep_identite_1000.csv")
res_simul_mix_dep_log_1000 = read.csv("output_2017-10-24/res_simul_mix_dep_log_1000.csv")
res_simul_mix_dep_identite_1000 = read.csv("output_2017-10-24/res_simul_mix_dep_identite_1000.csv")

colnames(res_simul_weibull_log_1000) = colnames(res_simul_weibull_log)
colnames(res_simul_weibull_identite_1000) = colnames(res_simul_weibull_log)
colnames(res_simul_mix_indep_log_1000) = colnames(res_simul_weibull_log)
colnames(res_simul_mix_indep_identite_1000) = colnames(res_simul_weibull_log)
colnames(res_simul_mix_dep_log_1000) = colnames(res_simul_weibull_log)
colnames(res_simul_mix_dep_identite_1000) = colnames(res_simul_weibull_log)

modif_algo = function(x){
  y = gsub(x = as.character(x), pattern = "RSF_regression", replacement = "rsf_reg")
  y = gsub(x = y, pattern = "Cox_regression", replacement = "cox_reg")
  return(factor(y))
}

res_simul_weibull_log_1000$algo = modif_algo(res_simul_weibull_log_1000$algo)
res_simul_weibull_identite_1000$algo = modif_algo(res_simul_weibull_identite_1000$algo)
res_simul_mix_indep_log_1000$algo = modif_algo(res_simul_mix_indep_log_1000$algo)
res_simul_mix_indep_identite_1000$algo = modif_algo(res_simul_mix_indep_identite_1000$algo)
res_simul_mix_dep_log_1000$algo = modif_algo(res_simul_mix_dep_log_1000$algo)
res_simul_mix_dep_identite_1000$algo = modif_algo(res_simul_mix_dep_identite_1000$algo)


write.csv(res_simul_weibull_log_1000, file = "output_2017-10-24/res_simul_weibull_log_1000.csv", row.names = F)
write.csv(res_simul_weibull_identite_1000, file = "output_2017-10-24/res_simul_weibull_identite_1000.csv", row.names = F)
write.csv(res_simul_mix_indep_log_1000, file = "output_2017-10-24/res_simul_mix_indep_log_1000.csv", row.names = F)
write.csv(res_simul_mix_indep_identite_1000, file = "output_2017-10-24/res_simul_mix_indep_identite_1000.csv", row.names = F)
write.csv(res_simul_mix_dep_log_1000, file = "output_2017-10-24/res_simul_mix_dep_log_1000.csv", row.names = F)
write.csv(res_simul_mix_dep_identite_1000, file = "output_2017-10-24/res_simul_mix_dep_identite_1000.csv", row.names = F)

########################################################
########################################################
########################################################


res_simul_weibull_log_1000_summary = read.csv("output_2017-10-24/res_simul_weibull_log_1000_summary.csv")
res_simul_weibull_identite_1000_summary = read.csv("output_2017-10-24/res_simul_weibull_identite_1000_summary.csv")
res_simul_mix_indep_log_1000_summary = read.csv("output_2017-10-24/res_simul_mix_indep_log_1000_summary.csv")
res_simul_mix_indep_identite_1000_summary = read.csv("output_2017-10-24/res_simul_mix_indep_identite_1000_summary.csv")
res_simul_mix_dep_log_1000_summary = read.csv("output_2017-10-24/res_simul_mix_dep_log_1000_summary.csv")
res_simul_mix_dep_identite_1000_summary = read.csv("output_2017-10-24/res_simul_mix_dep_identite_1000_summary.csv")

colnames(res_simul_weibull_log_1000_summary) = colnames(res_simul_weibull_log_summary)
colnames(res_simul_weibull_identite_1000_summary) = colnames(res_simul_weibull_log_summary)
colnames(res_simul_mix_indep_log_1000_summary) = colnames(res_simul_weibull_log_summary)
colnames(res_simul_mix_indep_identite_1000_summary) = colnames(res_simul_weibull_log_summary)
colnames(res_simul_mix_dep_log_1000_summary) = colnames(res_simul_weibull_log_summary)
colnames(res_simul_mix_dep_identite_1000_summary) = colnames(res_simul_weibull_log_summary)

res_simul_weibull_log_1000_summary$algo = modif_algo(res_simul_weibull_log_1000_summary$algo)
res_simul_weibull_identite_1000_summary$algo = modif_algo(res_simul_weibull_identite_1000_summary$algo)
res_simul_mix_indep_log_1000_summary$algo = modif_algo(res_simul_mix_indep_log_1000_summary$algo)
res_simul_mix_indep_identite_1000_summary$algo = modif_algo(res_simul_mix_indep_identite_1000_summary$algo)
res_simul_mix_dep_log_1000_summary$algo = modif_algo(res_simul_mix_dep_log_1000_summary$algo)
res_simul_mix_dep_identite_1000_summary$algo = modif_algo(res_simul_mix_dep_identite_1000_summary$algo)


write.csv(res_simul_weibull_log_1000_summary, file = "output_2017-10-24/res_simul_weibull_log_1000_summary.csv", row.names = F)
write.csv(res_simul_weibull_identite_1000_summary, file = "output_2017-10-24/res_simul_weibull_identite_1000_summary.csv", row.names = F)
write.csv(res_simul_mix_indep_log_1000_summary, file = "output_2017-10-24/res_simul_mix_indep_log_1000_summary.csv", row.names = F)
write.csv(res_simul_mix_indep_identite_1000_summary, file = "output_2017-10-24/res_simul_mix_indep_identite_1000_summary.csv", row.names = F)
write.csv(res_simul_mix_dep_log_1000_summary, file = "output_2017-10-24/res_simul_mix_dep_log_1000_summary.csv", row.names = F)
write.csv(res_simul_mix_dep_identite_1000_summary, file = "output_2017-10-24/res_simul_mix_dep_identite_1000_summary.csv", row.names = F)



