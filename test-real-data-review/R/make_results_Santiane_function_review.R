

calcul_facteurs_v1 = function(survie_aux_echeances , scenario = scenario_moyen, type = 1){
  # attention le premier terme de "survie aux echeances" doit être > 0 (sinon ça renvoie n'importe quoi)

  probs_chute = - diff(survie_aux_echeances)
  facteur_reprise_sante = sum(probs_chute[1:scenario$N_mois_precompte] *
                                ((scenario$N_mois_precompte-1):0)/scenario$N_mois_precompte
  )
  facteur_precompte_sante = 1 - facteur_reprise_sante

  survie_lineaire = survie_aux_echeances[(scenario$N_mois_debut_lineaire + 1):length(survie_aux_echeances) ]
  facteur_lineaire_sante = sum( survie_lineaire *
                                  (1 + scenario$revalo_annuelle)^(1+floor(0:(length(survie_lineaire)-1)/12) )  / 12 )

  facteur_CA = scenario$pourcentage_precompte * facteur_precompte_sante +
    scenario$pourcentage_lineaire * facteur_lineaire_sante

  if (type == 2){
    return(facteur_CA)
  }
  return(c(facteur_CA,
           scenario$pourcentage_precompte * facteur_precompte_sante / facteur_CA,
           scenario$pourcentage_lineaire * facteur_lineaire_sante / facteur_CA)
  )
}

calcul_facteurs_v2 = function(prob_surv ,time_surv, echeance,
                              scenario = scenario_moyen, type = 1){

  survie_aux_echeances = approx(x = c(0,time_surv,echeance[length(echeance)]),
                                y = c(1,prob_surv,0),
                                xout = echeance,
                                method = "constant")$y

  probs_chute = - diff(survie_aux_echeances)
  facteur_reprise_sante = sum(probs_chute[1:scenario$N_mois_precompte] *
                                ((scenario$N_mois_precompte-1):0)/scenario$N_mois_precompte
  )
  facteur_precompte_sante = 1 - facteur_reprise_sante

  survie_lineaire = survie_aux_echeances[(scenario$N_mois_debut_lineaire + 1):length(survie_aux_echeances) ]
  facteur_lineaire_sante = sum( survie_lineaire *
                                  (1 + scenario$revalo_annuelle)^(1+floor(0:(length(survie_lineaire)-1)/12) )  / 12 )

  facteur_CA = scenario$pourcentage_precompte * facteur_precompte_sante +
    scenario$pourcentage_lineaire * facteur_lineaire_sante

  if (type == 2){
    return(facteur_CA)
  }
  return(c(facteur_CA,
           scenario$pourcentage_precompte * facteur_precompte_sante / facteur_CA,
           scenario$pourcentage_lineaire * facteur_lineaire_sante / facteur_CA)
  )
}

calcul_facteurs_one = function(x, echeancier, scenario, type = 1, ...){
  ## attention la fonction ne marche pas pour x = 0
  surv_curv_one = 1 * (echeancier < x)
  surv_curv_one[length(surv_curv_one)] = 0
  return(calcul_facteurs_v1(survie_aux_echeances = surv_curv_one,
                            scenario = scenario_moyen,
                            type = type))
}


eval_facteurCA_groupes = function(to_plot, echeance, scenario){
  phi_par_groupe = c()
  for (i in 1:length(levels(to_plot$classes))){
    a = survfit(formula = Surv(time = y, event = delta) ~ 1,
                data = to_plot[to_plot$classes == i,])
    phi_par_groupe[i] = calcul_facteurs_v2(prob_surv = a$surv[which(a$time < tail(echeance,1))],
                                           time_surv = a$time[which(a$time < tail(echeance,1))],
                                           echeance = echeance,
                                           scenario = scenario,
                                           type = 2)
  }
  return(phi_par_groupe)
}


make_surv_classes_from_index = function(quantiles_breaks,index_breaks,data,y_var, delta_var){
  breaks = quantile(index_breaks, probs = quantiles_breaks)
  breaks[1] = breaks[1]  -  0.01 * abs(breaks[1])
  breaks[length(breaks)] = breaks[length(breaks)] + abs(breaks[length(breaks)]) * 0.01
  classes = cut(index_breaks, right = F, include.lowest = F, breaks = breaks )
  classes2 = as.factor(classes)
  to_plot = data.frame( y = data[,y_var],
                        delta = data[,delta_var],
                        classes = factor(classes, ordered = T,
                                         levels = levels(classes) ,
                                         labels = as.character(1:length(levels(classes2)))  ))
}


caracterize_classes_one_var = function(data, classes, nom_variable, is_x_lab,is_legend ){
  data$classes = classes
  d = count_(data, vars = c(nom_variable, "classes"))
  colnames(d) = c(nom_variable, "classes","freq")
  d2 = count_(data, vars = "classes")
  colnames(d2) = c("classes","freq2")
  d = merge(d, d2, by = "classes")
  d$prop = d$freq / d$freq2

  ggplot(data = d, aes_string(x = nom_variable, y = "prop", fill = "classes")) +
    geom_bar(position = "dodge", stat = "identity") +
    ylab("Prop") +
    xlab("") +
    #xlab(ifelse(is_x_lab,"Modalite de la variable","")) +
    #guides(colour = guide_legend(override.aes = list(size = 10), title = "Classes : ") ) +
    theme(legend.position=ifelse(is_legend,"bottom","none"),
          text = element_text(size=12)) +
    ggtitle( paste0("Distribution de ",nom_variable," par classe") )
}

caracterize_classes_multiple_var = function(data, classes, noms_variables,nb_colonnes){
  list_plot = list()
  nb_lignes = floor(length(noms_variables)/nb_colonnes - 0.001) + 1
  for (i in 1:length(noms_variables) ){
    list_plot[[i]] = caracterize_classes_one_var(data = data,
                                                 classes = classes,
                                                 nom_variable = noms_variables[i],
                                                 is_x_lab = (i == nb_lignes),
                                                 is_legend = (i == nb_lignes))
  }
  multiplot(plotlist = list_plot, cols = nb_colonnes)
}


make_surv_plot2 = function(data, x_var, y_for_title, y_name = "y", delta_name = "delta",
                           line_width = 1.5, title_plus = "",xlim,ylim){
  formula = as.formula( paste0("Surv(time = ", y_name, ", event =", delta_name," ) ~ " ,x_var))
  fit= survfit(formula,
               data=data,
               type = "kaplan-meier")

  list_surv_curves = extract_surv_curv_to_plot(fit)
  to_plot = do.call(rbind, list_surv_curves)
  to_plot[,x_var] = factor(to_plot$classe,
                           levels = levels(as.factor(to_plot$classe)),
                           labels = levels(data[,x_var]))

  surv_plot = ggplot(data = to_plot , aes_string(x = "temps" , y = "surv",
                                                 group = x_var, colour = x_var)) +
    geom_line(size = line_width) +
    guides(colour = guide_legend(override.aes = list(size = 10), title = "Modalites : ") ) +
    theme(legend.position="bottom",text = element_text(size=20)) +
    xlab("Duree (jrs)") +
    ylab("Proba de survie") +
    ggtitle(paste0(y_for_title," : Variable ", x_var ," ", title_plus) ) +
    ylim(ylim) +
    xlim(xlim)
  return(surv_plot)
}


extract_surv_curv_to_plot = function(surv_obj){
  fin_class = cumsum(surv_obj$strata)
  deb_class = c(1 , fin_class[-length(fin_class)] + 1)
  list_surv_curv = list()
  for (i in 1:length(deb_class)){
    temps = c(0,surv_obj$time[deb_class[i]:fin_class[i]])
    surv = c(1 , surv_obj$surv[deb_class[i]:fin_class[i]])
    list_surv_curv[[i]] = data.frame(
      temps = temps,
      surv = surv,
      classe = i)
  }
  return(list_surv_curv)
}


make_result_real_data = function(y_var,
                                 delta_var,
                                 x_vars,
                                 data,
                                 n_train,
                                 n_test,
                                 max_time,
                                 phi,
                                 phi.args,
                                 ev_methods,
                                 types_w_ev,
                                 max_w_ev,
                                 bandwidths = NULL,

                                 v_sw_RF_mode1_type_w,
                                 v_sw_RF_mode2_type_w,
                                 v_sw_gam_type_w,

                                 v_sw_RF_mode1_minleaf,
                                 v_sw_RF_mode2_minleaf,
                                 v_rsf_reg_minleaf,
                                 v_rrt_reg_minleaf,
                                 v_rlt_reg_minleaf,

                                 v_sw_RF_mode1_max_w_mod,
                                 v_sw_RF_mode2_max_w_mod,
                                 v_sw_gam_max_w_mod,

                                 v_sw_RF_mode1_maxdepth,
                                 v_sw_RF_mode2_maxdepth,
                                 v_rsf_reg_maxdepth,
                                 v_rrt_reg_maxdepth,
                                 v_rlt_reg_maxdepth,

                                 ntree,
                                 n_repet,
                                 seed){

  data = data[,c(y_var, delta_var, x_vars)]

  cat("", file = "computation_progress.txt")

  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)

  res = foreach(i=1:n_repet,
                .combine = rbind,
                .packages = c("survival"
                              #,"sword"
                ),
                .export= c(
                  "calcul_facteurs_v1",
                  "scenario_moyen",
                  "echeancier",

                  "y_var",
                  "delta_var",
                  "x_vars",
                  "data",
                  "n_train",
                  "n_test",
                  "max_time",
                  "phi",
                  "phi.args",
                  "ev_methods",
                  "types_w_ev",
                  "max_w_ev",
                  "bandwidths",
                  "ntree",
                  "n_repet",
                  "seed",


                  "v_sw_RF_mode1_type_w",
                  "v_sw_RF_mode2_type_w",
                  "v_sw_gam_type_w",

                  "v_sw_RF_mode1_minleaf",
                  "v_sw_RF_mode2_minleaf",
                  "v_rsf_reg_minleaf",
                  "v_rrt_reg_minleaf",
                  "v_rlt_reg_minleaf",

                  "v_sw_RF_mode1_max_w_mod",
                  "v_sw_RF_mode2_max_w_mod",
                  "v_sw_gam_max_w_mod",

                  "v_sw_RF_mode1_maxdepth",
                  "v_sw_RF_mode2_maxdepth",
                  "v_rsf_reg_maxdepth",
                  "v_rrt_reg_maxdepth",
                  "v_rlt_reg_maxdepth"
                ),
                .verbose = F) %dopar% {

                  # load sword development version when developing
                  devtools::load_all("~/Google Drive/GitHub/sword")

                  set.seed(seed + i)

                  cat(paste0(i," in ", n_repet, "\n"),
                      file = "computation_progress.txt",
                      append = TRUE)

                  train_lines = sample(1:nrow(data), size = n_train)
                  test_lines = sample(setdiff(1:nrow(data), train_lines), size = n_test)

                  # Computation of mat_w through a call to cox_reg

                  # impossible to make a unique Call to cox_reg since we use a max_w_ev argument
                  # in the call in which we compute the results
                  res_mat_w = cox_reg(y_var = y_var,
                                      delta_var = delta_var,
                                      x_vars = x_vars,
                                      train = data[train_lines, ],
                                      test = data[test_lines, ],
                                      max_time = max_time,
                                      ev_methods = c("weighted"),
                                      types_w_ev = c("KM", "Cox", "RSF", "unif"),
                                      max_w_ev = 1000,
                                      cox_obj = F)

                  mat_w = rbind(res_mat_w$mat_w_train,
                                res_mat_w$mat_w_test)

                  # w_RF

                  ## mode 1
                  param_w_RF_mode1 = expand.grid(type_pred = NA,
                                                 algo = "w_RF1",
                                                 type_w = v_sw_RF_mode1_type_w,
                                                 minleaf = v_sw_RF_mode1_minleaf,
                                                 maxdepth = v_sw_RF_mode1_maxdepth,
                                                 max_w_mod = v_sw_RF_mode1_max_w_mod)


                  list_res_w_RF_mode1 = lapply(X = 1:nrow(param_w_RF_mode1),
                                               FUN = function(j){
                                                 unlist(sw_reg(y_var = y_var,
                                                               delta_var = delta_var,
                                                               x_vars = x_vars,
                                                               train = data[train_lines,],
                                                               test = data[test_lines,],
                                                               type_reg = "RF",
                                                               type_w = as.character(param_w_RF_mode1$type_w[j]),
                                                               max_time =  max_time,
                                                               phi = phi,
                                                               phi.args = phi.args,
                                                               ev_methods = ev_methods,
                                                               types_w_ev = types_w_ev,
                                                               max_w_mod = param_w_RF_mode1$max_w_mod[j],
                                                               max_w_ev = max_w_ev,
                                                               sw_reg_obj = F,
                                                               cens_mod_obj = F,
                                                               bandwidths = bandwidths,
                                                               mat_w = mat_w,
                                                               mode_sw_RF = 1,
                                                               minleaf = param_w_RF_mode1$minleaf[j],
                                                               maxdepth = param_w_RF_mode1$maxdepth[j],
                                                               sampsize = length(train_lines), # for the sample size being the same as the train dataset
                                                               # nsplit = 10,
                                                               ntree = ntree,
                                                               mtry = length(x_vars)
                                                 )$perf_test)
                                               }
                  )
                  res_w_RF_mode1 = do.call(rbind, lapply(list_res_w_RF_mode1,
                                                         function(x) x[match(names(list_res_w_RF_mode1[[1]]), names(x))]))

                  ## mode 2

                  param_w_RF_mode2 = expand.grid(type_pred = c("weights_tree", "KM_local"),
                                                 algo = "w_RF2",
                                                 type_w = v_sw_RF_mode2_type_w,
                                                 minleaf = v_sw_RF_mode2_minleaf,
                                                 maxdepth = v_sw_RF_mode2_maxdepth,
                                                 max_w_mod = v_sw_RF_mode2_max_w_mod)

                  param_w_RF_mode2_bis = unique(param_w_RF_mode2[,c("algo", "type_w", "minleaf", "maxdepth", "max_w_mod")])

                  list_res_w_RF_mode2 = lapply(X = 1:nrow(param_w_RF_mode2_bis),
                                               FUN = function(j){
                                                 one_res_w_RF_mode2 = sw_reg(y_var = y_var,
                                                                             delta_var = delta_var,
                                                                             x_vars = x_vars,
                                                                             train = data[train_lines,],
                                                                             test = data[test_lines,],
                                                                             type_reg = "RF",
                                                                             type_w = as.character(param_w_RF_mode2_bis$type_w[j]),
                                                                             max_time =  max_time,
                                                                             phi = phi,
                                                                             phi.args = phi.args,
                                                                             ev_methods = ev_methods,
                                                                             types_w_ev = types_w_ev,
                                                                             max_w_mod = param_w_RF_mode2_bis$max_w_mod[j],
                                                                             max_w_ev = max_w_ev,
                                                                             sw_reg_obj = F,
                                                                             cens_mod_obj = F,
                                                                             bandwidths = bandwidths,
                                                                             mat_w = mat_w,
                                                                             mode_sw_RF = 2,
                                                                             minleaf = floor(param_w_RF_mode2_bis$minleaf[j] * mean(data[train_lines, delta_var])),
                                                                             maxdepth = param_w_RF_mode2_bis$maxdepth[j],
                                                                             ntree = ntree,
                                                                             cp = 0,
                                                                             xval = 0)
                                                 return(rbind(unlist(one_res_w_RF_mode2$perf_test),
                                                              unlist(one_res_w_RF_mode2$perf_test_KMloc)))
                                               }
                  )
                  res_w_RF_mode2 = do.call(rbind, lapply(list_res_w_RF_mode2,
                                                         function(x) x[,match(colnames(list_res_w_RF_mode2[[1]]), colnames(x))]))

                  # # w_gam
                  # param_w_gam = expand.grid(type_pred = NA,
                  #                           algo = "w_gam",
                  #                           type_w = v_sw_gam_type_w,
                  #                           minleaf = NA,
                  #                           maxdepth = NA,
                  #                           max_w_mod = v_sw_gam_max_w_mod)
                  #
                  #
                  # list_res_w_gam = lapply(X = 1:nrow(param_w_gam),
                  #                         FUN = function(j){
                  #                           unlist(sw_reg(y_var = y_var,
                  #                                         delta_var = delta_var,
                  #                                         x_vars = x_vars,
                  #                                         train = data[train_lines,],
                  #                                         test = data[test_lines,],
                  #                                         type_reg = "gam",
                  #                                         type_w = as.character(param_w_gam$type_w[j]),
                  #                                         max_time =  max_time,
                  #                                         phi = phi,
                  #                                         phi.args = phi.args,
                  #                                         ev_methods = ev_methods,
                  #                                         types_w_ev = types_w_ev,
                  #                                         max_w_mod = param_w_gam$max_w_mod[j],
                  #                                         max_w_ev = max_w_ev,
                  #                                         sw_reg_obj = F,
                  #                                         cens_mod_obj = F,
                  #                                         bandwidths = bandwidths,
                  #                                         mat_w = mat_w
                  #                           )$perf_test)
                  #                         }
                  # )
                  # res_w_gam = do.call(rbind, lapply(list_res_w_gam,
                  #                                   function(x) x[match(names(list_res_w_gam[[1]]), names(x))]))

                  # rsf_reg
                  param_rsf_reg = expand.grid(type_pred = NA,
                                              algo = "rsf_reg",
                                              type_w = NA,
                                              minleaf = v_rsf_reg_minleaf,
                                              maxdepth = v_rsf_reg_maxdepth,
                                              max_w_mod = NA)

                  list_res_rsf_reg = lapply(X = 1:nrow(param_rsf_reg),
                                            FUN = function(j){
                                              unlist(rsf_reg(y_var = y_var,
                                                             delta_var = delta_var,
                                                             x_vars = x_vars,
                                                             train = data[train_lines,],
                                                             test = data[test_lines,],
                                                             max_time =  max_time,
                                                             phi = phi,
                                                             phi.args = phi.args,
                                                             ev_methods = ev_methods,
                                                             types_w_ev = types_w_ev,
                                                             max_w_ev = max_w_ev,
                                                             RSF.object = F,
                                                             bandwidths = bandwidths,
                                                             mat_w = mat_w,
                                                             minleaf = param_rsf_reg$minleaf[j],
                                                             maxdepth = param_rsf_reg$maxdepth[j],
                                                             # nsplit = 10,
                                                             ntree = ntree,
                                                             mtry = length(x_vars)
                                              )$perf_test)
                                            }
                  )
                  res_rsf_reg = do.call(rbind, lapply(list_res_rsf_reg,
                                                      function(x) x[match(names(list_res_rsf_reg[[1]]), names(x))]))

                  # cox_reg
                  param_cox_reg = expand.grid(type_pred = NA,
                                              algo = "cox_reg",
                                              type_w = NA,
                                              minleaf = NA,
                                              maxdepth = NA,
                                              max_w_mod = NA)

                  list_res_cox_reg = lapply(X = 1:nrow(param_cox_reg),
                                            FUN = function(j){
                                              unlist(cox_reg(y_var = y_var,
                                                             delta_var = delta_var,
                                                             x_vars = x_vars,
                                                             train = data[train_lines,],
                                                             test = data[test_lines,],
                                                             max_time = max_time,
                                                             ev_methods = ev_methods,
                                                             types_w_ev = types_w_ev,
                                                             max_w_ev = max_w_ev,
                                                             phi = phi,
                                                             phi.args = phi.args,
                                                             cox_obj = F,
                                                             bandwidths = bandwidths,
                                                             mat_w = mat_w
                                              )$perf_test)
                                            }
                  )
                  res_cox_reg = do.call(rbind, lapply(list_res_cox_reg,
                                                      function(x) x[match(names(list_res_cox_reg[[1]]), names(x))]))


                  # rrt_reg
                  param_rrt_reg = expand.grid(type_pred = NA,
                                              algo = "rrt_reg",
                                              type_w = NA,
                                              minleaf = v_rrt_reg_minleaf,
                                              maxdepth = v_rrt_reg_maxdepth,
                                              max_w_mod = NA)

                  list_res_rrt_reg = lapply(X = 1:nrow(param_rrt_reg),
                                            FUN = function(j){
                                              unlist(
                                                rrt_reg(y_var = y_var,
                                                        delta_var = delta_var,
                                                        x_vars = x_vars,
                                                        train = data[train_lines,],
                                                        test = data[test_lines,],
                                                        max_time = max_time,
                                                        phi = phi,
                                                        phi.args = phi.args,
                                                        ev_methods = ev_methods,
                                                        types_w_ev = types_w_ev,
                                                        max_w_ev = max_w_ev,
                                                        mat_w = mat_w,
                                                        rrt_obj = F,
                                                        bandwidths = bandwidths,
                                                        minleaf = floor(param_rrt_reg$minleaf[j] * mean(data[train_lines,delta_var])),
                                                        maxdepth = param_rrt_reg$maxdepth[j],
                                                        ntree = ntree,
                                                        cp = 0,
                                                        xval = 0)$perf_test_KMloc)
                                            })
                  res_rrt_reg = do.call(rbind, lapply(list_res_rrt_reg,
                                                      function(x) x[match(names(list_res_rrt_reg[[1]]), names(x))]))



                  # rlt_reg

                  ## with RL
                  param_rlt_reg = expand.grid(type_pred = NA,
                                              algo = "rlt_reg",
                                              type_w = NA,
                                              minleaf = v_rlt_reg_minleaf,
                                              maxdepth = v_rlt_reg_maxdepth,
                                              max_w_mod = NA)

                  list_res_rlt_reg = lapply(X = 1:nrow(param_rlt_reg),
                                            FUN = function(j){
                                              unlist(
                                                rlt_reg(y_var = y_var,
                                                        delta_var = delta_var,
                                                        x_vars = x_vars,
                                                        train = data[train_lines,],
                                                        test = data[test_lines,],
                                                        max_time = max_time,
                                                        phi = phi,
                                                        phi.args = phi.args,
                                                        ev_methods = ev_methods,
                                                        types_w_ev = types_w_ev,
                                                        max_w_ev = max_w_ev,
                                                        rlt_obj = F,
                                                        bandwidths = bandwidths,
                                                        mat_w = mat_w,
                                                        minleaf = param_rlt_reg$minleaf[j],
                                                        maxdepth = param_rlt_reg$maxdepth[j],
                                                        ntree = ntree,
                                                        mtry = length(x_vars),
                                                        embed.ntrees = 10,
                                                        importance = F,
                                                        resample.prob = 1,
                                                        reinforcement = T
                                                )$perf_test)
                                            })
                  res_rlt_reg = do.call(rbind, lapply(list_res_rlt_reg,
                                                      function(x) x[match(names(list_res_rlt_reg[[1]]), names(x))]))


                  ## without RL
                  param_rlt_reg_no_RL = expand.grid(type_pred = NA,
                                                    algo = "rlt_reg_no_RL",
                                                    type_w = NA,
                                                    minleaf = v_rlt_reg_minleaf, # same cases studied as for rlt_reg
                                                    maxdepth = v_rlt_reg_maxdepth,
                                                    max_w_mod = NA)

                  list_res_rlt_reg_no_RL = lapply(X = 1:nrow(param_rlt_reg_no_RL),
                                                  FUN = function(j){
                                                    unlist(
                                                      rlt_reg(y_var = y_var,
                                                              delta_var = delta_var,
                                                              x_vars = x_vars,
                                                              train = data[train_lines,],
                                                              test = data[test_lines,],
                                                              max_time = max_time,
                                                              phi = phi,
                                                              phi.args = phi.args,
                                                              ev_methods = ev_methods,
                                                              types_w_ev = types_w_ev,
                                                              max_w_ev = max_w_ev,
                                                              rlt_obj = F,
                                                              bandwidths = bandwidths,
                                                              mat_w = mat_w,
                                                              minleaf = param_rlt_reg_no_RL$minleaf[j],
                                                              maxdepth = param_rlt_reg_no_RL$maxdepth[j],
                                                              ntree = ntree,
                                                              mtry = length(x_vars),
                                                              importance = F,
                                                              resample.prob = 1,
                                                              reinforcement = F
                                                      )$perf_test)
                                                  })
                  res_rlt_reg_no_RL = do.call(rbind, lapply(list_res_rlt_reg_no_RL,
                                                            function(x) x[match(names(list_res_rlt_reg_no_RL[[1]]), names(x))]))


                  list_res = list(
                    res_w_RF_mode1, res_w_RF_mode2,
                    #res_w_gam,
                    res_rsf_reg, res_cox_reg, res_rrt_reg,
                    res_rlt_reg, res_rlt_reg_no_RL
                  )


                  return(cbind(censoring_rate_with_threshold = res_mat_w$cens_rate,
                               rbind(param_w_RF_mode1, param_w_RF_mode2,
                                     #param_w_gam,
                                     param_rsf_reg, param_cox_reg, param_rrt_reg, param_rlt_reg, param_rlt_reg_no_RL),
                               iter = i,
                               do.call(rbind, lapply(list_res,
                                                     function(x) x[, match(colnames(list_res[[1]]), colnames(x))]))
                  ))
                }
  stopCluster(cl)
  return(res)
}


