

calc_R2_weibull = function(multi, beta ,X, lambda, shape, n_simul){
  # this function evaluates the R2 of a weibull model given covariates X, and parameters multi, beta,
  ## lambda and shape
  # the result is a close approximation of the reality since it relies on simulated data
  coefs = (as.matrix(X) %*% beta) * multi
  y = exp(- coefs) * rweibull(n = n_simul,
                              scale = lambda,
                              shape = shape )
  1 - 1 / length(y) * sum( (y - lambda * exp(-coefs) *gamma(1 + 1/shape) )^2 ) /
    sd(y)^2
}

objective_function1 = function(multi, beta,X, lambda, shape, n_simul , target){
  # Objective function involved in the calibration of the parameter multi so that a given R2 (target_R2) is
  ## achieved by a given simulated sample
  return(abs(calc_R2_weibull(multi = multi,
                             beta = beta,
                             X = X,
                             lambda = lambda,
                             shape = shape,
                             n_simul = n_simul)
             - target))
}

calc_prop_censored = function(lambda, coefs, censored_var, shape, censoring_threshold){
  # this function evaluates the proportion of non censored observations given observations of the censored
  ## variable (min(censored_var,c_phi)) and a given distribution for C (parameters lambda, coefs, shape)
  sum( pmin(censored_var, censoring_threshold) > exp(-coefs) * rweibull(length(censored_var),
                                                                        scale = lambda,
                                                                        shape = shape) ) /
    length(censored_var)
}

objective_function2 = function(lambda, coefs, censored_var, shape, target, censoring_threshold){
  # Objective function involved in the calibration of the parameter lambda (scale parameter for C)
  ## so that a given proportion of non censored observations is achieved by a given simulated sample
  return(abs(calc_prop_censored(lambda = lambda,
                                coefs = coefs,
                                censored_var = censored_var,
                                shape = shape,
                                censoring_threshold = censoring_threshold)
             - target))
}


generate_censored_data = function(n_simul,
                                  n_vars,
                                  prop_censored,
                                  lambdaT,
                                  shapeT,
                                  mixture_T,
                                  mixture_T_type = NULL,
                                  mixture_T_n_groups = NULL,
                                  shapeC,
                                  target_R2_C = NULL,
                                  censoring_threshold = Inf){
  # function which generates the data
  # list of arguments :
  ## n_simul : number of simulated observations
  ## lambdaT : scale parameter of the weibull T distribution
  ## shapeT : shape parameter of the weibull T distribution
  ## shapeC : shape parameter of the weibull C distribution
  ## n_vars : number of covariates
  ## target_R2_T : percentage of explained variance in the model for T given X
  ## target_R2_C : percentage of explained variance in the model for C given X
  ## prop_censored : proportion of non censored observations in the simulated data
  ## with_jump (bool) : is the data generated with a jump at lambdaT/2 or not ?
  ## jump_importance : relative importance of the jump part in the influence of X

  # Note : data is generated so that it fits to some target objectives :
  ## - target_R2_T
  ## - target_R2_C
  ## - prop_censored
  # For these to holds, each target matches an optimised parameter. Also we use while loops to
  ## verifie that our data fits the requirements after the optimisation, since we observes
  ## sometimes the optimisation not giving a good value for the parameter

  # Remark : if mixture_T_type = "dependant", n_vars
  if (!mixture_T & !is.null(mixture_T_type)){stop("If mixture_T = FALSE, mixture_T_type has to be set to NULL")}
  if (!mixture_T & !is.null(mixture_T_n_groups)){stop("If mixture_T = FALSE, mixture_T_n_groups has to be set to NULL")}

  if (!is.null(mixture_T_type)){
    mixture_T_type = match.arg(as.character(mixture_T_type), c("dependant", "independant"))
    if (n_vars < 2){stop("If mixture_T_type = dependant, n_vars has to be >= 2")}
  }

  # generate X
  copula1 = copula::normalCopula(param = runif(n = 1,min = 0, max = 0.6),
                                 dim = n_vars,
                                 dispstr = "ar1")
  X = data.frame(copula::rCopula( n = n_simul, copula = copula1))
  colnames(X) = paste0("v", 1:n_vars)
  X = apply(X = X, MARGIN = 2, FUN = function(x){2 * x - 1})


  # generate T
  if (!mixture_T){
    # case 1: no jump in the T distribution
    betaT = rnorm(n_vars, mean = 0, sd = 0.2)
    coefT = as.matrix(X) %*% betaT
    Te = exp(-coefT) * rweibull(n = n_simul, scale = lambdaT, shape = shapeT)
    R2_T = 1 - 1 / length(Te) * sum((Te - lambdaT * exp(-coefT) *gamma(1 + 1/shapeT) )^2) /
      sd(Te)^2 # R2 calculé sans prise en compte du seuil
  }
  if (mixture_T){
    if (mixture_T_type == "independant"){
      # build the groups
      probs_group = rep(1,mixture_T_n_groups)/mixture_T_n_groups
      groups = sample(1:mixture_T_n_groups, prob = probs_group, replace = T, size = n_simul)

      # simulate T
      betasT = matrix(rnorm(n_vars * mixture_T_n_groups, mean = 0, sd = 0.3), nrow = n_vars)
      coefT = sapply(X = 1:n_simul, FUN = function(i){sum(as.matrix(X[i,]) * betasT[,groups[i]])})
      Te = exp(-coefT) * rweibull(n = n_simul, scale = lambdaT, shape = shapeT)

      # le R2 qui suit est vrai je pense, mais il a tendance à être proche de 0 (réduire le nb de groupes pour qu'il ne soit pas trop faible)
      all_coefT = as.matrix(X) %*% betasT
      partial_expected_mean_T = apply(X = all_coefT, MARGIN = 1, FUN = function(x){sum(exp(-x) * probs_group)})
      R2_T = 1 - 1 / length(Te) * sum((Te - lambdaT * partial_expected_mean_T * gamma(1 + 1/shapeT))^2) /
        sd(Te)^2
    }
    if (mixture_T_type == "dependant"){
      # build the groups
      groups = ifelse(X[,1] >= 0 & X[,2] >= 0, 1,
                      ifelse(X[,1] >= 0 & X[,2] < 0, 2,
                             ifelse(X[,1] < 0 & X[,2] >= 0, 3,
                                    4)))

      # simulate T
      betasT = matrix(rnorm(n_vars * 4, mean = 0, sd = 0.2), nrow = n_vars)
      coefT = sapply(X = 1:n_simul, FUN = function(i){sum(as.matrix(X[i,]) * betasT[,groups[i]])})
      Te = exp(-coefT) * rweibull(n = n_simul, scale = lambdaT, shape = shapeT)

      # le R2 qui suit est vrai je pense, mais il a tendance à être proche de 0 (réduire le nb de groupes pour qu'il ne soit pas trop faible)
      R2_T = 1 - 1 / length(Te) * sum((Te - lambdaT * exp(-coefT) * gamma(1 + 1/shapeT))^2) /
        sd(Te)^2
    }
  }

  # generate C
  if (!is.null(target_R2_C)){
    R2_C = -1
    prop_censored_empirical = -1
    while ((abs(prop_censored_empirical - prop_censored) > 0.01) |
           ( abs(R2_C - target_R2_C) > 0.01) ){
      betaC0 = rnorm(n_vars, mean = 0, sd = 0.2)
      opt1 = optimize(f =  objective_function1,
                      lower = 0,
                      upper = 10,
                      beta = betaC0,
                      X = X,
                      n_simul = n_simul,
                      lambda = 100,
                      shape = shapeC,
                      target = target_R2_C)

      betaC = opt1$minimum * betaC0
      coefC = as.matrix(X) %*% betaC
      res_optimize = optimize(f =  objective_function2,
                              lower = 0,
                              upper = 10000,
                              coefs = coefC,
                              censored_var = Te,
                              shape = shapeC,
                              target = prop_censored,
                              censoring_threshold = censoring_threshold)

      lambdaC = res_optimize$minimum
      C = exp(-coefC) * rweibull(n = n_simul,
                                 scale =  lambdaC,
                                 shape = shapeC)

      prop_censored_empirical = sum(pmin(Te,censoring_threshold) > C) / length(Te)
      R2_C = 1 - 1 / length(C) * sum( (C - lambdaC * exp(-coefC) * gamma(1+1/shapeC) )^2 ) /
        sd(C)^2
    }
  }
  if(is.null(target_R2_C)){
    prop_censored_empirical = -1
    while (abs(prop_censored_empirical - prop_censored) > 0.01 ){
      betaC = rnorm(n_vars, mean = 0, sd = 0.2)
      coefC = as.matrix(X) %*% betaC
      res_optimize = optimize(f =  objective_function2,
                              lower = 0,
                              upper = 10000,
                              coefs = coefC,
                              censored_var = Te,
                              shape = shapeC,
                              target = prop_censored,
                              censoring_threshold = censoring_threshold)

      lambdaC = res_optimize$minimum
      C = exp(-coefC) * rweibull(n = n_simul,
                                 scale =  lambdaC,
                                 shape = shapeC)

      prop_censored_empirical = sum(pmin(Te,censoring_threshold) > C) / length(Te)
    }
    R2_C = 1 - 1 / length(C) * sum( (C - lambdaC * exp(-coefC) * gamma(1+1/shapeC))^2 ) /
      sd(C)^2
  }

  ## construction the dataframe
  data_sim = cbind(X,Te,C)
  data_sim = as.data.frame(data_sim)
  colnames(data_sim) = c(paste0("v",1:n_vars), "Te", "C")

  data_sim$y = pmin(data_sim$Te, data_sim$C)
  data_sim$delta = (data_sim$Te <= data_sim$C) * 1

  out = list(data_sim = data_sim,
             R2_T = R2_T,
             R2_C = R2_C,
             prop_censored_empirical = prop_censored_empirical,
             coefT = coefT,
             lambdaC = lambdaC,
             coefC = coefC)

  return(out)
}


make_result_simulated_data = function(v_n_simul,
                                      v_n_vars,
                                      v_prop_censored,
                                      v_target_R2_C,
                                      v_censoring_threshold,
                                      v_mixture_T,
                                      v_mixture_T_type,
                                      v_mixture_T_n_groups,
                                      lambdaT,
                                      shapeT,
                                      shapeC,

                                      v_w_RF_mode1_type_w,
                                      v_w_RF_mode2_type_w,
                                      v_w_gam_type_w,

                                      v_RF_minleaf,
                                      v_w_RF_mode1_minleaf,
                                      v_w_RF_mode2_minleaf,
                                      v_RSF_reg_minleaf,

                                      v_w_RF_mode1_max_w_mod,
                                      v_w_RF_mode2_max_w_mod,
                                      v_w_gam_max_w_mod,

                                      v_RF_maxdepth,
                                      v_w_RF_mode1_maxdepth,
                                      v_w_RF_mode2_maxdepth,
                                      v_RSF_maxdepth,

                                      types_w_ev,
                                      max_w_ev,

                                      ntree,
                                      n_repet,
                                      prop_train,
                                      phi = function(x){x},
                                      phi.args = list(),
                                      ev_methods,
                                      bandwidths,
                                      seed){

  ev_methods <- match.arg(as.character(ev_methods), c("concordance","single", "group", "weighted"), several.ok = T)

  grid_param = expand.grid(n_simul = v_n_simul,
                           n_vars = v_n_vars,
                           prop_censored = v_prop_censored,
                           target_R2_C = v_target_R2_C,
                           censoring_threshold = v_censoring_threshold,
                           mixture_T = v_mixture_T,
                           mixture_T_type = ifelse(!is.null(v_mixture_T_type), v_mixture_T_type, NA) ,
                           mixture_T_n_groups = ifelse(!is.null(v_mixture_T_n_groups), v_mixture_T_n_groups, NA))

  tab_results =
    do.call(rbind,
            lapply(X = 1:nrow(grid_param),
                   FUN = function(i){

                     cat(paste0(i," in ", nrow(grid_param), "\n"))

                     n_cores <- detectCores() - 1
                     cl <- makeCluster(n_cores)
                     registerDoParallel(cl)

                     res = foreach(k=1:n_repet,
                                   .combine = rbind,
                                   .packages = c("sword",
                                                 "copula"
                                   ),
                                   .export= c(
                                     # function for simulated data

                                     "calc_R2_weibull",
                                     "objective_function1",
                                     "calc_prop_censored",
                                     "objective_function2",
                                     "generate_censored_data",
                                     "RF_classic",

                                     # parameters

                                     "grid_param",
                                     "lambdaT",
                                     "shapeT",
                                     "shapeC",
                                     "ntree",
                                     "prop_train",
                                     "phi",
                                     "phi.args",
                                     "ev_methods",
                                     "bandwidths",
                                     "types_w_ev",
                                     "max_w_ev",
                                     "seed",

                                     "v_w_RF_mode1_type_w",
                                     "v_w_RF_mode2_type_w",
                                     "v_w_RF_mode1_minleaf",
                                     "v_w_RF_mode2_minleaf",

                                     "v_w_RF_mode1_max_w_mod",
                                     "v_w_RF_mode2_max_w_mod",
                                     "v_w_gam_type_w",
                                     "v_w_gam_max_w_mod",
                                     "v_RSF_reg_minleaf",
                                     "v_RF_minleaf",

                                     "v_RF_maxdepth",
                                     "v_w_RF_mode1_maxdepth",
                                     "v_w_RF_mode2_maxdepth",
                                     "v_RSF_maxdepth"
                                   ),
                                   .verbose = F) %dopar% {

                                     set.seed(seed + k)

                                     list_data = generate_censored_data(n_simul = grid_param$n_simul[i],
                                                                        n_vars = grid_param$n_vars[i],
                                                                        prop_censored = grid_param$prop_censored[i],
                                                                        lambdaT = lambdaT,
                                                                        shapeT = shapeT,
                                                                        target_R2_C = grid_param$target_R2_C[i],
                                                                        shapeC = shapeC,
                                                                        censoring_threshold = grid_param$censoring_threshold[i],
                                                                        mixture_T = grid_param$mixture_T[i],
                                                                        mixture_T_type = switch(is.na(grid_param$mixture_T_type[i]) + 1, grid_param$mixture_T_type[i], NULL),
                                                                        mixture_T_n_groups = switch(is.na(grid_param$mixture_T_n_groups[i]) + 1, grid_param$mixture_T_n_groups[i], NULL)
                                     )

                                     data_sim = list_data$data_sim

                                     train_lines = sample(1:nrow(data_sim),
                                                          size = floor(prop_train * nrow(data_sim)))


                                     # Computation of mat_w through a call to cox_reg

                                     # impossible to make a unique Call to cox_reg since we use a max_w_ev argument
                                     # in the call in which we compute the results
                                     res_mat_w = cox_reg(y_var = "y",
                                                         delta_var = "delta",
                                                         x_vars = paste0("v",1:grid_param$n_vars[i]),
                                                         train = data_sim[train_lines, ],
                                                         test = data_sim[-train_lines, ],
                                                         max_time = grid_param$censoring_threshold[i],
                                                         ev_methods = c("weighted"),
                                                         types_w_ev = c("KM", "Cox", "RSF", "unif"),
                                                         max_w_ev = 1000,
                                                         cox_obj = F)

                                     mat_w = rbind(res_mat_w$mat_w_train,
                                                   res_mat_w$mat_w_test)


                                     ########### results of the models


                                     param_RF_classic = expand.grid(type_pred = NA,
                                                                    algo = "RF_bench",
                                                                    type_w = NA,
                                                                    minleaf = v_RF_minleaf,
                                                                    maxdepth = v_RF_maxdepth,
                                                                    max_w_mod = NA)

                                     res_RF_classic = do.call(rbind, lapply(X = 1:nrow(param_RF_classic),
                                                                            FUN = function(j){
                                                                              unlist(RF_classic(
                                                                                y_no_cens_var = "Te",
                                                                                x_vars = paste0("v",1:grid_param$n_vars[i]),
                                                                                delta_var = "delta",
                                                                                y_var = "y",
                                                                                train = data_sim[train_lines,],
                                                                                test = data_sim[-train_lines,],
                                                                                phi = phi,
                                                                                phi.args = phi.args,
                                                                                max_time = grid_param$censoring_threshold[i],
                                                                                ev_methods = ev_methods,
                                                                                mat_w = mat_w,
                                                                                types_w_ev = types_w_ev,
                                                                                max_w_ev = max_w_ev,
                                                                                bandwidths = bandwidths,
                                                                                minleaf = param_RF_classic$minleaf[j],
                                                                                maxdepth = param_RF_classic$maxdepth[j],
                                                                                ntree = ntree,
                                                                                #nsplit = 10,
                                                                                mtry = length(grid_param$n_vars[i]))$perf_test)
                                                                            })
                                     )

                                     # w_RF

                                     ## mode1

                                     param_w_RF_mode1 = expand.grid(type_pred = NA,
                                                                    algo = "w_RF1",
                                                                    type_w = v_w_RF_mode1_type_w,
                                                                    minleaf = v_w_RF_mode1_minleaf,
                                                                    maxdepth = v_w_RF_mode1_maxdepth,
                                                                    max_w_mod = v_w_RF_mode1_max_w_mod)


                                     res_w_RF_mode1 = do.call(rbind, lapply(X = 1:nrow(param_w_RF_mode1),
                                                                            FUN = function(j){
                                                                              unlist(sw_reg(
                                                                                y_var = "y",
                                                                                delta_var = "delta",
                                                                                x_vars = paste0("v",1:grid_param$n_vars[i]),
                                                                                train = data_sim[train_lines,],
                                                                                test = data_sim[-train_lines,],
                                                                                type_reg = "RF",
                                                                                type_w = param_w_RF_mode1$type_w[j],
                                                                                max_time =  grid_param$censoring_threshold[i],
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
                                                                                minleaf = param_w_RF_mode1$minleaf[j],
                                                                                maxdepth = param_w_RF_mode1$maxdepth[j],
                                                                                #nsplit = 10,
                                                                                ntree = ntree,
                                                                                mtry = length(grid_param$n_vars[i]),
                                                                                sampsize = length(train_lines), # for the sample size being the same as the train dataset
                                                                                y_no_cens_var = "Te"
                                                                              )$perf_test)
                                                                            }
                                     ))

                                     param_w_RF_mode2 = expand.grid(type_pred = c("weights_tree", "KM_local"),
                                                                    algo = "w_RF2",
                                                                    type_w = v_w_RF_mode2_type_w,
                                                                    minleaf = v_w_RF_mode2_minleaf,
                                                                    maxdepth = v_w_RF_mode2_maxdepth,
                                                                    max_w_mod = v_w_RF_mode2_max_w_mod)

                                     param_w_RF_mode2_bis = unique(param_w_RF_mode2[,c("algo", "type_w", "minleaf", "maxdepth", "max_w_mod")])

                                     res_w_RF_mode2 = do.call(rbind, lapply(X = 1:nrow(param_w_RF_mode2_bis),
                                                                            FUN = function(j){
                                                                              one_res_w_RF_mode2 = sw_reg(y_var = "y",
                                                                                                          delta_var = "delta",
                                                                                                          x_vars = paste0("v",1:grid_param$n_vars[i]),
                                                                                                          train = data_sim[train_lines,],
                                                                                                          test = data_sim[-train_lines,],
                                                                                                          type_reg = "RF",
                                                                                                          type_w = param_w_RF_mode2_bis$type_w[j],
                                                                                                          max_time =  grid_param$censoring_threshold[i],
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
                                                                                                          y_no_cens_var = "Te",
                                                                                                          mode_sw_RF = 2,
                                                                                                          maxdepth = param_w_RF_mode2_bis$maxdepth[j],
                                                                                                          minleaf = param_w_RF_mode2_bis$minleaf[j] * floor(mean(data_sim[train_lines,"delta"])),
                                                                                                          ntree = ntree,
                                                                                                          cp = 0,
                                                                                                          xval = 0)

                                                                              return(rbind(unlist(one_res_w_RF_mode2$perf_test),
                                                                                           unlist(one_res_w_RF_mode2$perf_test_KMloc)))
                                                                            }
                                     )
                                     )

                                     # w_gam
                                     param_w_gam = expand.grid(type_pred = NA,
                                                               algo = "w_gam",
                                                               type_w = v_w_gam_type_w,
                                                               minleaf = NA,
                                                               maxdepth = NA,
                                                               max_w_mod = v_w_gam_max_w_mod)


                                     res_w_gam = do.call(rbind, lapply(X = 1:nrow(param_w_gam),
                                                                       FUN = function(j){
                                                                         unlist(sw_reg(y_var = "y",
                                                                                       delta_var = "delta",
                                                                                       x_vars = paste0("v",1:grid_param$n_vars[i]),
                                                                                       train = data_sim[train_lines,],
                                                                                       test = data_sim[-train_lines,],
                                                                                       type_reg = "gam",
                                                                                       type_w = param_w_gam$type_w[j],
                                                                                       max_time =  grid_param$censoring_threshold[i],
                                                                                       phi = phi,
                                                                                       phi.args = phi.args,
                                                                                       ev_methods = ev_methods,
                                                                                       types_w_ev = types_w_ev,
                                                                                       max_w_mod = param_w_gam$max_w_mod[j],
                                                                                       max_w_ev = max_w_ev,
                                                                                       sw_reg_obj = F,
                                                                                       cens_mod_obj = F,
                                                                                       bandwidths = bandwidths,
                                                                                       mat_w = mat_w,
                                                                                       y_no_cens_var = "Te"
                                                                         )$perf_test)
                                                                       }
                                     ))

                                     # rsf_reg
                                     param_rsf_reg = expand.grid(type_pred = NA,
                                                                 algo = "rsf_reg",
                                                                 type_w = NA,
                                                                 minleaf = v_RSF_reg_minleaf,
                                                                 maxdepth = v_RSF_maxdepth,
                                                                 max_w_mod = NA)


                                     res_rsf_reg = do.call(rbind, lapply(X = 1:nrow(param_rsf_reg),
                                                                         FUN = function(j){
                                                                           unlist(rsf_reg(y_var = "y",
                                                                                          delta_var = "delta",
                                                                                          x_vars = paste0("v",1:grid_param$n_vars[i]),
                                                                                          train = data_sim[train_lines,],
                                                                                          test = data_sim[-train_lines,],
                                                                                          max_time =  grid_param$censoring_threshold[i],
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
                                                                                          #nsplit = 10,
                                                                                          ntree = ntree,
                                                                                          mtry = length(grid_param$n_vars[i]),
                                                                                          y_no_cens_var = "Te"
                                                                           )$perf_test)
                                                                         }
                                     )
                                     )

                                     # cox_reg
                                     param_cox_reg = expand.grid(type_pred = NA,
                                                                 algo = "cox_reg",
                                                                 type_w = NA,
                                                                 minleaf = NA,
                                                                 maxdepth = NA,
                                                                 max_w_mod = NA)

                                     res_cox_reg = do.call(rbind, lapply(X = 1:nrow(param_cox_reg),
                                                                         FUN = function(j){
                                                                           unlist(cox_reg(y_var = "y",
                                                                                          delta_var = "delta",
                                                                                          x_vars = paste0("v",1:grid_param$n_vars[i]),
                                                                                          train = data_sim[train_lines,],
                                                                                          test = data_sim[-train_lines,],
                                                                                          types_w_ev = types_w_ev,
                                                                                          max_w_ev = max_w_ev,
                                                                                          max_time = grid_param$censoring_threshold[i],
                                                                                          phi = phi,
                                                                                          phi.args = phi.args,
                                                                                          cox_obj = F,
                                                                                          ev_methods = ev_methods,
                                                                                          mat_w = mat_w,
                                                                                          bandwidths = bandwidths,
                                                                                          y_no_cens_var = "Te"
                                                                           )$perf_test)
                                                                         }
                                     )
                                     )

                                     return(cbind(rbind(param_RF_classic, param_w_RF_mode1, param_w_RF_mode2, param_w_gam, param_rsf_reg, param_cox_reg),
                                                  iter = k,
                                                  rbind(res_RF_classic, res_w_RF_mode1, res_w_RF_mode2, res_w_gam, res_rsf_reg, res_cox_reg)))
                                   }
                     stopCluster(cl)
                     return(cbind( data.frame(grid_param[i,])[rep(as.character(i), nrow(res)),], res))
                   }
            )
    )
  return(tab_results)
}



RF_classic = function(y_no_cens_var,
                      x_vars,
                      delta_var,
                      y_var,
                      train,
                      test = NULL,
                      phi = function(x){x},
                      phi.args = list(),
                      max_time = NULL,
                      ev_methods,
                      mat_w,
                      types_w_ev,
                      max_w_ev,
                      bandwidths = NULL,
                      RF.object = T,
                      ntree,
                      mtry,
                      maxdepth,
                      minleaf,
                      ...){

  # column names of mat_w should be explicit
  if(!is.null(mat_w) & is.null(colnames(mat_w))) colnames(mat_w) = paste0("w",1:ncol(mat_w))

  ev_methods <- match.arg(as.character(ev_methods), c("concordance", "group", "weighted"), several.ok = T)
  if (is.null(bandwidths) & ("group" %in% ev_methods)) bandwidths = 50
  types_w_ev = match.arg(as.character(types_w_ev), c("KM", "Cox", "RSF", "unif"), several.ok = T)

  train = train[,c(x_vars, y_no_cens_var, delta_var, y_var)]
  test = test[,c(x_vars, y_no_cens_var, delta_var, y_var)]

  if (!is.null(test)){
    data = rbind(train, test)
    data$is_train = c(rep(1, nrow(train)), rep(0, nrow(test)))
  } else {
    data = train
    data$is_train = 1
  }

  if (("group" %in% ev_methods) & (nrow(data) < 500)){
    bandwidths = pmin(bandwidths, ifelse(!is.null(test), nrow(test), nrow(train)))
    stop("group performance criteria must not be accurate because it
         needs more observations to converge")
  }

  if (is.null(max_time)){max_time = max(train[which(train[, delta_var] == 1), y_var])}

  data$y_prime = pmin(data[,y_var], max_time)
  data$delta_prime = 1 * ((data[,delta_var] != 0) | (data[,y_var] >= max_time))
  data$phi = sapply(X = 1:length(data$y_prime),
                    FUN = function(i){do.call(phi, c(list(x=data$y_prime[i]), phi.args))})
  if(!is.null(y_no_cens_var)){
    data$phi_non_censored = sapply(X = 1:nrow(data),
                                   FUN = function(i){do.call(phi, c(list(x=pmin(data[,y_no_cens_var], max_time)[i]), phi.args))})
  }

  # Computation of the weitghts if not provided
  if (is.null(mat_w)){
    mat_w_train = matrix(rep(0, length(types_w_ev) * sum(data$is_train == 1) ), ncol = length(types_w_ev))
    colnames(mat_w_train) = types_w_ev
    if (!is.null(test)){
      mat_w_test = matrix(rep(0, length(types_w_ev) * sum(data$is_train == 0) ), ncol = length(types_w_ev))
      colnames(mat_w_test) = types_w_ev
    }
    for (j in 1:length(types_w_ev)){
      mat_w_train[,j] = sword:::make_weights(data = data[data$is_train == 1, ],
                                     y_name = "y_prime",
                                     delta_name = "delta_prime",
                                     y_name2 = y_var,
                                     delta_name2 = delta_var,
                                     type = types_w_ev[j],
                                     max_ratio_weights = 1000,
                                     x_vars = x_vars,
                                     cens_mod_obj = FALSE)$weights
      if (!is.null(test)){
        mat_w_test[,j] = sword:::make_weights(data = data[data$is_train == 0, ],
                                      y_name = "y_prime",
                                      delta_name = "delta_prime",
                                      y_name2 = y_var,
                                      delta_name2 = delta_var,
                                      type = types_w_ev[j],
                                      max_ratio_weights = 1000,
                                      x_vars = x_vars,
                                      cens_mod_obj = FALSE)$weights
      }
    }
  }

  # Build mat_w_train & mat_w_test if mat_w provided
  if (!is.null(mat_w)){
    mat_w_train = mat_w[1:nrow(train),]
    if (!is.null(test)){
      mat_w_test = mat_w[(nrow(train)+1):(nrow(data)),]
    }
  }

  # Thresholding of the weights_eval
  ## train

  n_w_ev_modif_train = apply(X = mat_w_train, MARGIN = 2,
                             FUN = function(x){
                               x = sum(x > min(x[x > 0]) * max_w_ev)
                             })

  mat_w_train = apply(X = mat_w_train, MARGIN = 2,
                      FUN = function(x){
                        x = pmin(x, min(x[x > 0]) * max_w_ev)
                        x = x / sum(x)
                      })
  ## test
  if (!is.null(test)){

    n_w_ev_modif_test = apply(X = mat_w_test, MARGIN = 2,
                              FUN = function(x){
                                x = sum(x > min(x[x > 0]) * max_w_ev)
                              })

    mat_w_test = apply(X = mat_w_test, MARGIN = 2,
                       FUN = function(x){
                         x = pmin(x, min(x[x > 0]) * max_w_ev)
                         x = x / sum(x)
                       })
  }

  # Build train & test
  train = data[data$is_train == 1,]
  if (!is.null(test)){
    test = data[data$is_train == 0,]
  }

  # Calibration of RSF model

  rfSRC = randomForestSRC::rfsrc( formula = phi_non_censored ~ .,
                                  data = train[,c("phi_non_censored",x_vars)],
                                  forest = T,
                                  ntree = ntree,
                                  mtry = mtry,
                                  nodesize = minleaf,
                                  nodedepth = maxdepth,
                                  ...)

  overfitted_predictions_RF_classic = randomForestSRC::predict.rfsrc(rfSRC ,
                                                                     train[,x_vars])$predicted

  # Performances on train test
  perf_train = sword:::eval_model(predictions = overfitted_predictions_RF_classic,
                          data = train,
                          phi_name = "phi",
                          y_name = "y_prime",
                          delta_name = "delta_prime",
                          max_time = max_time,
                          ev_methods = ev_methods,
                          phi = phi,
                          phi.args = phi.args,
                          mat_w = mat_w_train,
                          phi_non_censored_name = "phi_non_censored",
                          bandwidths = bandwidths)

  if (!is.null(test)){
    test_predictions_RF_classic = randomForestSRC::predict.rfsrc(rfSRC,
                                                                 test[,x_vars])$predicted

    # Performances on test set
    perf_test = sword:::eval_model(predictions = test_predictions_RF_classic,
                           data = test,
                           phi_name = "phi",
                           y_name = "y_prime",
                           delta_name = "delta_prime",
                           max_time = max_time,
                           ev_methods = ev_methods,
                           phi = phi,
                           phi.args = phi.args,
                           mat_w = mat_w_test,
                           phi_non_censored_name = "phi_non_censored",
                           bandwidths = bandwidths)
  }

  result = list(
    pred_train = overfitted_predictions_RF_classic,
    perf_train = perf_train,
    train = train[,c(y_var, delta_var, "y_prime", "delta_prime", "phi", "phi_non_censored", x_vars)],
    mat_w_train = mat_w_train,
    max_time = max_time,
    phi = phi,
    phi.args = phi.args,
    x_vars = x_vars,
    cens_rate = sum(data$delta_prime == 0) / nrow(data)
  )
  if (RF.object){result$RF.object = rfSRC}
  if (!is.null(test)){
    result$pred = test_predictions_RF_classic
    result$perf_test = perf_test
    result$test = test[,c(y_var, delta_var, "y_prime", "delta_prime", "phi", "phi_non_censored", x_vars)]
    result$mat_w_test = mat_w_test
  }
  return(result)
}

