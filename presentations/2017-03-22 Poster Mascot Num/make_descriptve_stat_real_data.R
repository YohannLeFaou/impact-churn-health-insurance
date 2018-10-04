
packages <- c("ggplot2",
  "survival",
  "randomForestSRC",
  #"ranger",
  #"caret",
  #"MASS",
  #"reshape",
  "dplyr"
  #"doParallel"
)
has   <- packages %in% rownames(installed.packages())
if(any(!has)) install.packages(packages[!has])
for(i in packages){ library(i,character.only = TRUE)}


setwd("Y:/Forsides France/06_Solo/Yohann LE FAOU/mission_forsides/Santiane 2016/analyse resiliation v2/")

################################################
##############  chargement des données

load("../data/resiliation_2016.RData")


######################## paramètres généraux
#################################################################################

jours_par_mois_bissextile_deb_juillet = c(31,31,30,31,30,31,31,29,31,30,31,30)
jours_par_mois_normale_deb_juillet = c(31,31,30,31,30,31,31,28,31,30,31,30)


echeancier = c(0, cumsum( c(jours_par_mois_bissextile_deb_juillet,
                            jours_par_mois_normale_deb_juillet,
                            jours_par_mois_normale_deb_juillet,
                            jours_par_mois_normale_deb_juillet)) + 2)
## pour ajouter des années, ajouter des cycles bi_norm_norm_norm


scenario_moyen = list(N_mois_precompte = 12,
                      N_mois_debut_lineaire = 13,
                      pourcentage_precompte = 0.5,
                      pourcentage_linéaire = 0.1,
                      revalo_annuelle = 0.08)

#################################################
########## Construction de la base
#################################################

# maxT = 1463
base_etude = resiliation_2016
base_etude$y = base_etude$y_from_janv_2016
base_etude$delta = base_etude$delta_from_janv_2016
# base_etude$y_prime = pmin(base_etude$y_from_janv_2016, maxT)
# base_etude$delta_prime = ((base_etude$delta == 1) | (base_etude$y >= maxT)) * 1
# base_etude$delta_c = rep(1,dim(base_etude)[1])
# base_etude$delta_c_censored_ = (base_etude$ancienneteContrat_from_janv_2016 ==
#                                   base_etude$y_from_janv_2016) * 1



var5 = c("NiveauGamme_ordered","civilite","zoneGeo_bis",
         "nbEnfants_quali_ordered","regimeTitulaire_bis2","ageAssure_quali_ordered")

var7 = c("NiveauGamme_ordered","civilite","zoneGeo",
         "nbEnfants_quali_ordered","regimeTitulaire_bis","ageAssure_quali_ordered",
         "groupeCanalAcquisition_bis")

# on a supprimé  : groupeCanalAcquisition_bis
# on regroupe certaines modalités de regime_titulaire

base_etude$regimeTitulaire_bis2 = base_etude$regimeTitulaire_bis
base_etude$regimeTitulaire_bis2[base_etude$regimeTitulaire_bis2 == "Alsace Moselle" |
                                  base_etude$regimeTitulaire_bis2 == "Fonctionnaire"] = "Salarié"
base_etude$regimeTitulaire_bis2 = as.factor(as.character(base_etude$regimeTitulaire_bis2))

base_etude$indicateur_Santiane = -1.15450 +
  0.01060 * base_etude$ageTitulaireDistribution +
  (-0.00014) * base_etude$ageTitulaireDistribution^2 +
  0.10700 * base_etude$nbAdultes +
  0.08930 * base_etude$nbEnfants +
  (-0.06500) * (base_etude$zoneGeo_bis == "Gds Metro Sud") +
  (-0.07170) * (base_etude$zoneGeo_bis == "IDF") +
  0.07170 * (base_etude$zoneGeo_bis == "Nord") +
  (-0.05480) * (base_etude$NiveauGamme_ordered == "bas") +
  0.08430 * (base_etude$NiveauGamme_ordered == "haut") +
  (-0.08090) * (base_etude$regimeTitulaire == "Alsace Moselle") +
  0.12240 * (base_etude$regimeTitulaire == "Sans emploi") +
  (-0.25690) * (base_etude$regimeTitulaire == "TnsIndependant")


complete_cases = complete.cases(base_etude[,c("y","delta",var5)])
base_etude = base_etude[complete_cases,]


make_surv_plot4 = function(data, x_var, y_for_title, y_name = "y", delta_name = "delta", line_width = 1.5, title_plus = "",xlim,ylim){
  formula = as.formula( paste0("Surv(time = ", y_name, ", event =", delta_name," ) ~ " ,x_var))
  fit= survfit(formula,
               data=data,
               type = "kaplan-meier")
  
  list_surv_curves = extract_surv_curv_to_plot(fit)
  to_plot = do.call(rbind, list_surv_curves)
  to_plot[,x_var] = factor(to_plot$classe, 
                           levels = levels(as.factor(to_plot$classe)),
                           labels = paste0("lev_",1:length(levels(base_etude[,x_var]))) )
  to_plot$surv = to_plot$surv^0.6

  surv_plot = ggplot(data = to_plot , aes_string(x = "temps" , y = "surv", 
                                                 group = x_var, colour = x_var)) +
    geom_line(size = line_width) +
    guides(colour = guide_legend(override.aes = list(size = 7), title = "Levels : ") ) +
    theme(legend.position="bottom",
          text = element_text(size=15),
          plot.title = element_text(hjust = 0.5)) +
    xlab("Time (days)") +
    ylab("Survival Prob.") +
    ggtitle(paste0(y_for_title," : Variable ", x_var ," ", title_plus) ) +
    ylim(ylim) +
    xlim(xlim)
  return(surv_plot)
}

plot_NiveauGamme_ordered = make_surv_plot4(data = base_etude, 
                                           x_var = "NiveauGamme_ordered", 
                                           y_for_title = "T", 
                                           xlim = c(0,2500), 
                                           ylim = c(0.2,1))

plot_civilite = make_surv_plot4(data = base_etude, 
                                x_var = "civilite", 
                                y_for_title = "T", 
                                xlim = c(0,2500), 
                                ylim = c(0.2,1))

plot_zoneGeo_bis = make_surv_plot4(data = base_etude, 
                                           x_var = "zoneGeo_bis", 
                                           y_for_title = "T", 
                                           xlim = c(0,2500), 
                                           ylim = c(0.2,1))

plot_nbEnfants_quali_ordered = make_surv_plot4(data = base_etude, 
                                           x_var = "nbEnfants_quali_ordered", 
                                           y_for_title = "T", 
                                           xlim = c(0,2500), 
                                           ylim = c(0.2,1))

plot_regimeTitulaire_bis2 = make_surv_plot4(data = base_etude, 
                                           x_var = "regimeTitulaire_bis2", 
                                           y_for_title = "T", 
                                           xlim = c(0,2500), 
                                           ylim = c(0.2,1))

plot_ageAssure_quali_ordered = make_surv_plot4(data = base_etude, 
                                           x_var = "ageAssure_quali_ordered", 
                                           y_for_title = "T", 
                                           xlim = c(0,2500), 
                                           ylim = c(0.2,1))

multiplot(plot_NiveauGamme_ordered, plot_civilite, plot_zoneGeo_bis,
          plot_nbEnfants_quali_ordered, plot_regimeTitulaire_bis2, 
          plot_ageAssure_quali_ordered, cols = 3)



