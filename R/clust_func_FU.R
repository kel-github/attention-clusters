# set of functions for following up the results of the clustering analysis
do_anova_test_w_grp <- function(data, dv){
  # perform group level ANOVA
  data[,eval(dv)] <- log(data[,eval(dv)])
  get_anova_table(anova_test(data=data%>%ungroup(), dv=eval(dv), wid=sub, within=c(cert,reward_type), between=group, effect.size="pes", type=3))
}

fu_group_by_cert_int <- function(data){
  # follow up the group by certainty interaction
  # compute the difference between .2 and .5, and
  # .5 and .8 for each group, and then compare using t-tsts
  # data should be the data upon which the anova was run
  data <- data %>% group_by(sub, cert, group) %>%
                   summarise(mu = mean(mu)) %>%
                   ungroup() %>% group_by(sub, group) %>%
                   summarise(d = mu[cert == ".2"] - mu[cert == ".5"],
                             c = mu[cert == ".5"] - mu[cert == ".8"]) %>%
                   ungroup()
  
 list(tc = with(data, t.test(c ~ group, paired = FALSE, var.equal = FALSE)),
      td = with(data, t.test(d ~ group, paired = FALSE, var.equal = FALSE)),
      data = data %>% group_by(group) %>%
                      summarise(cert = mean(c), sd = sd(c), N = length(c),
                                uncert = mean(d), sdu = sd(d)))
}


fu_group_by_val_by_cert_int <- function(data){
  # follow up the group by certainty by value interaction
  # compute a value x certainty anova for each group
  
  lapply(unique(data$group), function(x)
                            get_anova_table(anova_test(data=data%>%ungroup()%>%filter(group==x), 
                                           dv=mu, 
                                           wid=sub, 
                                           within=c(cert,reward_type), 
                                           effect.size="pes", type=3)))
}

fu_group_by_val_int_acc <- function(data){
  # follow up the group by certainty by value interaction
  # compute a value x certainty anova for each group
  
  data <- data %>% group_by(sub, group, reward_type) %>%
                   summarise(acc = mean(acc)) %>%
                   ungroup()
  
  lapply(unique(data$group), function(x)
    get_anova_table(anova_test(data=data%>%ungroup()%>%filter(group==x), 
                               dv=acc, 
                               wid=sub, 
                               within=c(reward_type), 
                               effect.size="pes", type=3)))
}

do_cert_models_4k <- function(cert_grp_reg_dat){
  # perform lme models and model selection for the spatial certainty factor
  # kargs:
  # -- cert_grp_reg_dat: data frame of summary rts generated from the post-hoc testing functions,
  #                      combined with spatial regressors
  sel_hist_rt <- lmer(mu~sel_hist + (sel_hist|sub), data=cert_grp_reg_dat, REML = FALSE)
  c_fact_rt <- lmer(mu~counterf + (counterf|sub), data=cert_grp_reg_dat, REML = FALSE)
  grp_cert_comp_rt <- anova(sel_hist_rt, c_fact_rt)
  
  list(sel_hist_rt, c_fact_rt, grp_cert_comp_rt)
}

do_val_models_4k <- function(val_grp_reg_dat){
  # perform lme models and model selection for the incentive value factor
  # kargs:
  # -- val_grp_reg_dat: data frame of summary rts generated from the post-hoc testing functions,
  #                      combined with spatial regressors
  
  # Note that if subject specific slopes were included, then there was boundary (singular) fit
  rv_rt <- lmer(mu ~ rv + (rv|sub), data = val_grp_reg_dat, REML = FALSE)
  c_fact_rt <- lmer( mu ~ counterf + (counterf|sub), data = val_grp_reg_dat, REML = FALSE)
  mot_rt <- lmer(mu ~ mot + (mot|sub), data = val_grp_reg_dat, REML = FALSE)
  grp_val_comp_rt <- anova(rv_rt, c_fact_rt, mot_rt)
  list(rv_rt, c_fact_rt, mot_rt, grp_val_comp_rt)
}

#### functions for clusters including temporal element
do_anova_test_w_grp_and_t <- function(data, dv){
  # perform group level ANOVA
  get_anova_table(anova_test(data=data%>%ungroup(), dv=eval(dv), wid=sub, within=c(cert,reward_type, block), between=group, effect.size="pes", type=3))
}