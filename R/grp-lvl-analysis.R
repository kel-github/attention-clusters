# contains functions to wrangle data for the att-clust-write-up Rmd files

score_accuracy <- function(raw){
  # produce an accuracy score for each participant
  # kwargs:
  # -- raw: raw data from get_raw_data
  raw %>% select(sub, reward_type, cert, resp) %>%
          group_by(sub, reward_type, cert) %>%
          summarise(acc = mean(resp)) %>%
          ungroup()
}

compute_rt_score <- function(clean_dat){
  # produce RT scores for each participant
  # kwargs
  # -- clean_dat - group data gotten using clean_data function
  clean_dat %>% group_by(sub, cert, reward_type) %>%
                summarise(mu = median(rt)) %>% ungroup()
}


do_anova_test <- function(data, dv){
  # perform group level ANOVA
  get_anova_table(anova_test(data=data%>%ungroup(), dv=eval(dv), wid=sub, within=c(cert,reward_type), effect.size="pes", type=3))
}

get_cert_posthoc <- function(sum_dat){
  # perform post-hoc comparisons
  # t-tests comparing the key levels of the cert iv (p=.2 > .5, .5 > .8)
  # kwargs
  # -- sum_dat: the same data.frame as fed into do_anova_test
  cert_sum <- sum_dat %>% group_by(sub, cert) %>%
                          summarise(rt = mean(mu),
                                    ie = mean(inveff))
  
  cert_comps <- data_frame(x = c(".2", ".5"),
                           y = c(".5", ".8"))
  
  do_t_rt <- function(cert_sum, x, y, dv = "rt"){
     t.test(x = cert_sum$rt[cert_sum$cert == x],
            y = cert_sum$rt[cert_sum$cert == y],
            paired = TRUE,
            var.equal = FALSE)
  }
  
  do_t_ie <- function(cert_sum, x, y, dv = "ie"){
    t.test(x = cert_sum$ie[cert_sum$cert == x],
           y = cert_sum$ie[cert_sum$cert == y],
           paired = TRUE,
           var.equal = FALSE)
  }
  
  cert_ts_rt <- mapply(do_t_rt, x = cert_comps$x, y = cert_comps$y, 
                       MoreArgs = list(cert_sum = cert_sum))
  
  cert_ts_ie <- mapply(do_t_ie, x = cert_comps$x, y = cert_comps$y, 
                       MoreArgs = list(cert_sum = cert_sum))
  list(cert_sum, cert_comps, cert_ts_rt, cert_ts_ie)
}


get_val_posthoc <- function(sum_dat){
  # perform post-hoc comparisons
  # t-tests performing pairwise testing of the 'reward_type' factor
  # kwargs
  # -- sum_dat: the same data.frame as fed into do_anova_test
  
  val_sum <- sum_dat %>% group_by(sub, reward_type) %>%
    summarise(rt = mean(mu),
              ie = mean(inveff))
  
  
  val_comps <- data_frame(x = c("htgt/hdst", "htgt/hdst", "htgt/hdst", "htgt/ldst", "htgt/ldst", "ltgt/ldst"),
                          y = c("htgt/ldst", "ltgt/ldst", "ltgt/hdst", "ltgt/ldst", "ltgt/hdst", "ltgt/hdst"))
  
  do_t_rt <- function(val_sum, x, y, dv = "rt"){
    t.test(x = val_sum$rt[val_sum$reward_type == x],
           y = val_sum$rt[val_sum$reward_type == y],
           paired = TRUE,
           var.equal = FALSE)
  }
  
  do_t_ie <- function(val_sum, x, y, dv = "ie"){
    t.test(x = val_sum$ie[val_sum$reward_type == x],
           y = val_sum$ie[val_sum$reward_type == y],
           paired = TRUE,
           var.equal = FALSE)
  }
  
  val_ts_rt <- mapply(do_t_rt, x = val_comps$x, y = val_comps$y, 
                       MoreArgs = list(val_sum = val_sum))
  
  val_ts_ie <- mapply(do_t_ie, x = val_comps$x, y = val_comps$y, 
                       MoreArgs = list(val_sum = val_sum))
  list(val_sum, val_comps, val_ts_rt, val_ts_ie)
}


do_val_BF <- function(sum_dat){
  # perform post-hoc comparisons
  # follow up null differences with a bayesian t-test
  # kwargs
  # -- sum_dat: the same data.frame as fed into do_anova_test
  
  val_sum <- sum_dat %>% group_by(sub, reward_type) %>%
    summarise(rt = mean(mu),
              ie = mean(inveff))
  hhll <- ttestBF(x = val_sum$rt[val_sum$reward_type == "htgt/hdst"], y = val_sum$rt[val_sum$reward_type == "ltgt/ldst"], paired = TRUE)
  hhhl <- ttestBF(x = val_sum$rt[val_sum$reward_type == "htgt/hdst"], y = val_sum$rt[val_sum$reward_type == "htgt/ldst"], paired = TRUE)
  llhl <- ttestBF(x = val_sum$rt[val_sum$reward_type == "ltgt/ldst"], y = val_sum$rt[val_sum$reward_type == "htgt/ldst"], paired = TRUE)
  list(hhll = hhll, hhhl = hhhl, llhl = llhl )
}
