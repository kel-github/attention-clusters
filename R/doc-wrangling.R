# contains functions to wrangle data for the att-clust-write-up Rmd files

score_accuracy <- function(raw){
  # produce an accuracy score for each participant
  raw %>% select(sub, reward_type, cert, resp) %>%
          group_by(sub, reward_type, cert) %>%
          summarise(acc = mean(resp)) %>%
          ungroup()
}

compute_rt_score <- function(clean_dat){
  # produce RT scores for each participant
  clean_dat %>% group_by(sub, cert, reward_type) %>%
                summarise(mu = median(rt)) %>% ungroup()
}


do_anova_test <- function(data, dv){
  # perform group level ANOVA
  get_anova_table(anova_test(data=data%>%ungroup(), dv=eval(dv), wid=sub, within=c(cert,reward_type), effect.size="pes", type=3))
}

get_data_4_reliability <- function(raw, clean_dat){
  
  # first get accuracy for even and odd trials
  odd <- with(raw, as.logical(t %% 2))
  acc <- rbind(raw[odd, ] %>% group_by(sub) %>% 
                summarise(acc = mean(resp)) %>% 
                mutate(ts = "odd"),
               raw[!odd, ] %>% group_by(sub) %>%
                 summarise(acc = mean(resp)) %>%
                 mutate(ts = "even"))
  odd <- with(clean_dat, as.logical(t %% 2))
  rt <- rbind(clean_dat[odd, ] %>% group_by(sub) %>%
               summarise(mu = mean(rt)) %>%
               mutate(ts = "odd"),
              clean_dat[!odd, ] %>% group_by(sub) %>%
                summarise(mu = mean(rt)) %>%
                mutate(ts = "even"))
  inner_join(acc, rt, by=c("sub", "ts"))
}

