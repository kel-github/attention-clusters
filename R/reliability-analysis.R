get_data_4_reliability <- function(raw, clean_dat){
  
  # first get accuracy for even and odd trials
  odd <- with(raw, as.logical(t %% 2))
  acc <- inner_join( rbind(raw[odd, ] %>% group_by(sub, cert) %>% 
                        summarise(acc = mean(resp)) %>% 
                        ungroup() %>%
                        group_by(sub) %>%
                        summarise(ts = "odd",
                               cue_eff = acc[cert == ".2"] - acc[cert == ".8"]) %>%
                        ungroup(),
                      raw[!odd, ] %>% group_by(sub, cert) %>% 
                        summarise(acc = mean(resp)) %>% 
                        ungroup() %>%
                        group_by(sub) %>%
                        summarise(ts = "even",
                                  cue_eff = acc[cert == ".2"] - acc[cert == ".8"]) %>%
                        ungroup()),
                     rbind(raw[odd, ] %>% group_by(sub, reward_type) %>%
                                     summarise(acc = mean(resp)) %>%
                                     ungroup() %>%
                                     group_by(sub) %>%
                                     summarise(ts = "odd",
                                               hhll = acc[reward_type == "htgt/hdst"] - acc[reward_type == "ltgt/ldst"],
                                               hhlh = acc[reward_type == "htgt/hdst"] - acc[reward_type == "ltgt/hdst"],
                                               hhhl = acc[reward_type == "htgt/hdst"] - acc[reward_type == "htgt/ldst"]) %>%
                                     ungroup(),
                            raw[!odd, ] %>% group_by(sub, reward_type) %>%
                                    summarise(acc = mean(resp)) %>%
                                    ungroup() %>%
                                    group_by(sub) %>%
                                    summarise(ts = "even",
                                              hhll = acc[reward_type == "htgt/hdst"] - acc[reward_type == "ltgt/ldst"],
                                              hhlh = acc[reward_type == "htgt/hdst"] - acc[reward_type == "ltgt/hdst"],
                                              hhhl = acc[reward_type == "htgt/hdst"] - acc[reward_type == "htgt/ldst"]) %>%
                                    ungroup()), by = c("sub", "ts"))
    
    
           

               
  odd <- with(clean_dat, as.logical(t %% 2))
  rt <- inner_join( rbind(clean_dat[odd, ] %>% group_by(sub, cert) %>% 
                             summarise(rt = mean(rt)) %>% 
                             ungroup() %>%
                             group_by(sub) %>%
                             summarise(ts = "odd",
                                       cue_eff = rt[cert == ".2"] - rt[cert == ".8"]) %>%
                             ungroup(),
                           clean_dat[!odd, ] %>% group_by(sub, cert) %>% 
                             summarise(rt = mean(rt)) %>% 
                             ungroup() %>%
                             group_by(sub) %>%
                             summarise(ts = "even",
                                       cue_eff = rt[cert == ".2"] - rt[cert == ".8"]) %>%
                             ungroup()),
                     rbind(clean_dat[odd, ] %>% group_by(sub, reward_type) %>%
                             summarise(rt = mean(rt)) %>%
                             ungroup() %>%
                             group_by(sub) %>%
                             summarise(ts = "odd",
                                       hhll = rt[reward_type == "htgt/hdst"] - rt[reward_type == "ltgt/ldst"],
                                       hhlh = rt[reward_type == "htgt/hdst"] - rt[reward_type == "ltgt/hdst"],
                                       hhhl = rt[reward_type == "htgt/hdst"] - rt[reward_type == "htgt/ldst"]) %>%
                             ungroup(),
                           clean_dat[!odd, ] %>% group_by(sub, reward_type) %>%
                             summarise(rt = mean(rt)) %>%
                             ungroup() %>%
                             group_by(sub) %>%
                             summarise(ts = "even",
                                       hhll = rt[reward_type == "htgt/hdst"] - rt[reward_type == "ltgt/ldst"],
                                       hhlh = rt[reward_type == "htgt/hdst"] - rt[reward_type == "ltgt/hdst"],
                                       hhhl = rt[reward_type == "htgt/hdst"] - rt[reward_type == "htgt/ldst"]) %>%
                             ungroup()), by = c("sub", "ts"))
  
  
  list(acc, rt)
}

produce_rel_cors <- function(rel_dat){
  # takes one element of the output of get_data_4_reliability 
  # converts to wideform, and then produces a
  # correlation table, where each metric from odd
  # trials is correlated with that of its even counterpart
  rel_dat_wide <- rel_dat %>% pivot_wider(id_cols = sub, 
                                          names_from = c(ts), 
                                          values_from = c(cue_eff, hhll, hhlh, hhhl))
  name_list <- names(rel_dat_wide)
  # first, get all the odd and even variable names
  odd_names <- name_list[str_detect(name_list, "odd")]
  ev_names <- name_list[str_detect(name_list, "even")]
  # luckily the lists match because of how the dataframe was put together
  # but some more robust pattern matching is required if I ever want to make the next step more general
  
  # now a little function to extract the data as I need for each cor
  do_cor_test_on_rel_dat <- function(rel_dat_wide, x, y){
    cor.test(x = rel_dat_wide %>% pull(x), y = rel_dat_wide %>% pull(y))
  }
  # now apply over the odd and even lists to get the correlations between each
  reliab_out <- mapply(do_cor_test_on_rel_dat, x = odd_names, y = ev_names, 
                       MoreArgs = list(rel_dat_wide),
                       SIMPLIFY = FALSE)
  # now I have the results, I want the r values and the p values
  get_cor_out <- do.call(rbind, lapply(1:length(reliab_out), function(x) reliab_out[[x]]$estimate))
  get_df_out <- do.call(rbind, lapply(1:length(reliab_out), function(x) reliab_out[[x]]$parameter))
  get_p_out <- do.call(rbind, lapply(1:length(reliab_out), function(x) reliab_out[[x]]$p.value))
  
  # now I want a good name for each correlation, so I will exctract the names, minus the 'odd'
  # and cbind that to my r's and p's to get a nice dataframe
  nu_names <- str_replace_all(odd_names, "odd_", "")
  data_frame(names = nu_names, r = get_cor_out, df = get_df_out, p = get_p_out)
}

