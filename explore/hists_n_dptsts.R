#### K. Garner
#### looking for clues that a mixture of cue uses may 
#### contribute to ltgt/hdst conditions
#### using: https://link.springer.com/article/10.3758/s13428-012-0225-x
##############################################################################
#### clear stuff and load packages
# set wd to source file location
rm(list=ls())
set.seed(42) # meaning of life
library(tidyverse)
library(pracma)
library(car)
library(gridExtra)
library(plotly)
library(htmlwidgets)
library(diptest)
library(mclust)

source("../R/behavioural-data-analysis-functions.R")

#############################################################################
## source previous work
source("simul_cue_use.R")
head(both)
both_subs <- as.numeric(names(both)) # get the subs that showed an effect
val_subs <- as.numeric(names(val))
cert_subs <- as.numeric(names(cert))

plt_sub_hsts <- function(sub_n, all_dat, fol_nm, fnm){
  # sub_n = integer [1]
  # all_dat = cleaned dataframe for all subs
  # fol_nm = str - name of figure subfolder (in ../images/)
  # fnm = str = filestem for filename - contains a "%d" for
  # sub number
  
  # for the cert == p.8 cond, plot a histogram 
  # for each value condition
  p <- all_dat %>% filter(sub == sub_n) %>%
          filter(cert == ".8") %>%
           ggplot(aes(x=rt, col=reward_type, 
                      fill=reward_type, 
                      group=reward_type)) +
           geom_histogram(alpha=0.5) +
           facet_wrap(~reward_type)
  ggsave(paste("../images", fol_nm, sprintf(fnm, sub_n), sep = "/"), 
         p, units = "cm", width = 16, height = 16)
  
}

lapply(both_subs, plt_sub_hsts, all_dat=clean_dat,
       fol_nm="p8_val_hsts/both", fnm="sub-%d_p8VH.pdf")
lapply(cert_subs, plt_sub_hsts, all_dat=clean_dat,
       fol_nm="p8_val_hsts/cert", fnm="sub-%d_p8VH.pdf")
lapply(val_subs, plt_sub_hsts, all_dat=clean_dat,
       fol_nm="p8_val_hsts/val", fnm="sub-%d_p8VH.pdf")  

# also, group level histograms
clean_dat %>% filter(sub %in% both_subs) %>%
  filter(cert == ".8") %>%
  ggplot(aes(x=rt, col=reward_type,
         fill=reward_type,
         group=reward_type)) +
   geom_histogram() +
   facet_wrap(~reward_type)

clean_dat %>% filter(sub %in% val_subs) %>%
  filter(cert == ".8") %>%
  ggplot(aes(x=rt, col=reward_type,
             fill=reward_type,
             group=reward_type)) +
  geom_histogram() +
  facet_wrap(~reward_type)

clean_dat %>% filter(sub %in% cert_subs) %>%
  filter(cert == ".8") %>%
  ggplot(aes(x=rt, col=reward_type,
             fill=reward_type,
             group=reward_type)) +
  geom_histogram() +
  facet_wrap(~reward_type)


###############################################################################\
## 1. apply hartigan's dip test, per subject and value condition in p=.8
## condition
get_dps_per_sub <- function(sub_n, all_dat, cert_val = ".8"){
  # returns for 1 subject, a dataframe containing
  # the results of hartigans dip test
  # for each value condition, at the defined
  # level of cert
  # sub_n [int]: subject number
  # all_dat: cleaned dataframe
  # cert_val [str]: a spatial certainty condition - e.g. ".8"
  
  val_conds <- unique(all_dat$reward_type)
  dips <- lapply(val_conds, function(x) dip.test(all_dat$rt[all_dat$sub == sub_n &
                                               all_dat$cert == cert_val &
                                               all_dat$reward_type == x],
                                                simulate.p.value = TRUE))
  names(dips) <- val_conds
  ps <- do.call(cbind, lapply(val_conds, function(x) dips[[x]]$p.value))
  tibble(sub = rep(sub_n, times = length(val_conds)),
             ps = c(ps), 
             reward_type = val_conds)
}

dip_dat <- do.call(rbind, lapply(both_subs, get_dps_per_sub, all_dat = clean_dat))
# do group level
lapply(unique(all_dat$reward_type), function(x) dip.test(all_dat$rt[all_dat$cert == ".8" &
                                                                    all_dat$reward_type == x]))
# all n.sig

###############################################################################
# Gaussian Mixture model
##############################################################################
## 2. apply GMM, per subject and value condition in p=.8
## condition
get_gmm_per_sub <- function(sub_n, all_dat, cert_val = ".8"){
  # returns for 1 subject, a dataframe containing
  # the bic values for both 1 and 2 component
  # GMM models for each condition
  # level of cert
  # sub_n [int]: subject number
  # all_dat: cleaned dataframe
  # cert_val [str]: a spatial certainty condition - e.g. ".8"
  
  val_conds <- unique(all_dat$reward_type)
  gmms1 <- lapply(val_conds, function(x) summary(Mclust(all_dat$rt[all_dat$sub == sub_n &
                                                          all_dat$cert == cert_val &
                                                          all_dat$reward_type == x],
                                                 G=1, model="V")))
  names(gmms1) <- val_conds
  gmms2 <- lapply(val_conds, function(x) summary(Mclust(all_dat$rt[all_dat$sub == sub_n &
                                                             all_dat$cert == cert_val &
                                                             all_dat$reward_type == x],
                                                G=2, model="V")))
  names(gmms2) <- val_conds

  bics1 <- do.call(cbind, lapply(val_conds, function(x) gmms1[[x]]$bic))
  bics2 <- do.call(cbind, lapply(val_conds, function(x) gmms2[[x]]$bic))
  bim <- bics2 < bics1 # which conditions show bimodality
  tibble(sub = rep(sub_n, times = length(val_conds)),
         bics1=c(bics1),
         bics2=c(bics2),
         bim=c(bim),
         reward_type = val_conds)
}

gmm_dat <- do.call(rbind, lapply(both_subs, get_gmm_per_sub, all_dat = clean_dat))

# classify subs
## Note: this seems overly sensitive - doing one on all RT data to see what we get
## and then sort which trials belong to which

##
clss_gmm_types = gmm_dat %>% group_by(sub) %>%
          summarise(type = if_else( bim[reward_type == "ltgt/hdst"] & sum(bim[reward_type != "ltgt/hdst"]) == 0,
                     "two_ltgt",
                     if_else( bim[reward_type == "ltgt/hdst"] & bim[reward_type == "htgt/ldst"] & 
                                sum(bim[reward_type == "htgt/hdst" | reward_type == "ltgt/ldst"] == 0),
                              "two_val", "other")))

clss_sub_dat <- inner_join(clean_dat %>% filter(sub %in% both_subs),
                           clss_gmm_types, by="sub")  



get_gmm_per_sub_alltrls <- function(sub_n, all_dat, cert_val = ".8"){
  # returns for 1 subject, a dataframe containing
  # the bic values for both 1 and 2 component
  # GMM models for each condition
  # level of cert
  # sub_n [int]: subject number
  # all_dat: cleaned dataframe
  # cert_val [str]: a spatial certainty condition - e.g. ".8"
  

  gmms1 <- summary(Mclust(all_dat$rt[all_dat$sub == sub_n &
                                     all_dat$cert == cert_val],
                                     G=1, model="V"))

  gmms2 <- summary(Mclust(all_dat$rt[all_dat$sub == sub_n &
                                     all_dat$cert == cert_val],
                                     G=2, model="V"))

  bim <- gmms1$bic > gmms2$bic # which conditions show bimodality
  tibble(sub = sub_n,
         bics1=gmms1$bic,
         bics2=gmms2$bic,
         bim=bim)
}

gmm_alltrls_dat <- do.call(rbind, lapply(both_subs, get_gmm_per_sub_alltrls, all_dat = clean_dat))
sum(gmm_all_dat$bim)/length(gmm_all_dat$bim) # 20 % participants show bimodal distribution
# question then becomes: are their ltgt/dst trials split between the two gaussians?


### still wondering whether quantile regression would be better