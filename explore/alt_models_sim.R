rm(list=ls())

set.seed(42)
library(tidyverse)
## Ami's qs from 32:11 mins
## https://unsw.zoom.us/rec/play/rz5ne0aTE99a-Xh0ulAKVHxX-ho4xjW_Tk-BQnd9xD1Hf69YK_KCfPpnyiVTwcIPB2hzo4oouvebQ26l.doI_Spk4TZsPGrAK?canPlayFromShare=true&from=my_recording&continueMode=true&componentName=rec-play&originRequestUrl=https%3A%2F%2Funsw.zoom.us%2Frec%2Fshare%2F9KqJ6AqzJRY8dRMBiNu-ElskD-UTz3Zp1yr2xoPxiGDeTtQ3VJwBpSRSl9LIwHBH.RWD0feKDih9jROmg
## is there any way in which we can think about the reward cues as 
## different cues?
## purple on right - orange on left - 
## is it possible a similar prediction is made, not by mixing reward and 
## certainty, but by looking selectively at cues?
## e.g. if I pick the purple one and I stick with that for a bit
## then if I pick the orange one and stick with that for a bit
## plan = simulate data where p's start with the purple one p of the time, and 
## switch to orange, and p of the time, they start with orange and switch to purple
## ps of [1,0], [.5,.5], [0,1]
## set parameters

sim_RTs <- function(t0, b, a, s, v, n){
  # simulate RTs using the LBA model
  # Input Args:
  # LBA parameters
  # ~~~~~~~~~~~~~~
  # t0: non-decision/basetime, e.g. 0.1
  #    b: criterion, e.g. 2
  #    a: upper end of startpoint distribution - e.g. .5
  #    s: std dev of rates, e.g. 1
  #    v: value of drift, e.g. 6 for fast, 4 for low
  # Other
  # ~~~~~~~~~~~~~~
  #    n: number of trials
  
  drifts = rnorm(n, mean=v, sd=s); # sample random values for drift rates across trials
  A = runif(n, min=0, max=a); # simulate start points for each trial
  RTs = ((b-A)/drifts) + t0; # RTs are distance over speed, plus non-decision time
  RTs
}


t0 = 0.1; # non-decision time
b = 2.0; # criterion
a = 0.5; # upper end of start point distribution
s = 1.0; # standard deviation of drift rates
# now set drift rates 
base_v = 10;
plus_v = -4; 
n_trials = 160

# case 1, participant prioritises purple with p=1
p_purple <- matrix(c(1, 0, # hvl
                     1, 0, # hvl
                     1, 0, # hvh
                     1, 1, # hvh
                     1, 0, # lvl
                     1, 1, # lvl
                     1, 1, # lvh
                     1, 1), # lvh
                   nrow = 8, ncol =2,
                   byrow = T)
p_purple <- kronecker(p_purple, rep(1, times=20))
p_purple_vs <- p_purple %*% c(base_v, plus_v) 

p_purple_ys <- unlist(lapply(p_purple_vs, sim_RTs, t0=t0, b=b, a=a, s=s, n = 1))

p_orange <- matrix(c(1,1,
                     1,1,
                     1,0,
                     1,1,
                     1,0,
                     1,1,
                     1,0,
                     1,0),
                   nrow = 8, ncol = 2,
                   byrow = T)
p_orange <- kronecker(p_orange, rep(1, times=20))
p_orange_vs <- p_orange %*% c(base_v, plus_v)  
p_orange_ys <- unlist(lapply(p_orange_vs, sim_RTs, t0=t0, b=b, a=a, s=s, n = 1))

p_even <- matrix(c(1, 0,
                   1, 1,
                   1, 0,
                   1, 1,
                   1, 0,
                   1, 1, 
                   1, 0,
                   1, 1),
                 nrow = 8, ncol = 2,
                 byrow = T)
p_even <- kronecker(p_even, rep(1, times=20))
p_even_vs <- p_even %*% c(base_v, plus_v)  
p_even_ys <- unlist(lapply(p_even_vs, sim_RTs, t0=t0, b=b, a=a, s=s, n=1))


##### form dataframe
dat4plt <- data.frame(y = c(p_purple_ys, p_orange_ys, p_even_ys),
                      sim = rep(c("p_high", "p_low", "p_even"), each = n_trials),
                      cond = rep(c("hvl", "hvh", "lvl", "lvh"), 
                                     each=40, times=3))
# section p5
prt <- dat4plt %>% group_by(sim) %>% summarise(p5 = quantile(y, .5))
dat4plt <- inner_join(dat4plt, prt, by="sim")
dat4plt <- dat4plt %>% mutate(p5_split = if_else(y < p5, 1, 2))

## now plot the data
# as before
pa <- dat4plt %>% ggplot(aes(x = cond, y = y, group = p5_split, colour = p5_split)) +
          geom_point(position = position_jitter(w = .05, h = .01)) +
          facet_wrap(~sim)
ggsave(paste("../images", "sim_peither.pdf", sep = "/"), pa, units = "cm", width = 16, height = 16)
# as ecdf
pb <- dat4plt %>% ggplot(aes(x = y, group = cond, colour = cond)) +
        stat_ecdf(pad = FALSE) +
         facet_wrap(~sim)
ggsave(paste("../images", "sim_peither_ecdf.pdf", sep = "/"), pb, units = "cm", width = 16, height = 16)




