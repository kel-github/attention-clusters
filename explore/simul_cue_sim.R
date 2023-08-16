# simulate idea
set.seed(42)
library(tidyverse)
# step 1 = plot additive data from p = .8 condition
n_trials <- 160
beta_cert <- .9
beta_val <- c(0, 0, 0, .2)
error <- rnorm(n_trials, mean = 0, sd = .1)

design <- matrix(c(1, 0, 0, 0, 0, 1, 0, 0,
                   0, 0, 1, 0, 0, 0, 0, 1), nrow = 4, ncol = 4)
design <- kronecker(design, rep(1, times=40))
y <- beta_cert + design %*% beta_val + error

### now make dataframe
dat <- data.frame(sub = 1,
                  reward_type = rep(c("lvl", "hvh", "hvl", "lvh"), each = 40),
                  rt = y)
prt <- dat %>% group_by(sub) %>% summarise(p5 = quantile(rt, .5))
dat$p5 <- prt$p5
dat <- dat %>% mutate(p5_split = if_else(rt < p5, 1, 2))

## now plot the data
# as before
dat %>% ggplot(aes(x = reward_type, y = rt, group = p5_split, colour = p5_split)) +
            geom_point(position = position_jitter(w = .05, h = .01)) + 
            ggtitle("cert = .8") +
            theme(legend.position = "none") 
# as ecdf
dat %>% ggplot(aes(x = rt, group = reward_type, colour = reward_type)) +
               stat_ecdf(pad = FALSE)
# so far confirms as I thought

##### step 2, bimodal distribution
# here I assume that only in the last group is there a probability
# of selecting from 1 of 2 distributions, either the distribution
# that forms the remaining three conditions or, the value
# based distraction
nu_beta_val <- 1.25 # value effect must be > valid effect to work
lvh_cond <- sample(c(beta_cert, nu_beta_val), size = 40, prob = c(.8, .2), 
                   replace = TRUE)

y <- c(rep(beta_cert, each = 120), lvh_cond) + error
dat <- data.frame(sub = 1,
                  reward_type = rep(c("lvl", "hvh", "hvl", "lvh"), each = 40),
                  rt = y)
prt <- dat %>% group_by(sub) %>% summarise(p5 = quantile(rt, .5))
dat$p5 <- prt$p5
dat <- dat %>% mutate(p5_split = if_else(rt < p5, 1, 2))

## now plot the data
# as before
dat %>% ggplot(aes(x = reward_type, y = rt, group = p5_split, colour = p5_split)) +
  geom_point(position = position_jitter(w = .05, h = .01)) + 
  ggtitle("cert = .8") +
  theme(legend.position = "none") 
# as ecdf
dat %>% ggplot(aes(x = rt, group = reward_type, colour = reward_type)) +
  stat_ecdf(pad = FALSE)

# checks out again!
