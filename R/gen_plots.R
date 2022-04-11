###################################################################
## produce and save each plot for the attention clusters experiment
###################################################################
rm(list=ls())

# set environment and any general variables
library(tidyverse)
library(cowplot)
library(wesanderson)
library(cluster)
library(fpc)
library(factoextra)
library(RColorBrewer)
source("behavioural-data-analysis-functions.R")
source("behavioural-data-plotting-functions.R")
source("data-wrangles.R")
source("grp-lvl-analysis.R")
source("grp-lvl-mxd-fx.R")
source("reliability-analysis.R")
source("cluster-funcs.R")
source("clust_func_FU.R")
source("plot_att_clust.R")
source("fig_label.R")

data_path <- '~/Insync/tmp-data/clusters/derivatives/'
cm2in <- .39
# colours taken from colorbrewer: https://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
cert_cols <- c('#1b9e77', '#d95f02')
val_cols <- c(cert_cols, '#7570b3')
data_col <- brewer.pal(n = 8, name = "Dark2")[8]

# behavioural data
plot_colours <- brewer.pal(n = 8, name = "Dark2")[c(4, 5, 6, 8)]
plot_colours <- lapply(plot_colours, function(x) col2rgb(x))
plot_colours <- sapply(plot_colours, function(x) rgb(x[1], x[2], x[3], alpha = 255*.7, maxColorValue = 255))

###################################################################
## theory plot
###################################################################

cert_reg <- generate_spatial_regressors(ps = c(.2, .5, .8), max_val = c(.8, .5, .8))
names(cert_reg)[1] = "cert"
cert_reg$cert <- as.numeric(c(.2, .5, .8))
cp <- plot_cert_theory(cert_reg)

value <- data.frame(cue_a = c(50, 50, 5, 5),
                    cue_b = c(5, 50, 5, 50),
                    max_val = c(50, 50, 5, 50))
val_reg <- generate_value_regressors_4_plotting(value)
vp <- plot_val_theory(val_reg, val_cols)


w <- 7*cm2in
h <- 14*cm2in
pdf(paste("../images/", "mot", "_", "pred", ".pdf", sep = ""),
    width = w, height = h)
# start here
# pdf(paste("~/Insync/documents/talk-images/", "mot_rv_cf", "_", "pred", ".pdf", sep = ""),
#     width = w, height = h)
par(mfrow = c(2, 1), mar = c(3, 3, 1, 1),
    oma = c(1, 2, 1, 1),
    mgp = c(2, 1, 0), las = 0, xpd = TRUE,
    cex.axis = .8, cex.lab = .8)

plot_cert_theory(cert_reg, cert_cols)
fig_label("A", cex = 1.25)
plot_val_theory(val_reg, val_cols)
fig_label("B", cex = 1.25)
dev.off()

###################################################################
## behavioural results (group level)
###################################################################
load(paste(data_path, "cleaned_data.Rmd", sep=""))
w <- 12*cm2in
h <- 6*cm2in
pdf(paste("../images/", "grp-lvl-behav", ".pdf", sep = ""),
    width = w, height = h)
par(mfrow = c(1, 2), mar = c(3, 3, 1, 1),
    oma = c(1, 2, 1, 1),
    mgp = c(2, 1, 0), las = 0, 
    cex.axis = .8, cex.lab = .8)

plot_behav(raw, "acc", FALSE, plot_colours = plot_colours)
fig_label("A", cex = 1.25)
plot_behav(clean_dat, "rt", TRUE, plot_colours = plot_colours)
fig_label("B", cex = 1.25)
dev.off()

###################################################################
## effect of spatial certainty and value with model fits
###################################################################
# cert_cols
# val_cols
load(file = paste(data_path, "grp-level-model-pred.Rmd", sep=""))
w <- 12*cm2in
h <- 6*cm2in

pdf(paste("../images/", "grp-lvl-model-fits", ".pdf", sep = ""),
    width = w, height = h)
par(mfrow = c(1, 2), mar = c(3, 3, 1, 1),
    oma = c(1, 2, 1, 1),
    mgp = c(2, 1, 0), las = 0,
    xpd = TRUE,
    cex.axis = .8, cex.lab = .8)

plot_me_cert_pred(cert_grp_reg_dat, c(.45, .7), c(.5, .71), model_cols = cert_cols, data_col = data_col)
fig_label("A", cex = 1.25)
plot_me_val_pred(val_grp_reg_dat, c(.5, .65), c(-10, -10), val_cols = val_cols, data_col = data_col)
legend(.5, .66, legend = c("rv", "cf", "m"),
       col = val_cols, lty = 1, bty = "n", cex = .8)
fig_label("B", cex = 1.25)
dev.off()


###################################################################
## cluster validation
###################################################################
load(file=paste(data_path, "cluster_solutions.Rmd", sep=""))
w <- 10*cm2in
h <- 10*cm2in
# pdf(paste("../images/", "k-means-sanity", ".pdf", sep = ""),
#     width = w, height = h)
pdf(paste("../images/", "clust_outcomes", ".pdf", sep = ""),
    width = w, height = h)
par(mfrow = c(2, 2), mar = c(3, 3, 1, 1),
    oma = c(1, 2, 1, 1),
    mgp = c(2, 1, 0), las = 0,
    xpd = FALSE,
    cex.axis = .6, cex.lab = .6)

prod_elbow_prnt(clusters_sc)
fig_label("A", cex = 1)
do_reach_plt(optics_res)
fig_label("B", cex = 1)
dp_pca_plot(clust_dat_sc, clusters_sc[[2]]$cluster, OR = FALSE)
fig_label("C", cex = 1)
dp_pca_plot(clust_dat_sc, optics_res)
fig_label("D", cex = 1)

dev.off()

# prod_elbow_prnt(clusters_sc)
# fig_label("A", cex = 2)
# plot_mu_sil_wdths(clusters_sc, clust_dat_sc)
# fig_label("B", cex = 2)
# plot_silhouette_prnt(clusters_sc, 2, clust_dat_sc)
# fig_label("C", cex = 2)
# dev.off()

# # now do the cluster solution/data points, using ggplot functions
# w <- 10*cm2in
# h <- 10*cm2in
# cp <- plot_clust_com(clusters_sc, 2, clust_dat_sc)
# ggsave("../images/clust_sol_PCA.pdf", plot = cp, width = w, height = h, units = "in")

###################################################################
## group behavioural results and modelling of each main effect
###################################################################
load(paste(data_path, "cleaned_data.Rmd", sep=""))
load(file=paste(data_path, "cluster_solutions.Rmd", sep=""))

w <- 10*cm2in
h <- 10*cm2in
pdf(paste("../images/", "clust-grp-behav_rt-acc", ".pdf", sep = ""),
    width = w, height = h)
# first get the clean_dat into a format ready for plotting
# by previously defined plot_behav function
par(mfrow = c(2, 2), mar = c(3, 3, 1, 1),
    oma = c(1, 2, 1, 1),
    mgp = c(2, 1, 0), las = 0,
    xpd = FALSE,
    cex.axis = .6, cex.lab = .6)

nu_clean_dat <- inner_join(clean_dat, grp_sum_dat %>% select(sub, group), by = "sub")
plot_behav(nu_clean_dat %>% filter(group == "1"), 'rt', FALSE, yl = c(.45, .9), plot_colours = plot_colours)
fig_label("A", cex = 1.25)
plot_behav(nu_clean_dat %>% filter(group == "0"), 'rt', FALSE, yl = c(.45, 1), plot_colours = plot_colours)
fig_label("B", cex = 1.25)

nu_raw_dat <- inner_join(raw, grp_sum_dat %>% select(sub, group), by = "sub")
plot_behav(nu_raw_dat %>% filter(group == "1"), 'acc', FALSE, yl = c(.5, 1), plot_colours = plot_colours)
fig_label("C", cex = 1.25)
plot_behav(nu_raw_dat %>% filter(group == "0"), 'acc', FALSE, yl = c(.5, 1), plot_colours = plot_colours)
fig_label("D", cex = 1.25)
dev.off()

### now plot modelling results!
load(file=paste(data_path, "modeldata_win_cluster_grps.Rmd", sep=""))
w <- 10*cm2in
h <- 10*cm2in


pdf(paste("../images/", "clust-grp-behav_model-fits", ".pdf", sep = ""),
    width = w, height = h)
# first get the clean_dat into a format ready for plotting
# by previously defined plot_behav function
par(mfrow = c(2, 2), mar = c(3, 3, 1, 1),
    oma = c(1, 2, 1, 1),
    mgp = c(2, 1, 0), las = 0,
    xpd = FALSE,
    cex.axis = .6, cex.lab = .6)

# now do the main effect of spatial certainty for each group
names(cert_grpA_reg_dat)[names(cert_grpA_reg_dat) == "mu"] = "rt"
plot_me_cert_pred(cert_grpA_reg_dat, c(.45, .85), c(.5, .85), model_cols = cert_cols, data_col = data_col)
fig_label("A", cex = 1.25)
names(cert_grpB_reg_dat)[names(cert_grpB_reg_dat) == "mu"] = "rt"
plot_me_cert_pred(cert_grpB_reg_dat, c(.45, 1), c(10, 1), model_cols = cert_cols, data_col = data_col)
fig_label("B", cex = 1.25)
# and now value, with the additional motivation pred for grb B
names(val_grpA_reg_dat)[names(val_grpA_reg_dat) == "mu"] = "rt"
plot_me_val_pred(val_grpA_reg_dat, c(.45, .65), c(.5, 10), val_cols = val_cols, data_col = data_col) 
legend(.5, .85, legend = c("rv", "cf", "m"),
       col = val_cols, lty = 1, bty = "n", cex = .8)
fig_label("C", cex = 1.25)
names(val_grpB_reg_dat)[names(val_grpB_reg_dat) == "mu"] = "rt"
plot_me_val_pred(val_grpB_reg_dat, c(.5, 1), c(5, .75), val_cols = val_cols, data_col = data_col)
fig_label("D", cex = 1.25)
dev.off()

###################################################################
## individual subject plots for modelling each main effect
###################################################################
plot_subs <- function(x, data){
  yl <- c(min(data$rt[data$sub == x] - .02), max(data$rt[data$sub == x] + .02))
  plot_me_val_pred(data %>% filter(sub == x), yl, c(.5, 10), val_cols = val_cols, data_col = data_col)
  title(main = paste("sub", x, sep = " "))
}

plot_subs_cert <- function(x, data){
  yl <- c(min(data$rt[data$sub == x] - .02), max(data$rt[data$sub == x] + .02))
  plot_me_cert_pred(data %>% filter(sub == x), yl, c(.5, 10), model_cols = cert_cols, data_col = data_col)
  title(main = paste("sub", x, sep = " "))
}

w <- 10*cm2in
h <- 15*cm2in
pdf(paste("../images/", "grp_cert_fits", ".pdf", sep = ""),
    width = w, height = h)
lapply(unique(cert_grp_reg_dat$sub), plot_subs_cert, data = cert_grp_reg_dat)
dev.off()


w <- 10*cm2in
h <- 15*cm2in
pdf(paste("../images/", "val_fits_A", ".pdf", sep = ""),
    width = w, height = h)
lapply(unique(val_grpA_reg_dat$sub), plot_subs, data = val_grpA_reg_dat)
dev.off()

pdf(paste("../images/", "val_fits_B", ".pdf", sep = ""),
    width = w, height = h)
lapply(unique(val_grpB_reg_dat$sub), plot_subs, data = val_grpB_reg_dat)
dev.off()

pdf(paste("../images/", "cert_fits_A", ".pdf", sep = ""),
    width = w, height = h)
lapply(unique(cert_grpA_reg_dat$sub), plot_subs_cert, data = cert_grpA_reg_dat)
dev.off()

pdf(paste("../images/", "cert_fits_B", ".pdf", sep = ""),
    width = w, height = h)
lapply(unique(cert_grpB_reg_dat$sub), plot_subs_cert, data = cert_grpB_reg_dat)
dev.off()

###################################################################
## individual subject plots for examples
###################################################################

plot_val_ind_4_print <- function(s){
  w <- 10*cm2in
  h <- 15*cm2in
  pdf(paste("../images/", "val_fits_s", s, ".pdf", sep = ""),
      width = w, height = h)
  yl <- with(val_grpA_reg_dat %>% filter(sub == s), c(min(rt)-.02, max(rt)+.02))
  plot_me_val_pred(val_grpA_reg_dat %>% filter(sub == s),
                   yl, c(.5, 10),
                   val_cols = val_cols,
                   data_col = data_col)
  title(main = paste("sub", s, sep = " "))
  dev.off()
}

ss <- c(113, 140, 78, 48)
lapply(ss, plot_val_ind_4_print)
