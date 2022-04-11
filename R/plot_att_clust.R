### set of functions to produce plots for print etc

#############################################################################################
## theory plots
#############################################################################################
plot_cert_theory <- function(cert_reg, cert_cols = NA){
  # takes regressor dataframe made by the function generate_spatial_regressors
  if (is.na(cert_cols)) cert_cols = c(wes_palette("IsleofDogs1")[3], wes_palette("IsleofDogs1")[6])
  names(cert_cols) <- c("sel", "cf")
  with(cert_reg, plot(x=cert, y = sel_hist,
                      type = "o",
                      col = cert_cols["sel"],
                      bty = "n",
                      pch = 20,
                      cex = 1,
                      ylab = expression(italic("Au")),
                      xlab = expression(italic("certainty")),
                      yaxt = "n",
                      xaxt = "n",
                      xlim = c(0, 1),
                      ylim = c(0, 2))
       )
  axis(side = 1, at = c(0, 1), labels = c("0", "1"))
  axis(side = 2, at = c(0, 2), labels = c("",""))
  with(cert_reg, points(x=cert, y=counterf,
                        type = "o", pch = 20,
                        col = cert_cols["cf"]))
  
  legend(0, 1.4, legend = c("sh", "cf"),
         col = cert_cols, pch = 19, bty = "n", cex = .8)
}

generate_value_regressors_4_plotting <- function(value){
  # get the value regressors for the group level modelling
  # kwargs: 
  # -- value - a dataframe containing 
  #     -- cue_a: target value for that trial
  #     -- cue_b: distractor value for that trial
  #     -- max_val: max value available on that trial
  
  value$rv <- with(value, mapply(relval, cue_a, cue_b))
  value <- value %>% mutate(rv = HH(p=rv)*.2)
  
  value$counterf <- with(value, mapply(L, cue_a, cue_b, max_val, MoreArgs = list(gamma=1)))
  value <- value %>% mutate(counterf = HH(p=counterf))
  
  value$mot <- NA
  for (i in 1:4) {
    value$mot[i] <- with(value, motiv(c(.8*.5, .8*.5), c(cue_a[i], cue_b[i])))
  }
  value <- value %>% mutate(mot = HH(p=mot))
  
  
  value <- value %>% select(c(cue_a, cue_b, rv, counterf, mot)) %>% pivot_longer(cols=c(rv, counterf, mot),
                                                                                 names_to = "model",
                                                                                 values_to = "Au")
  
  value$Au[with(value, is.infinite(Au))] = 0
  value$cue <- rep(c("htgt/ldst", "htgt/hdst", "ltgt/ldst", "ltgt/hdst"), each=3)
  value$cue <- as.factor(value$cue)
  value
}


plot_val_theory <- function(val_reg, val_cols = NA){
  # takes regressor dataframe made by the function generate_value_regressors
  
  # some settings
  if (is.na(val_cols)) val_cols <- wes_palette("IsleofDogs1")[c(3, 6, 4)]
  names(val_cols) <- c("rv", "cf", "mot")
  
  names(val_reg)[names(val_reg) == "cue"] <- "incentives"
  
  with(val_reg, plot.default(x = c(1:4), 
                             y=c(Au[model == "mot" & incentives == "htgt/hdst"],
                                 Au[model == "mot" & incentives == "htgt/ldst"],
                                 Au[model == "mot" & incentives == "ltgt/hdst"],
                                 Au[model == "mot" & incentives == "ltgt/ldst"])*3,
                             type = "o",
                             col = val_cols["mot"],
                             bty = "n",
                             pch = 20,
                             cex = 1,
                             ylab = expression(italic("Au")),
                             xlab = expression(italic("incentives (tgt/dst)")),
                             yaxt = "n",
                             xaxt = "n",
                             ylim = c(0, .75),
                             xlim = c(0.5, 4.5)))
  axis(side = 1, at = c(1, 2, 3, 4), labels = c("h/h", "h/l", "l/h", "l/l"))
  axis(side = 2, at = c(0, 2.5), labels = c("",""))
  
  with(val_reg, points(x = c(1:4),
                       y = c(Au[model == "rv" & incentives == "htgt/hdst"],
                             Au[model == "rv" & incentives == "htgt/ldst"],
                             Au[model == "rv" & incentives == "ltgt/hdst"],
                             Au[model == "rv" & incentives == "ltgt/ldst"]),
                       type = "o", pch = 20,
                       col = val_cols["rv"])
  )
  with(val_reg, points(x = c(1:4),
                     y = c(Au[model == "counterf" & incentives == "htgt/hdst"],
                           Au[model == "counterf" & incentives == "htgt/ldst"],
                           Au[model == "counterf" & incentives == "ltgt/hdst"],
                           Au[model == "counterf" & incentives == "ltgt/ldst"]),
                     type = "o", pch = 20,
                     col = val_cols["cf"])
  )
  legend(0.15, 0.7, legend = names(val_cols),
         col = val_cols, pch = 19, bty = "n", cex = .8)

}


#############################################################################################
## plot behaviour
#############################################################################################
plot_behav <- function(data, dv, leg_on, yl = c(), plot_colours = c()){
  # --data: either 'raw' or 'clean_dat'
  # --dv: 'acc' or 'rt'
  # --leg_on: TRUE or FALSE (for if you want a legend)
  if (is_empty(plot_colours)) plot_colours <- wes_palette("IsleofDogs1")[c(1:4)]
  if (dv == "acc") {
    dat <- score_accuracy(data) %>% group_by(reward_type, cert) %>%
               summarise(mu = mean(acc),
                         sd = sd(acc),
                         N = length(acc),
                         se = sd/sqrt(N)) %>% ungroup()
    if (is_empty(yl)) yl <- c(.5, 1)
    ylabel <- "acc"
  }
  if (dv == "rt") {
    dat <- compute_rt_score(data) 
    names(dat)[names(dat) == "mu"] = "rt"
    dat <- dat %>% group_by(reward_type, cert) %>%
                   summarise(mu = mean(rt),
                             sd = sd(rt),
                             N = length(rt),
                             se = sd/sqrt(N)) %>% ungroup()
    if (is_empty(yl)) yl <- c(0.45, 0.75)
    ylabel <- "RT"
  }
  
  # some settings, regardless of DV being plotted
  val_levels <- levels(dat$reward_type)
  val_order <- c(1, 2, 4, 3)
  val_levels <- val_levels[val_order]
  names(plot_colours) <- val_levels
  
  with(dat, plot(x = c(1, 2, 3),
                 y = mu[reward_type == val_levels[1]],
                 bty = 'n',
                 type = 'o',
                 pch = 20,
                 cex = 0.8, 
                 col = plot_colours[val_levels[1]],
                 ylim = yl, 
                 xlim = c(0.5, 4.5),
                 ylab = ylabel,
                 xlab = "sc",
                 xaxt = 'n',
                 yaxt = 'n'))
  axis(side = 1, at = c(1, 2, 3), labels = c(".2", ".5", ".8"))
  if (dv == "acc") axis(side = 2, at = c(.5, 1), labels = c(".5", "1"), las = 2)
  if(dv == "rt") axis(side = 2, at = c(min(yl), max(yl)), labels = c(as.character(c(min(yl), max(yl)))), las = 2)
  
  with(dat, arrows(x0 = c(1, 2, 3), 
                   y0 = mu[reward_type == val_levels[1]] - (1.96*se[reward_type == val_levels[1]]),
                   x1 = c(1, 2, 3), 
                   y1 = mu[reward_type == val_levels[1]] + (1.96*se[reward_type == val_levels[1]]),
                   code = 3,
                   col = plot_colours[val_levels[1]],
                   angle = 90,
                   length = .025))
  
  for (i in 2:length(val_levels)){
    
    with(dat, points(x = c(1, 2, 3),
                   y = mu[reward_type == val_levels[i]],
                   type = 'o',
                   pch = 20,
                   cex = 0.8, 
                   col = plot_colours[val_levels[i]]))
    
    with(dat, arrows(x0 = c(1, 2, 3), 
                     y0 = mu[reward_type == val_levels[i]] - (1.96*se[reward_type == val_levels[i]]),
                     x1 = c(1, 2, 3), 
                     y1 = mu[reward_type == val_levels[i]] + (1.96*se[reward_type == val_levels[i]]),
                     code = 3,
                     col = plot_colours[val_levels[i]],
                     angle = 90,
                     length = .025))   
    
  }
  if (leg_on){
    legend("topright", inset = c(-0.05, 0), legend = c("h/h", "h/l", "l/h", "l/l"),
           col = plot_colours, pch = 20, bty = "n", cex = 0.75) 
  } 
}

#############################################################################################
## plot main effects vs model predictions
#############################################################################################
plot_me_cert_pred <- function(cert_grp_reg_dat, yl, leg_loc, model_cols = c(), data_col = c()){
  # plot the main effect of certainty and the ensuing model predictions
  # against the data
  # -- cert_grp_reg_data -> dataframe generated within the rmd
  # -- yl: ylims c(.45, .7)
  # -- leg_loc: legend coordinates (x, y)

  if (is_empty(model_cols)) model_cols <- c(wes_palette("IsleofDogs1")[c(3,6,4)])
  if (is_empty(data_col)) data_col <- wes_palette("IsleofDogs1")[1]
  
  # certainty plot
  cert_dat <- cert_grp_reg_dat %>% group_by(cert) %>%
                                   summarise(mu = mean(rt),
                                             sd = sd(rt),
                                             N = length(rt),
                                             se = sd/sqrt(N),
                                             sh = mean(pred_sel_hist_rt),
                                             cf = mean(pred_cfact_rt)) %>%
                                    ungroup()
  
  with(cert_dat, plot(x = c(1, 2, 3),
                      y = mu,
                      type = 'p',
                      col = data_col,
                      bty = 'n',
                      pch = 20,
                      cex = .8,
                      xlab = expression(italic('sc')),
                      ylab = expression(italic('RT')),
                      xaxt = 'n',
                      yaxt = 'n',
                      xlim = c(0.5, 3.5),
                      ylim = yl)
       )
  
  axis(side = 1, at = c(1, 2, 3), labels = c(".2", ".5", ".8"))
  yl <- round(yl, 2)
  axis(side = 2, at = c(min(yl), max(yl)), labels = c(as.character(c(min(yl), max(yl)))), las = 2)
  
  with(cert_dat, arrows(x0 = c(1, 2, 3), 
                        y0 = mu - (1.96*se),
                        x1 = c(1, 2, 3), 
                        y1 = mu + (1.96*se),
                        code = 3,
                        col = data_col,
                        angle = 90,
                        length = .025))
  
  cert_models <- c("sh", "cf")
  cidx <- which(names(cert_dat) %in% cert_models)
  names(model_cols) <- cert_models
  for (i in 1:length(cidx)){
    
    with(cert_dat, points(x = c(1, 2, 3),
                          y = t(cert_dat[,cidx[i]]),
                          type = 'l',
                          col = model_cols[ i ],
                          lty = 1)
         )
  }

  legend(leg_loc[1], leg_loc[2], legend = c("sh", "cf"),
         col = model_cols, lty = 1, bty = "n", cex = .8)

}

plot_me_val_pred <- function(val_grp_reg_dat, yl, leg_loc, val_cols = c(), data_col = c()){
  # plot the main effect of certainty and the ensuing model predictions
  # against the data
  # -- val_grp_reg_dat -> created and saved from Rmd
  # -- yl -> ylims
  # -- leg_loc -> where do you want the legend?
  
  if (is_empty(val_cols)) val_cols <- wes_palette("IsleofDogs1")[c(1, 3,6)]
  names(val_cols) <- c("rv", "cf", "mot")
  if (is_empty(data_col)) data_col <- wes_palette("IsleofDogs1")[1]
  
  # plot
  val_dat <- val_grp_reg_dat %>% group_by(reward_type) %>%
    summarise(mu = mean(rt),
              sd = sd(rt),
              N = length(rt),
              se = sd/sqrt(N),
              rv = mean(pred_rv_rt),
              cf = mean(pred_cfact_rt),
              m = mean(pred_mot_rt)) %>%
    ungroup()
  val_dat$mu = val_dat$mu[c(1, 2, 4, 3)] # putting in sensible order for plotting, clunky coding, sorry world
  val_dat$se = val_dat$se[c(1, 2, 4, 3)]
  val_dat$rv = val_dat$rv[c(1, 2, 4, 3)]
  val_dat$cf = val_dat$cf[c(1, 2, 4, 3)]
  val_dat$m = val_dat$m[c(1, 2, 4, 3)]
  
  with(val_dat, plot(x = c(1, 2, 3, 4),
                     y = mu,
                     type = 'p',
                     col = data_col,
                     bty = 'n',
                     pch = 20,
                     cex = .8,
                     xlab = expression(italic('incentives T/D')),
                     ylab = expression(italic('RT')),
                     xaxt = 'n',
                     yaxt = 'n',
                     xlim = c(0.5, 4.5),
                     ylim = yl)
  )
  
  axis(side = 1, at = c(1, 2, 3, 4), labels = c("h/h", "h/l", "l/h", "l/l"))
  yl <- round(yl, 2)
  axis(side = 2, at = c(min(yl), max(yl)), labels = c(as.character(c(min(yl), max(yl)))), las = 2)
  
  with(val_dat, arrows(x0 = c(1, 2, 3, 4),
                       y0 = mu - (1.96*se),
                       x1 = c(1, 2, 3, 4),
                       y1 = mu + (1.96*se),
                       code = 3,
                       col = data_col,
                       angle = 90,
                       length = .025))
  
  val_models <- c("m", "rv", "cf")
  vidx <- which(names(val_dat) %in% val_models)
  names(val_cols) <- val_models
  for (i in 1:length(vidx)){
    
    with(val_dat, points(x = c(1, 2, 3, 4),
                          y = t(val_dat[,vidx[i]]),
                          type = 'l',
                          col = val_cols[ i ],
                          lty = 1)
    )
  }
  
  legend(leg_loc[1], leg_loc[2], legend = val_models,
         col = val_cols, lty = 1, bty = "n", cex = .8)
  
}

#############################################################################################
## plot k-means validations
#############################################################################################
prod_elbow_prnt <- function(cluster_list){
  # proiduce the elbow plot for the summed within ss
  # -- cluster list: output from do_clust_func()
  
  wss <- do.call(cbind, lapply(1:length(cluster_list), function(x) sum(cluster_list[[x]]$withinss)))
  
  plot(1:length(cluster_list), wss, 
       type = "b", 
       xlab = "nK", 
       ylab = expression(italic("Sum within SS")),
       col = wes_palette("IsleofDogs1")[3],
       pch = 19,
       bty = 'n',
       xaxt = 'n',
       yaxt = 'n')
  axis(side = 1, at = c(1, 10))
  axis(side = 2, at = c(200, 500))
}

plot_mu_sil_wdths <- function(cluster_list, data){
  # produce the average silhouette index, across clusters
  # for each solution
  # -- cluster list: output from do_clust_func()
  # -- data: data entered into do_clust_func 
  d <- dist(data)
  get_mu_ss <- function(clusters, d){
    ss <- silhouette(clusters, d)
    mean(ss[,3])
  }
 mu_sil <- do.call(rbind, lapply(2:length(cluster_list), function(x) get_mu_ss(cluster_list[[x]]$cluster, d)))

 plot(c(2:length(cluster_list)), mu_sil, 
      type = "b", 
      xlab = "nK", 
      ylab = expression(italic(paste(mu, " S"))),
      col = wes_palette("IsleofDogs1")[1],
      pch = 19,
      bty = 'n',
      xaxt = 'n',
      yaxt = 'n',
      ylim = c(0,.6),
      xlim = c(1.5, length(cluster_list)+.5))
 axis(side = 1, at = c(2, 10))
 axis(side = 2, at = c(0, .6))  
}

plot_silhouette_prnt <- function(cluster_list, x, data){
  # plot the silhouette groupings, given
  # chosen cluster solution
  # -- cluster list: output from do_clust_func()
  # -- data: data entered into do_clust_func 
  # -- x: cluster solution
  
  d <- dist(data)
  sil <- silhouette(cluster_list[[x]]$cluster, d)
  sil <- data_frame(cluster = sil[,"cluster"],
                    neighbour = sil[,"neighbor"],
                    sil_width = sil[,"sil_width"])
  
  # next, sort the dataframe by cluster,
  # then label 1:n, then plot as a bar chart, 
  # colouring by cluster
  sil <- sil %>% arrange(cluster, sil_width) %>% mutate(n = 1:length(cluster)) 
  with(sil, plot(n, sil_width,
                 bty = 'n',
                 type = 'p',
                 pch = 20,
                 xlab = expression(italic("n")), 
                 ylab = expression(italic("S")),
                 col = c(rep(wes_palette("IsleofDogs1")[2], each = sum(cluster == 1)), 
                         rep(wes_palette("IsleofDogs1")[4], each = sum(cluster == 2))),
                 bty = 'n',
                 xaxt = 'n',
                 yaxt = 'n',
                 ylim = c(-0.2, 1)))
       axis(side = 1, at = c(1, 147))
       axis(side = 2, at = c(-0.2, 1))  
       abline(h = 0, lty = 2, col = "gray48")
       abline(h = 1, lty = 2, col = "gray48")
}

plot_clust_com <- function(clusters, solutionN, cluster_data){
  # plot the first two componentsof data along with the cluster
  # membership for each data point
  # --clusters: list of cluster solutions
  # --solutionN: the N solution you want to plot
  # --cluster_data: the data you used to do the clustering analysis
  fviz_cluster(clusters[[solutionN]], data = cluster_data, ellipse = FALSE,
               main = "") + theme_cowplot() + 
    theme(legend.position = "none") +
    xlab("PC1") + ylab("PC2") +
    ylim(c(-6, 6)) + xlim(c(-5, 5)) +
    scale_color_manual(labels = c("trackers", "followers"), values = wes_palette("IsleofDogs1")[c(2,4)]) +
    theme(legend.position = "bottom",
          text = element_text(size = 12))
}

#############################################################################################
## plot optics validations
#############################################################################################
dp_pca_plot <- function(data, c, OR = TRUE){
  # -- data:: clust_dat_sc
  # -- c: grouping vector from clustering algorithm or 
  # results from optics cluster analysis
  
  # this logical conjunctive I added because I wanted
  # a quick way to also do the pca plot on the k-means results
  # in the case of k-means, c = the clustering vector 
  if (OR){
    c <- c$cluster
    ga = 0
    gb = 1
  } else {
    c <- c
    ga = 2
    gb = 1
  }
  pca <- prcomp(as.matrix(data), retx = TRUE, center = FALSE, scale = FALSE)
  
  # adding code from this function so that I can replicate the results of fviz_cluster
  # https://github.com/kassambara/factoextra/blob/master/R/facto_summarize.R
  # elmt <- get_pca(pca, "ind")
  axes <- c(1,2)
  # dd <- data.frame(elmt$coord[, axes, drop=FALSE], stringsAsFactors = TRUE)
  # coord <- apply(dd^2, 1, sum) # x^2 + y2 + ...
  # ind <- cbind(dd, coord = coord)
  # or
  ind <- facto_summarize(pca, element = "ind", result = "coord", axes = axes)
  colnames(ind)[2:3] <- c("x", "y")
  plot_data <- cbind.data.frame(ind, c = c, stringsAsFactors = TRUE)
  #eig <- get_eigenvalue(pca)[axes, 2]

  c_cols <- rep(NA, 1, length(c))
  c_cols[plot_data$c == ga] = "#d95f02"
  c_cols[plot_data$c == gb] = "#7570b3"
  
  with(plot_data, plot(x=x, y=y,
               bty = 'n',
               type = 'p',
               pch = 20,
               xlab = expression(italic("PC1")), 
               ylab = expression(italic("PC2")),
               col = c_cols,
               cex = 0.6,
               bty = 'n',
               ylim = c(-4, 4),
               xlim = c(-4, 4)))              
}

do_reach_plt <- function(optics_res){
  # given results of clustering analysis from optics algorithm,
  # plot the reachability of the datapoints
  plot(x = 1:length(optics_res$cluster),
       y = optics_res$reachdist[optics_res$order],
       bty = 'n',
       type = 'h',
       xlab = expression(italic("RD")),
       ylab = expression(italic(epsilon)),
       col = c(rep("#7570b3", each = sum(optics_res$cluster == 1)),
               rep("#d95f02", each = sum(optics_res$cluster == 0))),
       xaxt = 'n',
       yaxt = 'n')
  axis(side = 1, at = c(0, 147))
  axis(side = 2, at = c(0, optics_res$eps))
  abline(h = optics_res$eps_cl, lty = 2, col = "gray48")
}


