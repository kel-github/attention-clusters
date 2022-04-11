# steps as outlined in Jess' paper
# standardise features (not in Jess' paper)
# set max number of clusters to 10
# allocate each participant to closest euclidean distance to centroid centre
# centroids calculated as mean score profile per cluster
# run the analysis 10,000 times, each time using a different random initialisation of the centroid matrix
# optimal number of clusers based on largest relative decrease in the sum of squared euclidean distances and automated scree test

# also using: https://rich-d-wilkinson.github.io/MATH3030/9-1-k-means-clustering.html

data_in2_cluster_format <- function(clean_dat, dv){
  # put data in the correct structure for clustering
  # kwargs:
  # -- sum_dat - data frame of summary scores per participant
  # -- dv - mu or inv_eff
    # first get accuracy for even and odd trials
   clean_dat %>% group_by(sub, cert, reward_type) %>%
                 summarise(mu = median(rt),
                           fq = quantile(rt, .25),
                           sq = quantile(rt, .75)) %>% 
                 pivot_wider(id_cols = sub, names_from = c("cert", "reward_type"),
                           values_from = c("mu", "fq", "sq"))
}

data_in2_cluster_4time <- function(clean_dat){
  # using clean dat, put into blocks of 80 trials
  # then 
  blocks <- rep(1:2, each = 360)
  tmp <- clean_dat %>% group_by(sub) %>% mutate(block = blocks[t]) %>% ungroup()
  tmp$block <- as.factor(tmp$block)
  tmp <- tmp %>% group_by(sub, block, cert, reward_type) %>%
         summarise(mu = median(rt), fq = quantile(rt, .25), sq = quantile(rt, .75)) %>% ungroup() %>%
         pivot_wider(id_cols = sub, names_from = c("block", "reward_type", "cert"), values_from = c("mu", "fq", "sq")) 

  tmp
}

#############################################################################################
## kmeans
#############################################################################################

do_clust_func <- function(clust_dat, n, max_clust = 10){
  # do cluster analysis given the data and n
  # kwargs:
  # -- clust_dat: output to data_in2_cluster_format
  # -- n: number of start points to try
  # -- max_clust: max number of clusters to go up to
  # -- uses: https://rss.onlinelibrary.wiley.com/doi/10.2307/2346830 
  lapply(1:max_clust, function(x) kmeans(clust_dat, centers = x, nstart = n))
}

prod_elbow <- function(cluster_list){
  
  wss <- do.call(cbind, lapply(1:length(cluster_list), function(x) sum(cluster_list[[x]]$withinss)))
  plot(1:length(cluster_list), wss, type = "b", xlab = "nK", ylab = "within SS")
}

plot_silhouette <- function(cluster_list, x, data){
  # get the silhouette index for a given cluster solution
  # cluster_list is the full list of clustering results
  d <- dist(data)
  sil <- silhouette(cluster_list[[x]]$cluster, d)
  sil <- data_frame(cluster = sil[,"cluster"],
                    neighbour = sil[,"neighbor"],
                    sil_width = sil[,"sil_width"])
  # next, sort the dataframe by cluster,
  # then label 1:n, then plot as a bar chart, 
  # colouring by cluster
  sil %>% arrange(desc(cluster)) %>% mutate(n = factor(x=1:length(cluster))) %>%
          ggplot(aes(x=n, y=sil_width, color = as.factor(cluster), group = cluster)) +
          geom_bar(stat = "identity") 
}


cluster_stats <- function(data, cluster_vec){
  # use the fpc package to produce a range of cluster stats for diagnostics of the
  # solution
  # -- data : data fed into the clusering algorithm
  # -- cluster_vec : the grouping vector from the clustering model
  
  d <- dist(data, method = "euclidean")
  cluster.stats(d,  cluster_vec)
}


#############################################################################################
## OPTIC algorithm
#############################################################################################

do_optics_clust <- function(data){
  # perform optic clustering algorithm on data
  # NOTE: parameters were manually tuned and are hard coded!
  
  # -- data: scaled data for clustering - e.g. clust_dat_sc
  d <- dist(data)
  res <- optics(d, minPts = 5)
  extractDBSCAN(res, eps_cl = 1.5)

}

#############################################################################################
## PLOTTING
#############################################################################################
plot_data_clust_grps <- function(sum_dat, sub_list, grp_vector){
  # given the df sum_dat, exclude any relevant participants 
  # and then plot the data by the clustering grouping vector
  # (grp_vector taken from the chosen output of do_clust_func)
  grp <- data_frame(sub_list, group = grp_vector)
  d <- inner_join(sum_dat %>% filter(sub %in% t(sub_list)), grp, by="sub") 
  d %>% group_by(reward_type, cert, group) %>%
                 summarise(murt = mean(mu),
                           N = length(mu),
                           se = sd(mu)/sqrt(N)) %>%
                  ggplot(aes(x = cert, y = murt, 
                             group = reward_type, col = reward_type)) +
                  geom_line() +
                  geom_errorbar(aes(ymin=murt-se, 
                                    ymax=murt+se), width=.2) +
                  scale_color_manual(values=wes_palette("IsleofDogs1")) +
                  scale_fill_manual(values=wes_palette("IsleofDogs1")) +
                  theme_cowplot() + ylab("rt") +
                  facet_wrap(~group) 
}

plot_data_clust_grps_t <- function(sum_dat, sub_list, grp_vector){
  # given the df sum_dat, exclude any relevant participants 
  # and then plot the data by the clustering grouping vector
  # (grp_vector taken from the chosen output of do_clust_func)
  grp <- data_frame(sub_list, group = grp_vector)
  d <- inner_join(sum_dat %>% filter(sub %in% t(sub_list)), grp, by="sub") 
  d %>% group_by(reward_type, cert, group, block) %>%
    summarise(murt = mean(mu),
              N = length(mu),
              se = sd(mu)/sqrt(N)) %>%
    ggplot(aes(x = cert, y = murt, 
               group = reward_type, col = reward_type)) +
    geom_line() +
    geom_errorbar(aes(ymin=murt-se, 
                      ymax=murt+se), width=.2) +
    scale_color_manual(values=wes_palette("IsleofDogs1")) +
    scale_fill_manual(values=wes_palette("IsleofDogs1")) +
    theme_cowplot() + ylab("rt") +
    facet_wrap(~group*block, ncol=2) 
}

dp_pca_plot <- function(data, c){
  # -- data:: clust_dat_sc
  # -- c: grouping vector from clustering algorithm
  pca <- prcomp(as.matrix(data))
  dat <- cbind(as.data.frame(pca$x), c)
  dat %>% ggplot(aes(x = PC1, y = PC2, group = c, colour = c)) +
    geom_point() + 
    scale_fill_manual(labels = c("trackers", "followers"), values = wes_palette("IsleofDogs1")[c(4,2)])
}


# this code was used in the main doc to get data for students:
# sum_dat <- clean_dat %>% group_by(sub, cert, reward_type) %>%
#                          summarise(mu = median(rt), vari = var(rt))
# rt_dat_4_clust <- data_in2_cluster_format(sum_dat, "mu")
# rt_dat_4_clust <- scale(rt_dat_4_clust[,2:ncol(rt_dat_4_clust)])
# var_dat_4_clust <- data_in2_cluster_format(sum_dat, "vari") # standardize the columns (features) 
# var_dat_4_clust <- scale(var_dat_4_clust[,2:ncol(var_dat_4_clust)])
# # write csv for students
# write.csv(cbind(rt_dat_4_clust, var_dat_4_clust[,2:ncol(var_dat_4_clust)]), paste(data_path, "Shelli_cluster_dat.csv", sep=""))
# rt_clust <- do_clust_func(rt_dat_4_clust, n=10000)
# 
# # cluster data for Luke
# rt_dat_4_clust_time <- data_in2_cluster_4time(clean_dat)
# write.csv(rt_dat_4_clust_time, paste(data_path, "Luke_cluster_dat.csv", sep=""))