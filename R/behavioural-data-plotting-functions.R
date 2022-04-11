## written by K. Garner, 2020
## functions to plot the behavioural data from MC WP1 - sequence comparisons

line.plot.by.sub.sess.TR <- function(data, dv, iv, grp, ylims, cols, facet_form = "sub~TR*sess"){
  # use this function to make a 9 panel plot 
  # x-axis = probability|value, y-axis = dv (RT|acc|inv_eff)
  # grp = ~x-axis, sess x TR panels
  # Inputs:
  #    data = data from 1 subject
  #    dv = string for variable on y axis
  #    iv = string for variable on x axis
  #    grp = factor to colour by
  #    ylims = range for y-axis
  #    cols = colour map
  #    facet_form = string: equation for facet_wrap
      ggplot(data, aes_string(x=iv, y=dv, col=grp)) +
      geom_line(aes_string(group=grp), size=1.1) + geom_point() +
      facet_wrap(eval(as.formula(facet_form)), labeller=label_both, ncol=3) +  
      scale_fill_manual(values=cols) +
      scale_color_manual(values=cols) + 
      ylab(dv) + xlab(iv) + ylim(ylims) +
      theme(panel.border = element_blank(), 
            panel.grid.major =   element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
}



sub.plot <- function(data, dv, iv, grp, cols){
  # plot behavioural data by subject, certainty on the x-axis
  # key args
  # --data: the data frame to use
  # --dv: which variable to plot?
  # --iv: what's going on the x axis?
  # --grp: group lines by which factor?
  # --cols: colour scheme
  ggplot(data, aes_string(x=iv, y=dv, col=grp)) +
    geom_line(aes_string(group=grp), size=1.1) + geom_point() +
    facet_wrap(~sub, nrow=1) +  
    scale_fill_manual(values=cols) +
    scale_color_manual(values=cols) + 
    ylab(dv) + xlab(iv) + 
    theme(panel.border = element_blank(), 
          panel.grid.major =   element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))
}

