
# Functions called by analysis_RRR.R


#################### FN: PLOT FOR ONE LEVEL OF HETEROGENEITY ####################

# expected global vars: res.all, colors
all_plots_one_dist = function(true.effect.dist, 
                              bias.min = -0.05, 
                              bias.max = 0.05,
                              mse.max = 0.05,
                              cover.min = 0,
                              results.path = "~/Desktop"){
  
  setwd(results.path)
  
  # take the relevant subset
  temp = res.all[ res.all$true.effect.dist == true.effect.dist, ]
  
  # check that the bounds are reasonable
  if ( min(temp$phatBias, na.rm = TRUE) <= bias.min ) warning( paste("bias.min is above the min observed, which was", min(temp$phatBias, na.rm = TRUE)) )
  if ( max(temp$phatBias, na.rm = TRUE) >= bias.max ) warning( paste("bias.max is below the max observed, which was", max(temp$phatBias, na.rm = TRUE)) )
  if ( max(temp$RMSE, na.rm = TRUE) >= mse.max ) warning( paste("mse.max is below the max observed, which was", max(temp$RMSE, na.rm = TRUE)) )
  if ( min(temp$Cover, na.rm = TRUE) <= cover.min ) warning( paste("cover.min is above the min observed, which was", max(temp$Cover, na.rm = TRUE)) )
  
  
  ##### Coverage #####
  #browser()
  limits = c(cover.min, 1)
  breaks = seq( min(limits), max(limits), 0.1)
  string = bquote( "Panel A:" ~ tau^2 ~ "= 0.25")
  p1 = plot_group( .level = .25,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "Cover",
                   .limits= limits,
                   .breaks = breaks,
                   .data = temp
  )
  
  string = bquote( "Panel B:" ~ tau^2 ~ "= 0.04")
  p2 = plot_group( .level = 0.04,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .limits= limits,
                   .breaks = breaks,
                   .y.name = "Cover",
                   .data = temp )
  
  string = bquote( "Panel C:" ~ tau^2 ~ "= 0.01" )
  p3 = plot_group( .level = 0.01,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .limits= limits,
                   .breaks = breaks,
                   .y.name = "Cover",
                   .data = temp )
  
  library(gridExtra)
  plots = grid.arrange(p1, p2, p3, nrow=3)
  ggsave( filename = paste(true.effect.dist, "_coverage", ".png", sep=""),
          plot = plots, path=NULL, width=12, height=14, units="in")
  
  
  ##### CI Width for Each Level of Tau^2 #####
  string = bquote( "Panel A:" ~ tau^2 ~ "= 0.25" )
  p1 = plot_group( .level = .25,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "Width",
                   .ylab = "CI width",
                   .limits = c(0,1),
                   .breaks = seq(0,1,.2),
                   .data = temp)
  
  string = bquote( "Panel B:" ~ tau^2 ~ "= 0.04" )
  p2 = plot_group( .level = 0.04,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "Width",
                   .ylab = "CI width",
                   .limits = c(0,1),
                   .breaks = seq(0,1,.2),
                   .data = temp)
  
  string = bquote( "Panel C:" ~ tau^2 ~ "= 0.01" )
  p3 = plot_group( .level = 0.01,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "Width",
                   .ylab = "CI width",
                   .limits = c(0,1),
                   .breaks = seq(0,1,.2),
                   .data = temp)
  
  library(gridExtra)
  plots = grid.arrange(p1, p2, p3, nrow=3)
  ggsave( filename = paste(true.effect.dist, "_width", ".png", sep=""),
          plot = plots, path=NULL, width=12, height=14, units="in")
  
  
  ##### Phat Bias for Each Level of Tau^2 #####
  
  string = bquote( "Panel A:" ~ tau^2 ~ "= 0.25" )
  p1 = plot_group( .level = .25,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "phatBias",
                   .ylab = "Bias",
                   .limits = c(bias.min, bias.max),
                   .breaks = seq(bias.min, bias.max, .05),
                   .data = temp)
  
  string = bquote( "Panel B:" ~ tau^2 ~ "= 0.04" )
  p2 = plot_group( .level = 0.04,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "phatBias",
                   .ylab = "Bias",
                   .limits = c(bias.min, bias.max),
                   .breaks = seq(bias.min, bias.max, .05),
                   .data = temp)
  
  string = bquote( "Panel C:" ~ tau^2 ~ "= 0.01" )
  p3 = plot_group( .level = .01,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "phatBias",
                   .ylab = "Bias",
                   .limits = c(bias.min, bias.max),
                   .breaks = seq(bias.min, bias.max, .05),
                   .data = temp)
  
  plots = grid.arrange(p1, p2, p3, nrow=3)
  ggsave( filename = paste(true.effect.dist, "_bias", ".png", sep=""),
          plot = plots, path=NULL, width=12, height=14, units="in")
  

  ##### Phat MSE for Each Level of Tau^2 #####
  string = bquote( "Panel A:" ~ tau^2 ~ "= 0.25" )
  p1 = plot_group( .level = .25,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "RMSE",
                   .ylab = "RMSE",
                   .limits = c(0, mse.max),
                   .breaks = seq(0, mse.max, .1),
                   .data = temp)
  
  string = bquote( "Panel B:" ~ tau^2 ~ "= 0.04" )
  p2 = plot_group( .level = 0.04,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "RMSE",
                   .ylab = "RMSE",
                   .limits = c(0,mse.max),
                   .breaks = seq(0, mse.max, .1),
                   .data = temp)
  
  string = bquote( "Panel C:" ~ tau^2 ~ "= 0.01" )
  p3 = plot_group( .level = 0.01,
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "RMSE",
                   .ylab = "RMSE",
                   .limits = c(0,mse.max),
                   .breaks = seq(0, mse.max, .1),
                   .data = temp)
  
  plots = grid.arrange(p1, p2, p3, nrow=3)
  ggsave( filename = paste(true.effect.dist, "_rmse", ".png", sep=""),
          plot = plots, path=NULL, width=12, height=14, units="in")
  
}

#################### FN: PLOT FOR ONE LEVEL OF HETEROGENEITY ####################

# plot one level of heterogeneity  
# .data: res.all or some subset of it (e.g., for one distribution)
plot_group = function( .level,
                       .title,
                       .ylab = "Coverage",
                       .include.logit = FALSE,
                       .legend = TRUE,
                       .y.name,
                       .limits = c(0.7, 1), 
                       .breaks = seq(0,1,.1),
                       .data ) {
  
  if ( .include.logit == TRUE ) {
    temp = .data[ .data$V == .level, ] 
  } else {
    temp = .data[ .data$V == .level &
                      .data$Method != "Logit", ]
  }
  
  # use appropriate "pretty" name depending on the y-variable
  if ( .ylab %in% c("Coverage", "CI width") ) {
    temp$Method = temp$Method.inf.pretty
    colors = c("orange", "black", "red", "blue")
  } else {  # if y-variable is about point estimates
    temp$Method = temp$Method.est.pretty
    temp = temp[ temp$Method != "Boot", ]
    colors = c("orange", "red", "blue")
  }
  
  p = 
    ggplot( temp, aes_string( x="k",
                              y=.y.name,
                              color="Method" ) ) +
    #ggplot( temp, aes_string( x="k", y=.y.name, color="Method.pretty", alpha = "prop.finished" ) ) +
    geom_line(lwd=1) +
    geom_point(size=2) +
    theme_bw() +
    scale_color_manual(values=colors) +
    scale_alpha_continuous( limits = c(0,1) ) +
    scale_y_continuous( limits=.limits, breaks=.breaks ) +
    ylab(.ylab) +
    ggtitle( .title ) +
    facet_grid( muN.pretty ~ TheoryP.pretty )
  #facet_grid( TheoryP.pretty ~ muN.pretty )
  
  if ( .ylab == "Coverage" ) p = p + geom_hline(yintercept = 0.95, linetype=2)
  if ( .ylab == "Bias" ) p = p + geom_hline(yintercept = 0, linetype=2)
  
  if ( .legend == TRUE ) {
    return(p)
  } else {
    return(p + theme(legend.position="none"))
  }
  
}  
