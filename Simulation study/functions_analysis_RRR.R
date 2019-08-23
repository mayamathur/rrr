
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
  if ( max(temp$phatMSE, na.rm = TRUE) >= mse.max ) warning( paste("mse.max is below the max observed, which was", max(temp$phatMSE, na.rm = TRUE)) )
  if ( min(temp$Cover, na.rm = TRUE) <= cover.min ) warning( paste("cover.min is above the min observed, which was", max(temp$Cover, na.rm = TRUE)) )
  
  
  ##### Coverage #####
  limits = c(cover.min, 1)
  breaks = seq( min(limits), max(limits), 0.1)
  string = bquote( "Panel A:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[1]) )
  p1 = plot_group( .level = unique( temp$V )[1],
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "Cover",
                   .limits= limits,
                   .breaks = breaks,
                   .data = temp
  )
  
  string = bquote( "Panel B:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[2]) )
  p2 = plot_group( .level = unique( temp$V )[2],
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .limits= limits,
                   .breaks = breaks,
                   .y.name = "Cover",
                   .data = temp )
  
  string = bquote( "Panel C:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[3]) )
  p3 = plot_group( .level = unique( temp$V )[3],
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .limits= limits,
                   .breaks = breaks,
                   .y.name = "Cover",
                   .data = temp )
  
  library(gridExtra)
  plots = grid.arrange(p1, p2, p3, nrow=3)
  ggsave( filename = paste("coverage_", true.effect.dist, ".png", sep=""),
          plot = plots, path=NULL, width=12, height=14, units="in")
  
  
  ##### CI Width for Each Level of Tau^2 #####
  string = bquote( "Panel A:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[1]) )
  p1 = plot_group( .level = unique( temp$V )[1],
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "Width",
                   .ylab = "CI width",
                   .limits = c(0,1),
                   .breaks = seq(0,1,.2),
                   .data = temp)
  
  string = bquote( "Panel B:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[3]) )
  p2 = plot_group( .level = unique( temp$V )[3],
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "Width",
                   .ylab = "CI width",
                   .limits = c(0,1),
                   .breaks = seq(0,1,.2),
                   .data = temp)
  
  string = bquote( "Panel C:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[2]) )
  p3 = plot_group( .level = unique( temp$V )[2],
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
  ggsave( filename = paste("width_", true.effect.dist, ".png", sep=""),
          plot = plots, path=NULL, width=12, height=14, units="in")
  
  
  ##### Phat Bias for Each Level of Tau^2 #####
  
  string = bquote( "Panel A:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[1]) )
  p1 = plot_group( .level = unique( temp$V )[1],
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "phatBias",
                   .ylab = "Bias",
                   .limits = c(bias.min, bias.max),
                   .breaks = seq(bias.min, bias.max, .1),
                   .data = temp)
  
  string = bquote( "Panel B:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[2]) )
  p2 = plot_group( .level = unique( temp$V )[2],
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "phatBias",
                   .ylab = "Bias",
                   .limits = c(bias.min, bias.max),
                   .breaks = seq(bias.min, bias.max, .1),
                   .data = temp)
  
  string = bquote( "Panel C:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[3]) )
  p3 = plot_group( .level = unique( temp$V )[3],
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "phatBias",
                   .ylab = "Bias",
                   .limits = c(bias.min, bias.max),
                   .breaks = seq(bias.min, bias.max, .1),
                   .data = temp)
  
  plots = grid.arrange(p1, p2, p3, nrow=3)
  ggsave( filename = paste("bias_", true.effect.dist, ".png", sep=""),
          plot = plots, path=NULL, width=12, height=14, units="in")
  

  ##### Phat MSE for Each Level of Tau^2 #####
  string = bquote( "Panel A:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[1]) )
  p1 = plot_group( .level = unique( temp$V )[1],
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "phatMSE",
                   .ylab = "MSE",
                   .limits = c(0, mse.max),
                   .breaks = seq(0, mse.max, .01),
                   .data = temp)
  
  string = bquote( "Panel B:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[2]) )
  p2 = plot_group( .level = unique( temp$V )[2],
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "phatMSE",
                   .ylab = "MSE",
                   .limits = c(0,mse.max),
                   .breaks = seq(0, mse.max, .01),
                   .data = temp)
  
  string = bquote( "Panel C:" ~ tau^2 ~ "=" ~ .(unique( temp$V )[3]) )
  p3 = plot_group( .level = unique( temp$V )[3],
                   .title = string,
                   .legend = TRUE, 
                   .include.logit = FALSE,
                   .y.name = "phatMSE",
                   .ylab = "MSE",
                   .limits = c(0,mse.max),
                   .breaks = seq(0, mse.max, .01),
                   .data = temp)
  
  plots = grid.arrange(p1, p2, p3, nrow=3)
  ggsave( filename = paste("mse_", true.effect.dist, ".png", sep=""),
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
    geom_hline(yintercept = 0.95, linetype=2) +
    scale_y_continuous( limits=.limits, breaks=.breaks ) +
    ylab(.ylab) +
    ggtitle( .title ) +
    facet_grid( muN.pretty ~ TheoryP.pretty )
  #facet_grid( TheoryP.pretty ~ muN.pretty )
  
  if ( .ylab == "Bias" ) p = p + geom_hline(yintercept = 0, linetype=2)
  
  if ( .legend == TRUE ) {
    return(p)
  } else {
    return(p + theme(legend.position="none"))
  }
  
}  
