
# avoid saving old copies of objects
rm(list=ls())

#################### HELPER FNS ####################

# plot one level of heterogeneity  
plot_group = function( .level,
                       .title,
                       .ylab = "Coverage",
                       .include.logit = FALSE,
                       .legend = TRUE,
                       .y.name,
                       .limits = c(0.7, 1), 
                       .breaks = seq(0,1,.1) ) {

  if ( .include.logit == TRUE ) {
    temp = res.all[ res.all$V == .level, ] 
  } else {
    temp = res.all[ res.all$V == .level &
                      res.all$Method != "Logit", ]
  }
  
  p = 
    ggplot( temp, aes_string( x="k",
                              y=.y.name,
                              color="Method",
                              lty = "true.effect.dist" ) ) +
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

library(dplyr)

#################### READ IN DATA ####################


# setwd("~/Dropbox/Personal computer/Independent studies/Meta-analysis metrics (RRR)/Linked to OSF (RRR)/Data/Simulation study results/2018-9-10 add P=0.15")
# s3 = fread("stitched.csv")
# 
# # previous results (P=0.20)
# setwd("~/Dropbox/Personal computer/Independent studies/Meta-analysis metrics (RRR)/Linked to OSF (RRR)/Data/Simulation study results/2018-9-6 add P=0.20")
# s2 = fread("stitched.csv")

# previous results (all others)
setwd("~/Desktop")
#setwd("~/Dropbox/Personal computer/Independent studies/Meta-analysis metrics (RRR)/Linked to OSF (RRR)/Data/Simulation study results/2018-8-28 BCa with 10K iterates")
library(data.table)
s1 = fread("stitched.csv")
s1 = as.data.frame(s1)
s1 = s1[ , names(s1) != "V1" ]

s = s1
dim(s)
#s = rbind(s1, s2, s3)

s = s %>% filter( !is.na(scen.name) & !is.na(Method) )
s = droplevels(s)
table(s$scen.name)

# get rid of messed-up rows (e.g., has something wrong for scen.name)
s = s %>% filter(!is.na(as.numeric(scen.name)) )

# what percent of the reps were NOT NA (e.g., didn't hit an error about infinite w due to few iterates or Fisher convergence)?
prop.finished = s %>% group_by(scen.name) %>%
                summarise( prop.finished = sum( is.na(Note) ) / length(Note) )
s = merge(s, prop.finished)

# all scenarios had <10% missing data :)
min(prop.finished$prop.finished)

# make sure variables are correct type
( make.logical = names(s)[ grepl("Cover", names(s)) ] )
( make.numeric = names(s)[ !names(s) %in% c("Method",
                                          "Note",
                                          "tail",
                                          "true.effect.dist") ] )
# for logicals, first make them logical and then numeric
s = s %>%
  mutate_at(make.logical, as.logical) %>%
  mutate_at(make.numeric, as.numeric) 




# sanity check for data generation: TheoryP and TruthP should be close
# keep close eye on tau^2 = 0.01 scenarios because we 
#  reject samples with tau = 0 estimate
# minimal bias in these scenarios :) 
s %>% group_by(V, TheoryP) %>%
  summarise( TruthP = mean(TruthP) )



#################### DATA WRANGLING ####################

# name the simulation reps
( n.methods = length( unique(s$Method) ) )
s$sim.rep = rep( 1:( nrow(s) / n.methods ), each = n.methods )


# for plotting joy
s$plot.panel = paste( expression(tau^2), " = ", s$V, 
                      "; q = ", s$q,
                      "; true P = ", s$TheoryP,
                      sep = "" )

# # indicate reps in which logit method yielded no CI
# # hence the mean coverage by simulation rep is NA 
# keep.rep = s %>% group_by(sim.rep) %>%
#   summarise( logit.was.NA = is.na( mean(Cover) ) )
# 
# # logit CI fails for 4% of all reps
# prop.table(table(keep.rep$logit.was.NA))
# 
# # merge into main dataset
# s = merge( s, keep.rep, by = "sim.rep")
# prop.table(table(s$logit.was.NA))

# summarize results, leaving NA if ANY reps failed (for logit)
res.all = s %>%
  group_by(scen.name, Method, true.effect.dist) %>%
  mutate(EmpVar = var(phat)) %>%
  summarise_if( is.numeric, function(x) mean(x) )


res.all = res.all[ !is.na(res.all$Method), ]

# MSE of Phat
res.all$phatMSE = res.all$phatBias^2 + res.all$EmpVar

# for plotting joy
res.all$muN.pretty = paste( "E[N] = ", res.all$muN )
res.all$TheoryP.pretty = paste( "True P = ",
                                format(round(res.all$TheoryP, 2), nsmall = 2) )

# also for plotting joy
library(car)
res.all$Method = recode( res.all$Method, " 'Original'='Theory' " )



#################### PLOTS ####################

# for scenarios in which logit has ANY missing data, does not show 
#  that line because would be an unfair comparison to original method,
#  which always works

library(ggplot2)

colors=c("orange", "black", "red", "blue")



##### Coverage Plot for Each Level of Tau^2 #####
limits = c(0, 1)
breaks = seq( min(limits), max(limits), 0.05)
string = bquote( "Panel A:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[1]) )
p1 = plot_group( .level = unique( res.all$V )[1],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .y.name = "Cover",
                 .limits= limits,
                 .breaks = breaks
                 )

string = bquote( "Panel B:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[2]) )
p2 = plot_group( .level = unique( res.all$V )[2],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .limits= limits,
                 .breaks = breaks,
                 .y.name = "Cover" )

string = bquote( "Panel C:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[3]) )
p3 = plot_group( .level = unique( res.all$V )[3],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .limits= limits,
                 .breaks = breaks,
                 .y.name = "Cover" )

library(gridExtra)
cov = grid.arrange(p1, p2, p3, nrow=3)
setwd("~/Desktop")
ggsave( filename = paste("coverage_RRR.png"),
        plot = cov, path=NULL, width=12, height=14, units="in")




##### CI Width for Each Level of Tau^2 #####
string = bquote( "Panel A:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[1]) )
p1 = plot_group( .level = unique( res.all$V )[1],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .y.name = "Width",
                 .ylab = "CI width",
                 .limits = c(0,1),
                 .breaks = seq(0,1,.2))

string = bquote( "Panel B:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[3]) )
p2 = plot_group( .level = unique( res.all$V )[3],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .y.name = "Width",
                 .ylab = "CI width",
                 .limits = c(0,1),
                 .breaks = seq(0,1,.2))

string = bquote( "Panel C:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[2]) )
p3 = plot_group( .level = unique( res.all$V )[2],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .y.name = "Width",
                 .ylab = "CI width",
                 .limits = c(0,1),
                 .breaks = seq(0,1,.2))

library(gridExtra)
plots = grid.arrange(p1, p2, p3, nrow=3)
ggsave( filename = paste("width_RRR.png"),
        plot = plots, path=NULL, width=12, height=14, units="in")


##### Phat Bias for Each Level of Tau^2 #####
string = bquote( "Panel A:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[1]) )
p1 = plot_group( .level = unique( res.all$V )[1],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .y.name = "phatBias",
                 .ylab = "Bias",
                 .limits = c(-0.05,.05),
                 .breaks = seq(-0.05,0.05,.1))

string = bquote( "Panel B:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[2]) )
p2 = plot_group( .level = unique( res.all$V )[2],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .y.name = "phatBias",
                 .ylab = "Bias",
                 .limits = c(-0.05,.05),
                 .breaks = seq(-0.05,0.05,.1))

string = bquote( "Panel C:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[3]) )
p3 = plot_group( .level = unique( res.all$V )[3],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .y.name = "phatBias",
                 .ylab = "Bias",
                 .limits = c(-0.05,.05),
                 .breaks = seq(-0.05,0.05,.1))

library(gridExtra)
plots = grid.arrange(p1, p2, p3, nrow=3)
ggsave( filename = paste("bias_RRR.png"),
        plot = plots, path=NULL, width=12, height=14, units="in")



##### Phat MSE for Each Level of Tau^2 #####

# bm 
string = bquote( "Panel A:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[1]) )
p1 = plot_group( .level = unique( res.all$V )[1],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .y.name = "phatMSE",
                 .ylab = "MSE",
                 .limits = c(0,.05),
                 .breaks = seq(0, 0.05, .01))

string = bquote( "Panel B:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[2]) )
p2 = plot_group( .level = unique( res.all$V )[2],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .y.name = "phatMSE",
                 .ylab = "MSE",
                 .limits = c(0,.05),
                 .breaks = seq(0, 0.05, .01))

string = bquote( "Panel C:" ~ tau^2 ~ "=" ~ .(unique( res.all$V )[3]) )
p3 = plot_group( .level = unique( res.all$V )[3],
                 .title = string,
                 .legend = TRUE, 
                 .include.logit = FALSE,
                 .y.name = "phatMSE",
                 .ylab = "MSE",
                 .limits = c(0,.05),
                 .breaks = seq(0, 0.05, .01))

library(gridExtra)
plots = grid.arrange(p1, p2, p3, nrow=3)
ggsave( filename = paste("MSE_RRR.png"),
        plot = plots, path=NULL, width=12, height=14, units="in")



#################### WINNING METHOD BY SCENARIO ####################

# point estimate winner
# exclude boot since it's not a "real" point estimate
est.winners = res.all %>% filter(Method != "Boot") %>%
  group_by(scen.name, V, TheoryP, true.effect.dist) %>%
  summarise( MSE.winner = Method[ which.min(phatMSE) ],
             Bias.winner = Method[ which.min(abs(phatBias)) ] )
View(est.winners)

# inference winner
# winning at coverage is defined as having coverage as close to 95% as possible
inf.winners = res.all %>% group_by(scen.name, TheoryP, true.effect.dist, V) %>%
  summarise( Coverage.winner = Method[ which.min( abs(Cover-0.95) ) ],
             Coverage.of.winner = Cover[ which.min( abs(Cover-0.95) ) ],
             Width.winner = Method[ which.min(Width) ] )
View(inf.winners)



#################### RULE OF THUMB ####################

# apply candidate rule of thumb and see how behavior is
# returns the name of the method that should be used for point estimate
#  and for CI
# of course we can't use true.effect.dist or V in practice, but rather estimators
rule_of_thumb = function( true.effect.dist, 
                          V ) {
  if (true.effect.dist == "normal") est.method = "Parametric"
  else est.method = "NP sign test"
  
  if ( V > 0.01 ) inf.method = "NP sign test"
  else inf.method = "Boot"
  
  return(data.frame(est.method, inf.method))
}
rule_of_thumb("expo", .0005)

# bm
res.win = s %>% group_by(scen.name) %>%
  summarise( k = k[1],
             mu = mu[1],
             V = V[1],
             muN = muN[1],
             minN = minN[1],
             
             true.effect.dist = true.effect.dist[1],
             TheoryP = TheoryP[1],
             phat.winner.name = rule_of_thumb(true.effect.dist[1], V[1])$est.method,
             Cover.winner.name = rule_of_thumb(true.effect.dist[1], V[1])$inf.method,
             phat.winner = mean( phat[ Method == phat.winner.name ] ),
             phat.bias.winner = mean( phatBias[ Method == phat.winner.name ] ),
             Cover.winner = mean( Cover[ Method == Cover.winner.name ] ) )
View(res.win)

# sanity check
table(res.win$V > 0.01, res.win$Cover.winner.name)
table(res.win$true.effect.dist, res.win$phat.winner.name)

# also sanity check
s$V[ s$scen.name == 103 ][1]
s$true.effect.dist[ s$scen.name == 103 ][1]
mean( s$phat[ s$scen.name == 103 & s$Method == "Parametric"] )  # matches res.win :)
mean( s$Cover[ s$scen.name == 103 & s$Method == "Boot"] )  # matches res.win :)

# how badly do we do, considering all scenarios?
agg.win = res.win %>% group_by(V, true.effect.dist) %>%
  summarise( minCover = min(Cover.winner),
             q5Cover = quantile(Cover.winner, 0.05),
             maxRelBias = max( abs(phat.bias.winner) / TheoryP ),
             maxBias = max( abs(phat.winner - TheoryP)),
             q95RelBias = quantile( abs(phat.bias.winner) / TheoryP, 0.95 ),
             n.scen = n()
             )
# note: filtering by k > 5 doesn't rescue the low-heterogeneity scenarios
View(agg.win)

#################### RULES OF THUMB ####################

# rule_of_thumb = function( Pmin ) {
#   min.cover = min( res.all$Cover[ res.all$TheoryP >= Pmin & 
#                                     res.all$Method == "Theory"],
#                    na.rm = TRUE )
#   mean.cover =   mean( res.all$Cover[ res.all$TheoryP >= Pmin &
#                                         res.all$Method == "Theory" ],
#                        na.rm = TRUE )
#   
#   print( paste("Min: ", min.cover, sep="") )
#   print( paste("Mean: ", mean.cover, sep="") )
#   print("")
# }
# 
# 
# # choose a rule of thumb by looking at min and mean coverage in scenarios 
# #  fulfilling the rule
# vapply( c(0.30, 0.20, 0.15, 0.10),
#         rule_of_thumb,
#         FUN.VALUE = "sdf" )
# 
# 
# 
# # proportion of all scenarios with coverage > 0.90
# agg.rot = res.all %>% 
# group_by( Method ) %>%
#             filter( TheoryP >= 0.15 & 
#                       Method != "Logit" ) %>%
#             summarise( cov.over.0.90 = mean( Cover > 0.90, na.rm = TRUE ),
#                        cov.over.0.85 = mean( Cover > 0.85, na.rm = TRUE ),
#                       min.cov = min( Cover, na.rm = TRUE ),
#                       mean.cov = mean( Cover, na.rm = TRUE )  )
# 
# View(agg.rot)
# 
# 
# agg.all = res.all %>% group_by(  
#                             Method 
#             ) %>%
#               filter( Method != "Logit" ) %>%
#               summarise( cov.over.0.90 = mean( Cover > 0.90, na.rm = TRUE ),
#                          cov.over.0.85 = mean( Cover > 0.85, na.rm = TRUE ),
#                          min.cov = min( Cover, na.rm = TRUE ),
#                          mean.cov = mean( Cover, na.rm = TRUE )  )
# View(agg.all)
# 
#             
# # save all analysis objects for manuscript
# setwd("~/Dropbox/Personal computer/Independent studies/Meta-analysis metrics (RRR)/Linked to OSF (RRR)/Markdown manuscript")
# 
# save.image("all_simulation_objects.RData")
# 
# # save( list = ls(agg.rot, agg.all),
# #       file = "all_simulation_objects.RData" )






