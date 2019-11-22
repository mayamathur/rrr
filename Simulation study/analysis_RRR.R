
# Plots to create for paper: 
# - Rejection rate of Porig by delta (4 plots)
# - Coverage of Phat (all dists)
# - RMSE of Phat
# - Bias of Phat
#  - Width of Phat



# Note: "**" denotes key results reported in paper

# avoid saving old copies of objects
#rm(list=ls())

library(dplyr)
library(metRology)
library(ggplot2)
library(data.table)

# directories
code.dir = "~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)/Other RRR code (git)/Simulation study"
data.dir =  "~/Desktop"
results.dir = "~/Desktop"

# helper fn
setwd(code.dir)
source("functions_analysis_RRR.R")
source("functions_RRR.R")  # for sim_data


setwd(data.dir)
s = as.data.frame( fread("stitched.csv") )
# s1 = as.data.frame( fread("stitched_1.csv") )
# s2 = as.data.frame( fread("stitched_2.csv") )
# s3 = as.data.frame( fread("stitched_3.csv") )
# # reorder latter's rows to match s1
# s2 = s2[ , names(s1) ]
# s = rbind(s1, s2)
dim(s)


#################### DATA QUALITY CHECKS ####################

# percent finished
n.scen = 4320
reps.per.scen = 512*2
nrow(s) / (n.scen * reps.per.scen)

# reasons for rep failure
prop.table( table( s$Note, useNA = "ifany" ) ) 

# finished sim reps, missing data, and rep.time by scenario
as.data.frame( s %>% group_by(scen.name) %>%
                 summarise( sim.reps = n(),
                            k = k[1],
                            prop.failed = round( mean(!is.na(Note)), 2 ),
                            mean.time = round( mean(foreach.time), 0 ),
                            max.time = round( max(foreach.time), 0 ) ) ) %>%
  arrange(sim.reps)



#################### LOOK AT JUST THE DIAGNOSTIC SCENARIOS ####################

# larger V and changing sd.w
s %>% filter( true.effect.dist == "normal" &
                delta == 0 &
                #V == 0.25 &
                minN == 200 & 
                N.orig == 200 ) %>%
  group_by(V, k, sd.w) %>%
  summarise( I2.mn = mean(EstI2),
             Reject.mn = mean(Reject),
             n = n())

# further increasing k above 25 helps again?



# compare to local version
s %>% filter( true.effect.dist == "normal" &
                 k == 15 &
                 V == 0.25 &
                 minN == 200 &
                 #muN == 250 &
                 N.orig == 200 &
                 delta == 0 ) %>%
  #group_by(TheoryP) %>%
  summarise(Reject.mn = mean(Reject))
# 0.7-0.9


# remove the other sd.w
s = s %>% filter( POrig.Method == "reml" )



#################### TYPE I ERROR RATE (DELTA = 0) ####################

# scenario parameters that vary
varying = c("k", 
            "V",
            "minN",
            #"TheoryP",  # don't group by TheoryP because not relevant for Porig
            "POrig.Method",
            "delta",
            "N.orig",
            "true.effect.dist")

# Type I error for normal case
typeI = s %>% filter( true.effect.dist == "normal" & delta == 0 & POrig.Method == "reml" ) %>%
  group_by(V, k) %>%
  summarise( I2.mn = mean(EstI2),
             EstMean.mn = mean(EstMean),
             EstVar.mn = mean(EstVar),
             Reject.mn = mean(Reject),
             n = n()) %>%
  arrange(Reject.mn)
View(typeI)
# **in the cases where we over-reject, seems like the issue is that we're 
# underestimating V, not that we're estimating phat wrong
# bm

# ".dots" argument allows passing a vector of variable names
agg = as.data.frame( s %>% group_by(.dots = varying) %>%
                 summarise( n = n(),
                            phat.mn = mean(phat),
                            phatBias.mn = mean(phatBias),
                            Cover.mn = mean(Cover, na.rm=TRUE),
                            #Width.mn = mean(Width),
                            Porig.mn = mean(Porig),
                            Reject.mn = mean(Reject),
                            I2.mn = mean(EstI2) ) )
# sanity check: did we group on all the needed variables?
library(testthat)
expect_equal( nrow(agg), length(unique(s$scen.name) ) )



# for plotting joy
agg$N.tot = agg$minN * agg$k  # only works when minN = muN
agg$k.pretty = paste( "No. replications:", agg$k )
agg$N.tot.pretty = paste( "N = ", agg$N.tot )
agg$N.orig.pretty = paste( "Norig = ", agg$N.orig )
agg$delta.pretty = paste( "delta = ", agg$delta )
agg$V.pretty = paste( "V = ", agg$V )
# agg$TheoryP.pretty = paste( "True P = ",
#                                 format(round(agg$TheoryP, 2), nsmall = 2) )

agg$dist.pretty = NA
agg$dist.pretty[ agg$true.effect.dist == "normal" ] = "Normal"
agg$dist.pretty[ agg$true.effect.dist == "expo" ] = "Exponential"
agg$dist.pretty[ agg$true.effect.dist == "t.scaled" ] = "t"
agg$dist.pretty[ agg$true.effect.dist == "unif2" ] = "Uniform mixture"



# **normal only
library(ggplot2)
ggplot( data = agg %>% filter(delta == 0 & true.effect.dist == "normal"),
        aes(x = k,
            y = Reject.mn,
            color = as.factor(minN),
            shape = as.factor(N.orig),
            lty = POrig.Method) ) +
  geom_point(size=2) +
  geom_line() +
  geom_hline(yintercept = 0.05,
             color = "red",
             lty = 2) +
  scale_y_continuous( limits = c(0, .15),
                      breaks = seq(0, .15, .01)) +
  facet_wrap(~ N.orig + V.pretty) +
  theme_bw()

# same, but using total N in replications instead of N per replication
ggplot( data = agg %>% filter(delta == 0 & true.effect.dist == "normal"),
        aes(x = k,
            y = Reject.mn,
            color = as.factor(minN),
            shape = as.factor(N.orig),
            lty = POrig.Method) ) +
  geom_point(size=2) +
  geom_line() +
  geom_hline(yintercept = 0.05,
             color = "red",
             lty = 2) +
  scale_y_continuous( limits = c(0, .15),
                      breaks = seq(0, .15, .01)) +
  facet_wrap(~ N.orig + V.pretty) +
  theme_bw()


# with minN on the x-axis; all dists
shapes = c(69, 78, 84, 85)
library(ggplot2)
ggplot( data = agg[agg$delta == 0,],
        aes(x = k,
            y = Reject.mn,
            color = as.factor(minN),
            shape = dist.pretty) ) +
  geom_point(size=2) +
  #geom_line() +
  geom_hline(yintercept = 0.05,
             color = "red",
             lty = 2) +
  scale_shape_manual(values = shapes) +
  facet_wrap(~ dist.pretty + V.pretty) +
  theme_bw()

# # with distributions on the x-axis
# library(ggplot2)
# ggplot( data = agg[agg$delta == 0,],
#         aes(x = dist.pretty,
#             y = Reject.mn,
#             color = k.pretty,
#             shape = as.factor(minN) ) ) +
#   geom_point(size=2) +
#   #geom_line() +
#   geom_hline(yintercept = 0.05,
#              color = "red",
#              lty = 2) +
#   #scale_shape_manual(values = shapes) +
#   facet_wrap(~ TheoryP.pretty + V.pretty) +
#   theme_bw()



# increasing the sample size within the replications doesn't really help
# but increasing the number of replications definitely does


# ** best version: all distributions
library(ggplot2)
ggplot( data = agg %>% filter(delta == 0),
        aes(x = k,
            y = Reject.mn,
            color = as.factor(minN),
            shape = as.factor(N.orig)) ) +
  geom_point(size=2) +
  geom_line() +
  facet_wrap(~ dist.pretty + V.pretty) +
  theme_bw()



#################### CANDIDATE RULE OF THUMB FOR TYPE I ERROR ####################

# candidate rule of thumb: ?
# also pretty robust to other distributions, surprisingly
# this rule seems to yield a max Type I error of 6%
agg %>% filter( delta == 0 &
                  true.effect.dist == "normal" ) %>%
  mutate(N.tot = k*minN) %>%
  group_by(N.tot, N.orig, POrig.Method) %>%
  summarise( n.scen = n(),
            Reject.max = max(Reject.mn), # max of scenarios' mean rejection rates
             Reject.mn2 = mean(Reject.mn),
            Perc.Okay = mean(Reject.mn < .06) )


# predictors of having unacceptable Type I error
# excluding N.orig only temporarily since it's not varying yet
covars = varying[ !varying %in% c("true.effect.dist", "delta", "N.orig") ]

agg$N.tot = agg$k * agg$minN

#( string = paste( "Reject.mn > 0.05 ~ ", paste( covars, collapse="+" ), sep = "") )
m = glm( Reject.mn > 0.05 ~ N.tot * V + POrig.Method,
     data = agg %>% filter(true.effect.dist == "normal" & delta == 0) )
summary(m)

m = lrm( as.formula(string),
         data = agg[ agg$true.effect.dist == "normal" & agg$delta == 0, ] )

#################### POWER (DELTA > 0) ####################

# with minN on the x-axis; normal only
# power is always horrible for delta = 0.2
# with k = 25, sometimes ok for delta = 0.5
library(ggplot2)
ggplot( data = agg %>% filter(delta == 1),
        aes(x = k,
            y = Reject.mn,
            color = as.factor(minN),
            shape = as.factor(N.orig)) ) +
  geom_point(size=2) +
  geom_line() +
  facet_wrap(~ dist.pretty + V.pretty) +
  theme_bw()
# 9 x 8


#################### ** TABLES FOR PAPER: HIGH-LEVEL SUMMARY OF INFERENCE AND POINT ESTIMATE RESULTS ####################

##### Inference Summary #####
inf.table = agg %>%
  filter(k>5, !is.na(Cover.mn)) %>%
 # group_by(V, Method.inf.pretty) %>%
  summarise( Mean.Cover = mean(Cover.mn),
             Min.Cover = min(Cover.mn),
             #q5Cover = quantile(Cover, 0.05, na.rm = TRUE),
             #Cover.Above.90 = sum(Cover>.9)/length(Cover),
             Width = mean(Width.mn) )
View(inf.table)
library(xtable)
print( xtable(inf.table), include.rownames = FALSE )

##### Point Estimates Summary #####
est.table = agg %>%
  group_by(true.effect.dist, V) %>%
  summarise( #Mean.RMSE = MetaUtility::format_stat( mean(RMSE), digits = 3 ),
             Mean.Abs.Bias = MetaUtility::format_stat( mean(abs(phatBias.mn)), digits = 3 ) )
View(est.table)

library(xtable)
print( xtable(est.table), include.rownames = FALSE )







#################### PHAT PLOTS ####################


# scenario parameters that vary
varying = c("k", 
            "V",
           # "minN",  # ~~~ averaging over these
          #  "muN",
            "TheoryP",  # don't group by TheoryP because not relevant for Porig
            "delta",
            "N.orig",
            "true.effect.dist",
            "sd.w")

# ".dots" argument allows passing a vector of variable names
agg = as.data.frame( s %>% group_by(.dots = varying) %>%
                       summarise( phat.mn = mean(phat),
                                  phatBias.mn = mean(phatBias),
                                  Cover.mn = mean(Cover, na.rm=TRUE),
                                  #Width.mn = mean(Width),
                                  Porig.mn = mean(Porig),
                                  Reject.mn = mean(Reject),
                                  I2.mn = mean(EstI2) ) )



# for plotting joy
agg$k.pretty = paste( "No. replications:", agg$k )
agg$muN.pretty = paste( "E[N] = ", agg$muN )
agg$N.orig.pretty = paste( "Norig = ", agg$N.orig )
agg$delta.pretty = paste( "delta = ", agg$delta )
agg$V.pretty = paste( "V = ", agg$V )
agg$TheoryP.pretty = paste( "True P = ",
                            format(round(agg$TheoryP, 2), nsmall = 2) )

agg$dist.pretty = NA
agg$dist.pretty[ agg$true.effect.dist == "normal" ] = "Normal"
agg$dist.pretty[ agg$true.effect.dist == "expo" ] = "Exponential"
agg$dist.pretty[ agg$true.effect.dist == "t.scaled" ] = "t"
agg$dist.pretty[ agg$true.effect.dist == "unif2" ] = "Uniform mixture"


# plot one level of heterogeneity  
# .data: res.all or some subset of it (e.g., for one distribution)
plot_group = function( 
  .title ="",
  .ylab = "Coverage",
  .legend = TRUE,
  .y.name,
  .limits = c(0.7, 1), 
  .breaks = seq(0,1,.1),
  .data ) {
  
  colors = c("orange", "black", "red", "blue")
  
  p = 
    ggplot( .data, aes_string( x="k",
                               y=.y.name,
                               shape = "N.orig.pretty",
                               color="TheoryP.pretty" ) ) +
    geom_line(lwd=1) +
    geom_point(size=2) +
    theme_bw() +
    scale_color_manual(values=colors) +
    scale_alpha_continuous( limits = c(0,1) ) +
    scale_y_continuous( limits=.limits, breaks=.breaks ) +
    ylab(.ylab) +
    ggtitle( .title ) +
    facet_wrap( ~ dist.pretty + V.pretty )
  #facet_grid( TheoryP.pretty ~ muN.pretty )
  
  # from other plots
  # library(ggplot2)
  # ggplot( data = agg %>% filter(delta == 0),
  #         aes(x = k,
  #             y = Reject.mn,
  #             color = as.factor(minN),
  #             shape = as.factor(N.orig)) ) +
  #   geom_point(size=2) +
  #   geom_line() +
  #   facet_wrap(~ dist.pretty + V.pretty) +
  #   theme_bw()

  
  if ( .ylab == "Coverage" ) p = p + geom_hline(yintercept = 0.95, linetype=2)
  if ( .ylab == "Bias" ) p = p + geom_hline(yintercept = 0, linetype=2)
  
  if ( .legend == TRUE ) {
    return(p)
  } else {
    return(p + theme(legend.position="none"))
  }
  
}  

plot_group(.y.name = "Cover.mn",
           .ylab = "Coverage",
           .data = agg,
           .limits = c(0.8,1))
# bm: not sure why there are still multiple lines


plot_group(.y.name = "phatBias.mn",
           .ylab = "Bias",
           .data = agg,
           .limits = c(-0.1, .4))
           #.limits = c(0.8,1))
# bm: not sure why there are still multiple lines

# shapes: Norig
# colors: minN










#### ~~~ OLD STUFF FROM NPPHAT:

# should we re-simulate data for the example density plots of the 4 distributions (slow)?
simulate.dist.from.scratch = FALSE

# should the four datasets of simulation results be read in and merged from scratch (somewhat slow)?
merge.from.scratch = FALSE


#################### READ IN DATA ####################

# merge data from the four simulation datasets
if (merge.from.scratch == TRUE) {
  setwd(data.dir)
  library(data.table)
  s1 = fread("stitched_expo_normal.csv")
  names(s1) = as.character(s1[1,])
  s1 = as.data.frame(s1[-c(1), -1])
  s1$sim_group = 1  # identify which "group" of simulations this belongs to
  
  s2 = fread("stitched_unif_t.csv")
  #names(s2) = as.character(s2[1,])
  s2 = as.data.frame(s2[-c(1), -1])
  s2$sim_group = 2  # identify which "group" of simulations this belongs to
  
  s3 = fread("stitched_all4_ensemble.csv")
  names(s3) = as.character(s3[2,])
  s3 = as.data.frame(s3[-c(1:2), -1])
  s3$sim_group = 3  # identify which "group" of simulations this belongs to
  # fix stupid names situation
  
  s4 = fread("stitched_fill_missing.csv")
  s4 = as.data.frame(s4[, -1])
  s4$sim_group = 4  # identify which "group" of simulations this belongs to

  #s = rbind(s1, s2, s3, s4)
  s = rbindlist(list(s1, s2, s3, s4), fill = TRUE)
  
  write.csv(s,
            "overall_stitched.csv",
            row.names = FALSE)
} else {
  # read in existing data
  setwd(data.dir)
  s = as.data.frame( fread("overall_stitched.csv") )
}



#################### DATA CLEANING AND QUALITY CHECKS ####################

# remove erroneously-written rows with nonsense scenario names (e.g., "TheoryP")
table( s$scen.name )
s = s[ !is.na( as.numeric(s$scen.name) ), ]
s = droplevels(s)

# rename the scenarios
# note that some identical scenarios have different names
#  this is an artifact from having run multiple sets of simulations
s = s %>% group_by(k, minN, V, true.effect.dist, TheoryP) %>%  # these are all the variables defining unique scenarios
  mutate(scen.name.2 = group_indices())
  

# how many reps were run per scenario and Method?
reps = s %>% group_by(scen.name.2, Method) %>%
  summarise( n = n() ) %>%
  arrange(n)
#View(reps)
# scenarios with too few reps for any method
# these are due to bootstrapping failures
length( unique( reps$scen.name.2[ reps$n < 500 ] ) )


# proportion of times the ensemble bootstrap failed
( boot.failures = s %>% filter(sim_group == 3) %>%  # only look at reps from last group; otherwise they wouldn't have attempted bootstrap
    group_by(k, TheoryP) %>%
    filter(Method == "NP ensemble") %>%
    summarise( boot.problem = sum(!is.na(Note)) / length(Note) ) )
#View(boot.failures)

# ** overall mean boot failures (reported in Figure 1)
# only use sim groups 3 and 4, where NP-ensemble actually was bootstrapped
mean( is.na( s$Note[s$sim_group %in% c(3,4) & s$Method == "NP ensemble"] ) )


# make sure variables are correct type
s = as.data.frame(s)
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


#################### AGGREGATE BY SCENARIO ####################

# for plotting joy
s$plot.panel = paste( expression(tau^2), " = ", s$V, 
                      "; q = ", s$q,
                      "; true P = ", s$TheoryP,
                      sep = "" )

# ** summarize results by method and scenario
res.all = s %>%
  filter(!is.na(Method)) %>%  # do not remove this -- behaves strangely if you do, despite use of na.rm below
  group_by(scen.name.2, Method) %>%
  #filter( scen.name == 236 & sim_group == 4 ) %>%  # DEBUGGING ONLY
  group_by(k, V, minN, TheoryP, true.effect.dist, Method) %>%  # all variables that define unique scenarios
  mutate(EmpVar = var(phat),
         n.cover = sum( !is.na(Cover) ),
         n.phat = sum( !is.na(phat) ),
         n.rows = n(),
         RMSE = sqrt( phatBias^2 + EmpVar ) ) %>%
  summarise_if( is.numeric, function(x) mean(x, na.rm = TRUE) ) %>%
  arrange(n.cover, n.phat) 

View(res.all)

setwd(results.dir)
# don't overwrite
# write.csv(res.all, 
#           "res_all.csv",
#           row.names = FALSE)


#################### PLOTS ####################

# for plotting joy
res.all$muN.pretty = paste( "E[N] = ", res.all$muN )
res.all$TheoryP.pretty = paste( "True P = ",
                                format(round(res.all$TheoryP, 2), nsmall = 2) )

# also for plotting joy
# name the methods differently for inference vs. point estimate plots
library(car)
res.all$Method.inf.pretty = recode( res.all$Method,
                                    " 'Boot'='BCa-parametric';
                         'NP ensemble'='BCa-calibrated';
                         'NP sign test'='Sign test'" )
res.all$Method.est.pretty = recode( res.all$Method,
                                    "'NP ensemble'='Calibrated';
                         'NP sign test'='Sign test max.'" )


# need to have folder on Desktop called "Plots for each distribution"
all_plots_one_dist(true.effect.dist = "expo",
                   bias.max = round(max(res.all$phatBias) + 0.01, 2),
                   bias.min = round( min(res.all$phatBias) - 0.01, 2),
                   mse.max = max(res.all$RMSE, na.rm = TRUE) + 0.01,
                   cover.min = .1,
                   results.path = "~/Desktop/Plots for each distribution")

all_plots_one_dist(true.effect.dist = "normal",
                   bias.max = round(max(res.all$phatBias) + 0.01, 2),
                   bias.min = round( min(res.all$phatBias) - 0.01, 2),
                   mse.max = max(res.all$RMSE, na.rm = TRUE) + 0.01,
                   cover.min = .1,
                   results.path = "~/Desktop/Plots for each distribution")

all_plots_one_dist(true.effect.dist = "unif2",
                   bias.max = round(max(res.all$phatBias) + 0.01, 2),
                   bias.min = round( min(res.all$phatBias) - 0.01, 2),
                   mse.max = max(res.all$RMSE, na.rm = TRUE) + 0.01,
                   cover.min = .1,
                   results.path = "~/Desktop/Plots for each distribution")

all_plots_one_dist(true.effect.dist = "t.scaled",
                   bias.max = round(max(res.all$phatBias) + 0.01, 2),
                   bias.min = round( min(res.all$phatBias) - 0.01, 2),
                   mse.max = max(res.all$RMSE, na.rm = TRUE) + 0.01,
                   cover.min = .1,
                   results.path = "~/Desktop/Plots for each distribution")


#################### WINNING METHOD BY SCENARIO ####################

# point estimate winner
# exclude boot since it's not a "real" point estimate
est.winners = res.all %>% filter(Method != "Boot") %>%
  group_by(scen.name, V, TheoryP, true.effect.dist) %>%
  summarise( MSE.winner = Method[ which.min(RMSE) ],
             Bias.winner = Method[ which.min(abs(phatBias)) ] )
View(est.winners)

# inference winner
# winning at coverage is defined as having coverage as close to 95% as possible
inf.winners = res.all %>% group_by(scen.name, TheoryP, true.effect.dist, V) %>%
  summarise( Coverage.winner = Method[ which.min( abs(Cover-0.95) ) ],
             Coverage.of.winner = Cover[ which.min( abs(Cover-0.95) ) ],
             Width.winner = Method[ which.min(Width) ] )
View(inf.winners)




#################### ** FLOWCHART STATS: SUMMARIZE INFERENCE RESULTS FOR FLOWCHART TERMINAL NODES ####################

##### Inference #####

# baseline requirements:
#  - k >= 10

# first use BCa if it converges:
# min coverage: 91%
nodeA = res.all %>%
  filter(k > 5) %>%
  filter(!is.na(Cover)) %>%
  group_by(Method) %>%
  summarise( Mean.Cover = mean(Cover),
             Min.Cover = min(Cover),
             Cover.Above.90 = sum(Cover>.9)/length(Cover),
             Width = mean(Width), 
             reps = n() )
View(nodeA)

# if it doesn't converge, here are scenarios in which 
#  you can use NP sign test instead:
#  - normal or t-distributed effects
#  - tau/mu >= 1 (i.e., V >= 0.25)
# min coverage: 92%
nodeB = res.all %>%
  filter(k > 5) %>%
  filter(V==.25)%>%
  filter(true.effect.dist %in% c("normal", "t.scaled")) %>%
  filter(!is.na(Cover)) %>%
  group_by(Method) %>%
  summarise( Mean.Cover = mean(Cover),
             Min.Cover = min(Cover),
             Cover.Above.90 = sum(Cover>.9)/length(Cover),
             Width = mean(Width) )
View(nodeB)



#################### PLOT EXAMPLE DATA FROM EACH DIST ####################

vec = c("normal", "expo", "unif2", "t.scaled")
vec2 = c(0.25, 0.16, 0.09, 0.04, 0.01)

params = as.data.frame( expand.grid(vec, vec2) )
names(params) = c("dist", "V")

if ( simulate.dist.from.scratch == TRUE ) {
  sims = apply( params,
                MARGIN = 1, 
                FUN = function(row) {
                  d = sim_data( k = 1000,
                                mu = 0.5,
                                V = as.numeric(row[["V"]]),
                                muN = 850,
                                minN = 800,
                                sd.w = 1,
                                true.effect.dist = row[["dist"]])
                  d$true.effect.dist = row[["dist"]]
                  d$V = as.numeric(row[["V"]])
                  return(d)
                } )
  
  sims = do.call(rbind, sims)
  setwd(results.dist.dir)
  write.csv(sims, 
            "simulated_density_data.csv",
            row.names = FALSE)
} else {
  setwd(results.dist.dir)
  sims = read.csv("simulated_density_data.csv")
  
  # we had simulated extra values of V, but then decided against actually simulating them
  # remove those
  sims = sims %>% filter( sims$V %in% c(0.01, 0.04, 0.25))
}

# for plotting joy
sims$dist.pretty = "Normal"
sims$dist.pretty[ sims$true.effect.dist == "expo"] = "Exponential"
sims$dist.pretty[ sims$true.effect.dist == "t.scaled"] = "t"
sims$dist.pretty[ sims$true.effect.dist == "unif2"] = "Uniform mixture"

# for plotting joy
sims$V.pretty = paste( expression(tau^2), " = ", sims$V,
                       sep = "")

# make density plot
ggplot( data = sims,
        aes(x = Mi,
            fill = dist.pretty)) +
  #geom_histogram(bins=20) +
  geom_vline(xintercept = 0.5, color = "gray") +
  geom_density(adjust=.5) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # last two arguments remove gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(-0.5, 1.5, .5),
                     limits = c(-0.5, 1.5)) +
  facet_grid(dist.pretty ~ V.pretty) +
  xlab( bquote(theta[i]) ) +
  ylab("") +
  labs(fill = "")
# 11 x 8 works well

# # sanity check
# # since t looks just like normal visually, confirm via Shapiro that 
# #  it really is different
# sims %>% filter(dist.pretty == "Normal") %>%
#   group_by(V) %>% 
#   summarise( shapiro.p = shapiro.test(Mi)$p.value )
# # works :) 

