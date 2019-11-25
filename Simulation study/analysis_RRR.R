
# Plots to create for paper: 
# - Rejection rate of Porig by delta (4 plots)
# - Coverage: Just quick stats
# - RMSE of Phat
# - Bias of Phat


# Note: "**" denotes key results reported in paper

# avoid saving old copies of objects
#rm(list=ls())

library(dplyr)
library(metRology)
library(ggplot2)
library(data.table)

# directories
root.dir = "~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)"
data.dir =  "~/Desktop"  # ~~~ update me later
code.dir = paste(root.dir, "Other RRR code (git)/Simulation study", sep="/")
results.dir = paste(root.dir, "Simulation study results/Simulated distributions", sep="/")
results.dist.dir = paste(root.dir, "Simulation study results/Simulated distributions", sep="/")

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



#################### MAKE 2 AGGREGATED DATASETS ####################

# in these datasets, n is the number of simulation reps
#  which may be >1024 (the number per scenario) because sometimes we can group certain scenarios together

##### Aggregated Dataset for Porig Performance #####
# aggregates across all values of TheoryP since that's about q, and q is irrelevant for Porig
# scenario parameters that vary
varying = c("k", 
            "V",
            "minN",
            "POrig.Method",  # REML or PM
            "delta",
            "N.orig",
            "true.effect.dist")


# need to group on POrig.Method because it's repeated within scen.name
# (each scenario provides a row for each method)
agg1 = as.data.frame( s %>%
                        group_by(.dots = varying) %>%  # ".dots" argument allows passing a vector of variable names
                        #group_by(scen.name, POrig.Method) %>%
                       summarise( n = n(),
                                  # k = k[1],
                                  # V = V[1],
                                  # minN = minN[1],
                                  # POrig.Method = POrig.Method[1],
                                  # delta = delta[1],
                                  # N.orig = N.orig[1],
                                  Reject.mn = mean(Reject),
                                  EstMean.mn = mean(EstMean),
                                  EstVar.mn = mean(EstVar),
                                  I2.mn = mean(EstI2) ) )
# note that this dataset will have FEWER rows than number of unique scenarios because we don't 
#  need to group on TheoryP here
dim(agg1)
# # merge in the scenario parameters
# agg1 = left_join( agg1,
#                   s[ !duplicated(s$scen.name), c("scen.name", varying) ],
#                   by = "scen.name" )


# for plotting joy
agg1$N.tot = agg1$minN * agg1$k  # only works when minN = muN
agg1$k.pretty = paste( "No. replications:", agg1$k )
agg1$N.tot.pretty = paste( "N = ", agg1$N.tot )
agg1$N.orig.pretty = paste( "Norig = ", agg1$N.orig )
agg1$delta.pretty = paste( "delta = ", agg1$delta )
agg1$V.pretty = paste( "V = ", agg1$V )

agg1$dist.pretty = NA
agg1$dist.pretty[ agg1$true.effect.dist == "normal" ] = "Normal"
agg1$dist.pretty[ agg1$true.effect.dist == "expo" ] = "Exponential"
agg1$dist.pretty[ agg1$true.effect.dist == "t.scaled" ] = "t"
agg1$dist.pretty[ agg1$true.effect.dist == "unif2" ] = "Uniform mixture"
agg1$dist.pretty = factor(agg1$dist.pretty,
                          levels = c("Normal",
                                     "Exponential",
                                     "t",
                                     "Uniform mixture"))
levels(agg1$dist.pretty)


##### Aggregated Dataset for Phat Performance #####
# unlike above, need to group by TheoryP 
# and don't need to group by delta or N.orig
# and also need to remove one of the two heterogeneity methods since the rest of their data is duplicated
varying = c("k", 
            "V",
            "minN",
            #"N.orig",
            "TheoryP",
            "true.effect.dist")


# need to group on POrig.Method because it's repeated within scen.name
# (each scenario provides a row for each method)
agg2 = as.data.frame( s %>%
                        filter(POrig.Method == "reml") %>%  # irrelevant for Phat, but need to get rid of duplicated data
                        group_by(.dots = varying) %>%
                        summarise( n = n(),
                                   Phat.mn = mean(phat),
                                   Phat.bias.mn = mean(phatBias),
                                   Phat.absbias.mn = mean( abs(phatBias) ), # ~~~ bm: thinking about whether this is right
                                   Phat.bt.med.mn = mean(phat.bt.med),
                                   Cover.mn = mean(Cover, na.rm = TRUE),
                                   EmpVar = var(phat),
                                   RMSE = sqrt(EmpVar + Phat.bias.mn^2) ) )

# for plotting joy
agg2$N.tot = agg2$minN * agg2$k  # only works when minN = muN
agg2$k.pretty = paste( "No. replications:", agg2$k )
agg2$N.tot.pretty = paste( "N = ", agg2$N.tot )
agg2$minN.pretty = paste( "N per replication = ", agg2$minN )
#agg2$Norig.pretty = paste( "N original = ", agg2$N.orig )
agg2$V.pretty = paste( "V = ", agg2$V )
agg2$TheoryP.pretty = paste( "True P = ",
                            format(round(agg2$TheoryP, 2), nsmall = 2) )


agg2$dist.pretty = NA
agg2$dist.pretty[ agg2$true.effect.dist == "normal" ] = "Normal"
agg2$dist.pretty[ agg2$true.effect.dist == "expo" ] = "Exponential"
agg2$dist.pretty[ agg2$true.effect.dist == "t.scaled" ] = "t"
agg2$dist.pretty[ agg2$true.effect.dist == "unif2" ] = "Uniform mixture"
agg2$dist.pretty = factor(agg2$dist.pretty,
                          levels = c("Normal",
                                     "Exponential",
                                     "t",
                                     "Uniform mixture"))

# save the datasets for OSF and Shiny app
setwd(results.dir)
write.csv( agg1, "agg1_porig.csv", row.names = FALSE )
write.csv( agg2, "agg2_phat.csv", row.names = FALSE )


#################### TYPE I ERROR RATE (DELTA = 0) ####################

# # Type I error for normal case
# typeI = s %>% filter( true.effect.dist == "normal" & delta == 0 & POrig.Method == "reml" ) %>%
#   group_by(V, k) %>%
#   summarise( I2.mn = mean(EstI2),
#              EstMean.mn = mean(EstMean),
#              EstVar.mn = mean(EstVar),
#              Reject.mn = mean(Reject),
#              n = n()) %>%
#   arrange(Reject.mn)
# View(typeI)
# # **in the cases where we over-reject, seems like the issue is that we're 
# # underestimating V, not that we're estimating phat wrong


# ##### normal only #####
# # **normal only
# library(ggplot2)
# ggplot( data = agg1 %>% filter(delta == 0 & true.effect.dist == "normal" & POrig.Method == "reml"),
#         aes(x = k,
#             y = Reject.mn,
#             color = as.factor(minN),
#             shape = as.factor(N.orig) ) ) +
#   geom_point(size=2) +
#   geom_line() +
#   geom_hline(yintercept = 0.05,
#              color = "red",
#              lty = 2) +
#   scale_y_continuous( limits = c(0, .15),
#                       breaks = seq(0, .15, .01)) +
#   facet_wrap(~ N.orig + V.pretty) +
#   theme_bw()

##### **Plot 1: Type I Error for All Distributions #####
shapes = c(69, 78, 84, 85)
colors = c("black", "orange")
library(ggplot2)
ggplot( data = agg1 %>% filter(delta == 0 & POrig.Method == "reml"),
        aes(x = k,
            y = Reject.mn,
            color = as.factor(minN)
            #shape = dist.pretty
            ) ) +
  geom_point(size=2) +
  geom_line() +
  geom_hline(yintercept = 0.05,
             color = "red",
             lty = 2) +
  scale_shape_manual(values = shapes) +
  scale_color_manual(values = colors,
                     name = "Sample size in each replication") +
  scale_y_continuous( limits = c(0,.13),
                      breaks = seq(0,.13,.01)) +
  ylab("Mean rejection rate") +
  xlab("Number of replications") + 
  #facet_wrap(~ dist.pretty + V.pretty, nrow=4) +
  facet_grid(dist.pretty ~ V.pretty) +
  theme_bw()

# # with distributions on the x-axis
# library(ggplot2)
# ggplot( data = agg1[agg1$delta == 0,],
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



#################### POWER (DELTA > 0) ####################

##### **Plot 2: Power for All Distributions #####
# with minN on the x-axis; all dists
shapes = c(69, 78, 84, 85)
colors = c("black", "orange")
library(ggplot2)
ggplot( data = agg1 %>% filter(delta > 0 & POrig.Method == "reml"),
        aes(x = k,
            y = Reject.mn,
            color = as.factor(minN)
            #shape = dist.pretty
        ) ) +
  geom_point(size=2) +
  geom_line() +
  geom_hline(yintercept = 0.05,
             color = "red",
             lty = 2) +
  scale_shape_manual(values = shapes) +
  scale_color_manual(values = colors,
                     name = "Sample size in each replication") +
  # scale_y_continuous( limits = c(0,.13),
  #                     breaks = seq(0,.13,.01)) +
  ylab("Mean power") +
  xlab("Number of replications") + 
  #facet_wrap(~ dist.pretty + V.pretty, nrow=4) +
  facet_grid(dist.pretty ~ V.pretty + delta) +
  theme_bw()



#################### CANDIDATE RULE OF THUMB FOR TYPE I ERROR ####################

# candidate rule of thumb: ?
# also pretty robust to other distributions, surprisingly
# this rule seems to yield a max Type I error of 6%
agg1 %>% filter( delta == 0 &
                  true.effect.dist == "normal" &
                   POrig.Method == "reml" &
                   k >= 10 &
                   N.tot >= 4000 ) %>%
  mutate(N.tot = k*minN) %>%
  #group_by(k, minN, N.orig, POrig.Method) %>%
  summarise( n.scen = n(),
            Reject.max = max(Reject.mn), # max of scenarios' mean rejection rates
             Reject.mn2 = mean(Reject.mn),
            Perc.Okay = mean(Reject.mn < .06) )


# predictors of having unacceptable Type I error
# excluding N.orig only temporarily since it's not varying yet
covars = varying[ !varying %in% c("true.effect.dist", "delta", "N.orig") ]

agg1$N.tot = agg1$k * agg1$minN

#( string = paste( "Reject.mn > 0.05 ~ ", paste( covars, collapse="+" ), sep = "") )
m = glm( Reject.mn > 0.05 ~ N.tot * V + POrig.Method,
     data = agg1 %>% filter(true.effect.dist == "normal" & delta == 0) )
summary(m)

m = lrm( as.formula(string),
         data = agg1[ agg1$true.effect.dist == "normal" & agg1$delta == 0, ] )




#################### PHAT PLOTS ####################

##### Plot: Coverage ######
# not interesting since it's always very high
plot_group(.y.name = "Cover.mn",
           .ylab = "Coverage",
           .data = agg2,
           .limits = c(0.85,1),
           .breaks = seq(0.85,1,.05))

# **Min coverage for k >= 10
agg2 %>% filter( k >= 10 ) %>%
  summarise( min(Cover.mn) )


##### **Plot 3: Bias #####
plot_group(.y.name = "Phat.bias.mn",
           .ylab = "Bias",
           .data = agg2,
           .limits = c(-0.1, .4))

# absolute bias
plot_group(.y.name = "Phat.absbias.mn",
           .ylab = "Absolute bias",
           .data = agg2,
           .limits = c(-0.1, .4))


##### **Plot 4: RMSE #####
plot_group(.y.name = "RMSE",
           .ylab = "RMSE",
           .data = agg2,
           .limits = c(0, .5))


#################### ** TABLES FOR PAPER: HIGH-LEVEL SUMMARY OF INFERENCE AND POINT ESTIMATE RESULTS ####################

#####  ***Porig Summary #####

Porig.table = agg1 %>%
  filter( k >= 10 ) %>%
  group_by(dist.pretty, V, minN, delta) %>%
  summarise( Reject = round( mean(Reject.mn), 3 ),
             Reject.max = round( max(Reject.mn), 3 ) ) 
View(Porig.table)

#####  ***Phat Summary #####
Phat.table = agg2 %>%
  filter(k >= 10) %>%
  group_by(dist.pretty, V, minN) %>%
  summarise( 
    Mean.Bias = round( mean(Phat.bias.mn), digits = 3),
    Mean.Abs.Bias = round( mean(Phat.absbias.mn), digits = 3),  # abs value, then mean
    #Mean.Abs.Bias2 = round( mean(abs(Phat.bias.mn)), digits = 3 ),  # mean bias within scenario, then abs valute
    Mean.RMSE = round( mean(RMSE), digits = 3 ) )
View(Phat.table)

library(xtable)
print( xtable(Phat.table), include.rownames = FALSE )








# 
# ##### Inference Summary #####
# inf.table = agg %>%
#   filter(k>5, !is.na(Cover.mn)) %>%
#  # group_by(V, Method.inf.pretty) %>%
#   summarise( Mean.Cover = mean(Cover.mn),
#              Min.Cover = min(Cover.mn),
#              #q5Cover = quantile(Cover, 0.05, na.rm = TRUE),
#              #Cover.Above.90 = sum(Cover>.9)/length(Cover),
#              Width = mean(Width.mn) )
# View(inf.table)
# library(xtable)
# print( xtable(inf.table), include.rownames = FALSE )
# 




#################### PLOT EXAMPLE DATA FROM EACH DIST ####################

# should we run this part again?
simulate.dist.from.scratch = TRUE

  
vec = c("normal", "expo", "unif2", "t.scaled")
vec2 = c(0.25, 0.01, 0.002)

params = as.data.frame( expand.grid(vec, vec2) )
names(params) = c("dist", "V")

if ( simulate.dist.from.scratch == TRUE ) {
  sims = apply( params,
                MARGIN = 1, 
                FUN = function(row) {
                  d = sim_data( k = 1000,
                                mu = 0.5,
                                V = as.numeric(row[["V"]]),
                                muN = 5,  # tiny sample sizes fine because we're only using the true effects
                                minN = 5,
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
}

# for plotting joy
sims$dist.pretty = "Normal"
sims$dist.pretty[ sims$true.effect.dist == "expo"] = "Exponential"
sims$dist.pretty[ sims$true.effect.dist == "t.scaled"] = "t"
sims$dist.pretty[ sims$true.effect.dist == "unif2"] = "Uniform mixture"
sims$dist.pretty = factor(sims$dist.pretty, 
                          levels = c("Normal",
                                     "Exponential",
                                     "t",
                                     "Uniform mixture"))

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
  xlab( bquote(theta['rep,i']) ) +
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

