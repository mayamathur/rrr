
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
# below, x 2 because of POrig.Method
nrow(s) / (n.scen * reps.per.scen * 2)

# reasons for rep failure
prop.table( table( s$Note, useNA = "ifany" ) ) 

# finished sim reps, missing data, and rep.time by scenario
# group by POrig.Method because each iterate produces two rows, one for each POrig.Method
sim.summ = as.data.frame( s %>% group_by(scen.name, POrig.Method) %>%
                 summarise( sim.reps = n(),
                            k = k[1],
                            prop.failed = round( mean(!is.na(Note)), 2 ),
                            mean.time = round( mean(foreach.time), 0 ),
                            max.time = round( max(foreach.time), 0 ) ) ) %>%
  arrange(sim.reps)

summary(sim.summ$sim.reps)
# ~~~ 32% of scenarios have fewer than the desired number of reps - why?
mean(sim.summ$sim.reps<1024)

# ** run more iterates for these
need.more = sim.summ$scen.name[ sim.summ$sim.reps < 1024 ]

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
shapes = c(69, 78, 84, 85)  # letters for distributions
colors = c("black", "orange")
library(ggplot2)
ggplot( data = agg1 %>% filter(delta == 0 & POrig.Method == "reml"),
        aes(x = k,
            y = Reject.mn,
            color = as.factor(minN),
            shape = as.factor(N.orig)
            ) ) +
  geom_point(size=2) +
  geom_line() +
  geom_hline(yintercept = 0.05,
             color = "red",
             lty = 2) +
  scale_shape_manual(values = c(1,2,3,4),
                     name = "Sample size in original") +
  scale_color_manual(values = colors,
                     name = "Sample size in each replication") +
  scale_y_continuous( limits = c(0,.13),
                      breaks = seq(0,.13,.01)) +
  ylab("Mean rejection rate") +
  xlab("Number of replications") + 
  #facet_wrap(~ dist.pretty + V.pretty, nrow=4) +
  facet_grid(dist.pretty ~ V.pretty) +
  theme_bw()
# 11 x 11

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
#shapes = c(69, 78, 84, 85)

# break in two for legibility (2 distributions each)
colors = c("black", "orange")
library(ggplot2)
ggplot( data = agg1 %>% filter(delta > 0 & POrig.Method == "reml" & dist.pretty %in% c("Normal", "Exponential")),
        aes(x = k,
            y = Reject.mn,
            color = as.factor(minN),
            shape = as.factor(N.orig)
        ) ) +
  geom_point(size=2) +
  geom_line() +
  geom_hline(yintercept = 0.05,
             color = "red",
             lty = 2) +
  scale_shape_manual(values = c(1,2,3,4),
                     name = "Sample size in original") +
  scale_color_manual(values = colors,
                     name = "Sample size in each replication") +
  # scale_y_continuous( limits = c(0,.13),
  #                     breaks = seq(0,.13,.01)) +
  ylab("Mean power") +
  xlab("Number of replications") + 
  #facet_wrap(~ dist.pretty + V.pretty, nrow=4) +
  facet_grid(dist.pretty + delta ~ V.pretty) +
  theme_bw()

ggplot( data = agg1 %>% filter(delta > 0 & POrig.Method == "reml" & dist.pretty %in% c("t", "Uniform mixture")),
        aes(x = k,
            y = Reject.mn,
            color = as.factor(minN),
            shape = as.factor(N.orig)
        ) ) +
  geom_point(size=2) +
  geom_line() +
  geom_hline(yintercept = 0.05,
             color = "red",
             lty = 2) +
  scale_shape_manual(values = c(1,2,3,4),
                     name = "Sample size in original") +
  scale_color_manual(values = colors,
                     name = "Sample size in each replication") +
  # scale_y_continuous( limits = c(0,.13),
  #                     breaks = seq(0,.13,.01)) +
  ylab("Mean power") +
  xlab("Number of replications") + 
  #facet_wrap(~ dist.pretty + V.pretty, nrow=4) +
  facet_grid(dist.pretty + delta ~ V.pretty) +
  theme_bw()



#################### CANDIDATE RULE OF THUMB FOR TYPE I ERROR ####################

# candidate rule of thumb: ?
# also pretty robust to other distributions, surprisingly
# this rule seems to yield a max Type I error of 6%
agg1 %>% filter( delta == 0 &
                  true.effect.dist == "normal" &
                   POrig.Method == "reml" &
                   k >= 10 &
                   N.tot >= 5000 ) %>%
  mutate(N.tot = k*minN) %>%
  #group_by(k, minN, N.orig, POrig.Method) %>%
  summarise( n.scen = n(),
            Reject.max = max(Reject.mn), # max of scenarios' mean rejection rates
             Reject.mn2 = mean(Reject.mn),
            Perc.Okay = mean(Reject.mn < .06) )


# # predictors of having unacceptable Type I error
# # excluding N.orig only temporarily since it's not varying yet
# covars = varying[ !varying %in% c("true.effect.dist", "delta", "N.orig") ]
# 
# agg1$N.tot = agg1$k * agg1$minN
# 
# #( string = paste( "Reject.mn > 0.05 ~ ", paste( covars, collapse="+" ), sep = "") )
# m = glm( Reject.mn > 0.05 ~ N.tot * V + POrig.Method,
#      data = agg1 %>% filter(true.effect.dist == "normal" & delta == 0) )
# summary(m)
# 
# m = lrm( as.formula(string),
#          data = agg1[ agg1$true.effect.dist == "normal" & agg1$delta == 0, ] )
# 



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
plot_group2(.y.name = "Phat.bias.mn",
           .ylab = "Bias",
           .dists = c("Normal", "Exponential"),
           .data = agg2,
           .limits = c(-0.1, .4))
plot_group2(.y.name = "Phat.bias.mn",
            .ylab = "Bias",
            .dists = c("t", "Uniform mixture"),
            .data = agg2,
            .limits = c(-0.1, .4))

# # absolute bias
# plot_group2(.y.name = "Phat.absbias.mn",
#            .ylab = "Absolute bias",
#            .data = agg2,
#            .limits = c(-0.1, .4))


##### **Plot 4: RMSE #####
plot_group2(.y.name = "RMSE",
           .ylab = "RMSE",
           .data = agg2,
           .dists = c("Normal", "Exponential"),
           .limits = c(0, .5))
plot_group2(.y.name = "RMSE",
            .ylab = "RMSE",
            .data = agg2,
            .dists = c("t", "Uniform mixture"),
            .limits = c(0, .5))


#################### ** TABLES FOR PAPER: HIGH-LEVEL SUMMARY OF INFERENCE AND POINT ESTIMATE RESULTS ####################

#####  ***Porig Summary #####

# just focus on one value for sample sizes for simplicity
Porig.table = agg1 %>%
  filter( k >= 10, minN == 300, N.orig == 50 ) %>%
  group_by(dist.pretty, V, delta) %>%
  summarise( Reject = round( mean(Reject.mn), 3 ),
             Reject.max = round( max(Reject.mn), 3 ) ) 
View(Porig.table)

#####  ***Phat Summary #####
Phat.table = agg2 %>%
  filter(k >= 10) %>%
  group_by(dist.pretty, V, minN) %>%
  summarise( 
    Mean.Bias = round( mean(Phat.bias.mn), digits = 3),
    #Mean.Abs.Bias = round( mean(Phat.absbias.mn), digits = 3),  # abs value, then mean
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
