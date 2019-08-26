
# avoid saving old copies of objects
#rm(list=ls())

library(dplyr)
library(metRology)
library(ggplot2)

# helper fns
code.dir = "~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)/Other RRR code (git)/Simulation study"
setwd(code.dir)
source("functions_analysis_RRR.R")
source("functions_RRR.R")  # for sim_data

# where to save the simulated data for density plots
results.dist.dir = "~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)/Simulated data to show distributions"
simulate.dist.from.scratch = FALSE

#################### READ IN DATA ####################

# current results (unif2 and t)
# ~~~ replace with the file called "stitched_unif_t"
setwd("~/Desktop")
library(data.table)
s1 = fread("stitched.csv")
s1 = s1[-1,-1]
head(s1)

# previous results (expo and normal)
setwd("~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)/Simulation results/2019-8-20 normal and expo")
s2 = fread("stitched_expo_normal.csv")
# fix stupid names situation
names(s2) = as.character(s2[1,])
s2 = s2[-1,-1]

s1 = as.data.frame(s1)
s2 = as.data.frame(s2)

s = rbind(s1, s2)
dim(s)


# how close are we to being done?
View( s %>% group_by(scen.name) %>% summarise(reps = n()/4) )

# remove failed reps, etc.
s = s %>% filter( !is.na(scen.name) & !is.na(Method) & !is.na(TheoryP))
s = droplevels(s)

# get rid of messed-up rows (e.g., has something wrong for scen.name)
s = s %>% filter(!is.na(as.numeric(scen.name)) )


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

# number of simulations per scenario
#  not including sim failures
s = s %>% group_by(scen.name) %>%
  mutate( sim.reps = n()/4 )


# temp only - check one scenario
temp = s %>% filter( k == 50 & V == 0.25 & minN == 100 & true.effect.dist == "unif2" & TheoryP == .2)
temp %>% group_by(Method) %>%
  filter(!is.na(Cover)) %>%
  summarise(Cover = mean(Cover), n())
# only 40 data points



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

# fix coverage limits
# also plot 90% coverage line


library(ggplot2)

colors=c("orange", "black", "red", "blue")

all_plots_one_dist(true.effect.dist = "expo",
                   bias.max = max(abs(res.all$phatBias)) + 0.01,
                   mse.max = max(res.all$phatMSE) + 0.01,
                   cover.min = .1,
                   results.path = "~/Desktop/Plots for each distribution")

all_plots_one_dist(true.effect.dist = "normal",
                   bias.max = max(abs(res.all$phatBias)) + 0.01,
                   mse.max = max(res.all$phatMSE) + 0.01,
                   cover.min = min(res.all$Cover, na.rm = TRUE) - 0.05,
                   results.path = "~/Desktop/Plots for each distribution")

all_plots_one_dist(true.effect.dist = "unif2",
                   bias.max = max(abs(res.all$phatBias)) + 0.01,
                   mse.max = max(res.all$phatMSE) + 0.01,
                   cover.min = .1,
                   results.path = "~/Desktop/Plots for each distribution")

all_plots_one_dist(true.effect.dist = "t.scaled",
                   bias.max = max(abs(res.all$phatBias)) + 0.01,
                   mse.max = max(res.all$phatMSE) + 0.01,
                   cover.min = min(res.all$Cover, na.rm = TRUE) - 0.05,
                   results.path = "~/Desktop/Plots for each distribution")


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


#################### ** SUMMARIZE RESULTS FOR PAPER ####################

# bm 
##### Inference #####
# filter out the levels of heterogeneity for which we don't recommend our methods
inf.table = res.all %>% filter(V > 0.04) %>%
  filter(Method != "NP ensemble") %>%  # this method doesn't give inference
  group_by(true.effect.dist, Method) %>%
  summarise( Mean.Cover = mean(Cover, na.rm = TRUE),
             Min.Cover = min(Cover, na.rm = TRUE),
             #q5Cover = quantile(Cover, 0.05, na.rm = TRUE),
             Cover.Above.90 = sum(Cover>.9)/length(Cover),
             
             Width = mean(Width, na.rm = TRUE) )

View(inf.table)


# find scenarios where NP sign test does fine
View( res.all %>% filter(V > 0.04) %>%
  filter(Method == "NP sign test") %>%  # this method doesn't give inference
  group_by(true.effect.dist, Method) %>%
  summarise( Mean.Cover = mean(Cover, na.rm = TRUE),
             Min.Cover = min(Cover, na.rm = TRUE),
             #q5Cover = quantile(Cover, 0.05, na.rm = TRUE),
             Cover.Above.90 = sum(Cover>.9)/length(Cover),
             
             Width = mean(Width, na.rm = TRUE) ) )

##### Point Estimates #####
# filter out the levels of heterogeneity for which we don't recommend our methods
( est.table = res.all %>% filter(V > 0.04) %>%
    group_by(true.effect.dist, Method) %>%
    summarise( Mean.Abs.Bias = mean( abs(phatBias) ),
               Max.Abs.Bias = max( abs(phatBias) ),
               
               # Mean.Rel.Bias = max( abs(phatBias) / TheoryP ),
               # Max.Rel.Bias = mean( abs(phatBias) / TheoryP ),
               
               Mean.MSE = mean(phatMSE),
               Max.MSE = max(phatMSE) ) )

View(est.table)



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
# # since t looks just like normal, confirm via Shapiro that 
# #  it really is different
# sims %>% filter(dist.pretty == "Normal") %>%
#   group_by(V) %>% 
#   summarise( shapiro.p = shapiro.test(Mi)$p.value )
# # works :) 

