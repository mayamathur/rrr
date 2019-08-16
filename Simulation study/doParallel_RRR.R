
# Simulations to run for the nonparametric Phat letter:
# 1.) Does NP - sign test inference do better than boot for small k and 
#  the worst scenarios in MAM appendix?

# 2.) Are there ever scenarios where boot outperforms NP - sign test in 
#  terms of coverage and width?

# 3.) For NP point estimate with small k or non-normal true effects, what is best:
#  NP - sign test estimate or NP - ensemble estimate?

# When results come back, remember I halved the boot.reps from 10,000 to 5,000
# so check if boot performance is similar to in MAM supplement.

# Bookmark: Make sure we're writing results for NP ensemble. 

######### FOR CLUSTER USE #########

# because Sherlock 2.0 restores previous workspace
rm( list = ls() )

# load command line arguments
args = commandArgs(trailingOnly = TRUE)
jobname = args[1]
scen = args[2]  # this will be a letter

# get scen parameters
setwd("/home/groups/manishad/RRR")
scen.params = read.csv( "scen_params.csv" )
p = scen.params[ scen.params$scen.name == scen, ]

print(p)


# simulation reps to run within this job
# this need to match n.reps.in.doParallel in the genSbatch script
# sim.reps = 2
# boot.reps = 100

# # real versions
sim.reps = 5
# was 10,000 in MAM paper
boot.reps = 5000



# EDITED FOR C++ ISSUE WITH PACKAGE INSTALLATION
library(crayon, lib.loc = "/home/groups/manishad/Rpackages/")
library(dplyr, lib.loc = "/home/groups/manishad/Rpackages/")
library(foreach, lib.loc = "/home/groups/manishad/Rpackages/")
library(doParallel, lib.loc = "/home/groups/manishad/Rpackages/")
library(boot, lib.loc = "/home/groups/manishad/Rpackages/")
library(metafor, lib.loc = "/home/groups/manishad/Rpackages/")
library(data.table, lib.loc = "/home/groups/manishad/Rpackages/")
library(purrr, lib.loc = "/home/groups/manishad/Rpackages/")


# for use in ml load R
# install.packages( c("doParallel", "foreach", "mvtnorm", "StepwiseTest", "matrixcalc"), lib = "/home/groups/manishad/Rpackages/" )

path = "/home/groups/manishad/RRR"
setwd(path)
source("functions_RRR.R")

# set the number of cores
registerDoParallel(cores=16)
######### END OF CLUSTER PART #########


# ######### FOR LOCAL USE #########
# 
# rm(list=ls())
# 
# # # isolate a bad scenario
# # # lower left of Supplement Panel C, where boot CI and theoretical both had ~85% coverage
# # k = 10  # running now on Sherlock
# # mu = 0.5
# # V = 0.5
# # minN = 800
# # muN = NA # placeholder only
# # sd.w = 1
# # tail="above"
# # true.effect.dist = "normal"
# 
# # full set of scenarios
# k = c(10)
# mu = 0.5  # mean of true effects (log-RR)
# V = c( 0.5^2 )  # variance of true effects
# muN = NA # just a placeholder; to be filled in later
# minN = c( 800 )
# sd.w = 1
# tail = "above"
# true.effect.dist = "expo"
# 
# # only running P = 0.20 to supplement previous sim results
# # set q to be the quantiles such that TheoryP is 0.2 for every V
# TheoryP = c(0.1)
# 
# qmat = matrix( NA, nrow = length(V), ncol = length(TheoryP) )
# 
# for (i in 1:length(TheoryP) ) {
#   new.qs = qnorm( p = 1 - TheoryP[i],
#                   mean = mu,
#                   sd = sqrt(V) )
#   qmat[,i] = new.qs
# }
# 
# q = unique( as.vector( qmat ))
# 
# # matrix of scenario parameters
# scen.params = expand.grid(k, mu, V, q, muN, minN, tail, sd.w, true.effect.dist)
# names(scen.params) = c("k", "mu", "V", "q", "muN", "minN", "tail", "sd.w", "true.effect.dist" )
# 
# 
# # only keep combos of V and q that lead to the TheoryPs we want
# TheoryP2 = 1 - pnorm( q = scen.params$q,
#                       mean = scen.params$mu,
#                       sd = sqrt(scen.params$V) )
# scen.params = scen.params[ round(TheoryP2,3) %in% TheoryP, ]
# table(scen.params$q, scen.params$V ); qmat  # each
# 
# 
# #start.at = which( my.letters == "aaaa" )
# start.at = 1
# scen.params$scen.name = start.at : ( start.at + nrow(scen.params) - 1 )
# ( n.scen = length(scen.params[,1]) )
# 
# # avoid doing all factorial combinations of muN and minN this way
# scen.params$muN = scen.params$minN + 50
# 
# 
# 
# 
# # sim.reps = 500  # reps to run in this iterate; leave this alone!
# # boot.reps = 1000
# sim.reps = 2
# boot.reps = 200
# 
# 
# library(foreach)
# library(doParallel)
# library(dplyr)
# library(boot)
# library(purrr)
# 
# # read in helper fns
# setwd("~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)/Other RRR code (git)/Simulation study")
# source("functions_RRR.R")
# 
# # # ~~~ DEBUGGING: FOR CLUSTER
# # # EDITED FOR C++ ISSUE WITH PACKAGE INSTALLATION
# # library(crayon, lib.loc = "/home/groups/manishad/Rpackages/")
# # library(dplyr, lib.loc = "/home/groups/manishad/Rpackages/")
# # library(foreach, lib.loc = "/home/groups/manishad/Rpackages/")
# # library(doParallel, lib.loc = "/home/groups/manishad/Rpackages/")
# # library(boot, lib.loc = "/home/groups/manishad/Rpackages/")
# # library(metafor, lib.loc = "/home/groups/manishad/Rpackages/")
# # library(data.table, lib.loc = "/home/groups/manishad/Rpackages/")
# # setwd("/home/groups/manishad/RRR")
# # source("functions_RRR.R")
# 
# # set the number of cores
# registerDoParallel(cores=16)
# 
# 
# scen = 1
# 
# ######### END OF LOCAL PART #########


########################### THIS SCRIPT COMPLETELY RUNS 1 SIMULATION  ###########################


# j is the number of simulation iterations to run sequentially
# so for j=10, we are generating 10 observed datasets,
# each with reps  datasets
#  and 500 bootstrap iterates for each

# this for-loop is supposed to receive a single ROW of parameters

# runs sim.reps simulations, each with reps (in scen.params) repetitions
# each scenario has a parameter called "reps"
# for ( j in 1:sim.reps ) {
#   
#   rs = power(scen=scen)
#   rs$scen.name = scen
#   new.row = merge(scen.params, rs)
#   
#   # remove redundant parameters column
#   #new.rows = new.rows[ , !names(new.rows) == "scen.name" ]
#   
#   # add row to output file
#   if ( j == 1 ) results = new.row
#   if ( j > 1 ) results = rbind( results, new.row )
#   # results should have boot.reps rows per "j" in the for-loop
#   
# }  # end loop over j simulation reps




########################### WRITE BLANK CSV FILE  ###########################


# this records that the rep started in case there is a problem with the bootstrapping
placeholder = data.frame( TrueMean = NA,
                          EstMean = NA,
                          MeanCover = NA,
                          
                          TrueVar = NA,
                          EstVar = NA,
                          VarCover = NA,
                          
                          TheoryP = NA,  # from Normal quantiles given mu, V
                          TruthP = NA,   # based on generated data
                          phat = NA,  # our estimator
                          phatBias = NA, # phat estimator vs. true proportion above
                          
                          Method = NA, 
                          
                          # CI performance
                          Cover = NA,
                          
                          Width = NA, 
                          
                          Note = "Sim failure" )


placeholder$scen.name = scen
placeholder = merge( placeholder, scen.params )

setwd("/home/groups/manishad/RRR/sim_results/long")
write.csv( placeholder, paste( "long_results", jobname, ".csv", sep="_" ) )
# this will be overwritten if the rep finished successfully



########################### RUN THE ACTUAL SIMULATION ###########################

CI.level = 0.95

rep.time = system.time({
rs = foreach( i = 1:sim.reps, .combine=rbind ) %dopar% {
  
  # extract simulation params for this scenario (row)
  # exclude the column with the scenario name itself (col) 
  p = scen.params[ scen.params$scen.name == scen, names(scen.params) != "scen.name"]
  

    ##### Simulate Dataset #####
    d = sim_data( k = p$k, 
                  mu = p$mu, 
                  V = p$V,
                  muN = p$muN, 
                  minN = p$minN,
                  sd.w = p$sd.w )

    
    # # DEBUGGING
    # mytry = try( sum( d$Mi > p$q ) / length( d$Mi ) )
    # if("try-error" %in% class(mytry)) browser()
    
    # true population proportion of studies with ES > q
    p.above = sum( d$Mi > p$q ) / length( d$Mi )
    
    ##### Compute Parametric Estimators #####
    # fit RE model
    m = rma.uni( yi = d$yi,
                 vi = d$vyi,
                 measure="SMD",
                 knha = TRUE,
                 method = "REML")
    M = as.numeric(m$b)
    t2 = m$tau2
    se.M = sqrt( as.numeric(m$vb) )
    se.t2 = m$se.tau2
    
    # suppress warnings about inability to do inference
    ours = suppressWarnings( prop_stronger( q = p$q,
                                            M = M,
                                            t2 = t2,
                                            se.M = se.M,
                                            se.t2 = se.t2,
                                            CI.level = CI.level,
                                            tail = p$tail ) )
    
    
    # theoretical expectation for proportion of studies with ES > q
    expected = 1 - pnorm(p$q, mean=p$mu, sd=sqrt(p$V))
    
    # for checking coverage of tau^2 CI
    CIs = confint(m)
    
    
    ##### Get Bootstrapped CI #####
    boot.res = boot( data = d, 
                     parallel = "multicore",
                     R = boot.reps, 
                     statistic = function(original, indices) {
                       
                       b = original[indices,]
                       
                       mb = rma.uni( yi = b$yi,
                                     vi = b$vyi,
                                     measure="SMD",
                                     knha = TRUE,
                                     method = "REML")
                       Mb = mb$b
                       t2b = mb$tau2
                       
                       suppressWarnings( prop_stronger( q = p$q,
                                                        M = Mb,
                                                        t2 = t2b,
                                                        CI.level = CI.level,
                                                        tail = p$tail )$Est )
                     }
    )
  
    
    #bootCIs = boot.ci(boot.res, type="perc")
    bootCIs = boot.ci(boot.res, type="bca")
    boot.lo = bootCIs$bca[4]
    boot.hi = bootCIs$bca[5]
    boot.median = median(boot.res$t)  # median Phat in bootstrap iterates
    
    ##### Get Nonparametric Phat and CI (Rui Wang) #####
    Phat.NP = prop_stronger_np( q = p$q,
                                  yi = d$yi,
                                  vi = d$vyi,
                                  tail = "above",
                                  R = 2000,
                                  return.vectors = FALSE)
    
    ##### Get Nonparametric Phat and CI (Wang ensemble) #####
    #write.csv("nothing", "flag1.csv")
    
    ens = my_ens( yi = d$yi, 
                  sei = sqrt(d$vyi) )
    if ( p$tail == "above" ) Phat.NP.ens = sum(ens > c(p$q)) / length(ens)
    if ( p$tail == "below" ) Phat.NP.ens = sum(ens < c(p$q)) / length(ens)
    
    #write.csv(Phat.NP.ens, "Phat_NP_ens.csv")
    
 
      rows =     data.frame( TrueMean = p$mu,
                             EstMean = M,
                             MeanCover = covers( p$mu, summary(m)$ci.lb, summary(m)$ci.ub ),

                             TrueVar = p$V,
                             EstVar = t2,
                             VarCover = covers( p$V, CIs$random["tau^2", "ci.lb"],
                                                CIs$random["tau^2", "ci.ub"] ),

                             TheoryP = expected,  # from Normal quantiles given mu, V
                             TruthP = p.above,   # based on generated data
                             phat = ours$Est,  # our estimator
                             phatBias = ours$Est - expected, # phat estimator vs. true proportion above

                             # method of calculating CI: exponentiate logit or not?
                             Method = "Parametric",

                             # CI performance
                             Cover = covers(expected, ours$lo, ours$hi),

                             Width = ours$hi - ours$lo,

                             Note = NA )
      
      rows = add_row( rows,
                      TrueMean = p$mu,
                      EstMean = M, 
                      MeanCover = covers( p$mu, summary(m)$ci.lb, summary(m)$ci.ub ),

                      TrueVar = p$V,
                      EstVar = t2,
                      VarCover = covers( p$V, CIs$random["tau^2", "ci.lb"],
                                         CIs$random["tau^2", "ci.ub"] ),

                      TheoryP = expected,  # from Normal quantiles given mu, V
                      TruthP = p.above,   # based on generated data
                      phat = boot.median,  # note that this is the median of the bootstrap iterates
                      phatBias = boot.median - expected, 

                      # method of calculating CI: exponentiate logit or not?
                      Method = "Boot",

                      # CI performance
                      Cover = covers(expected, boot.lo, boot.hi),

                      Width = boot.hi - boot.lo,

                      Note = NA)

      rows = add_row( rows,
                      TrueMean = p$mu,
                      EstMean = NA,
                      MeanCover = NA,

                      TrueVar = NA,
                      EstVar = NA,
                      VarCover = NA,

                      TheoryP = expected,  # from Normal quantiles given mu, V
                      TruthP = p.above,   # based on generated data
                      phat = Phat.NP$Est,  # nonparametric estimator
                      phatBias = Phat.NP$Est - expected, # phat estimator vs. true proportion above

                      # method of calculating CI: exponentiate logit or not?
                      Method = "NP sign test",

                      # CI performance
                      Cover = covers(expected, Phat.NP$lo, Phat.NP$hi),

                      Width = Phat.NP$hi - Phat.NP$lo,

                      Note = NA)
      
      rows = add_row( rows,
                      TrueMean = p$mu,
                      EstMean = NA,
                      MeanCover = NA,

                      TrueVar = NA,
                      EstVar = NA,
                      VarCover = NA,

                      TheoryP = expected,  # from Normal quantiles given mu, V
                      TruthP = p.above,   # based on generated data
                      phat = Phat.NP.ens,  # nonparametric estimator
                      phatBias = Phat.NP.ens - expected, # phat estimator vs. true proportion above

                      # method of calculating CI: exponentiate logit or not?
                      Method = "NP ensemble",

                      # CI performance
                      Cover = NA,

                      Width = NA,

                      Note = NA)


      # add in scenario parameters
      rows$scen.name = scen
      rows = as.data.frame( merge(rows, scen.params) )
      rows

}  ### end parallelized loop

} )[3]  # end timer



head(rs)

# time in seconds
rep.time

# # ~~ COMMENT OUT BELOW PART TO RUN ON CLUSTER
# # see results
# rs %>% group_by(Method) %>% summarise(coverage = mean(Cover, na.rm=TRUE))
# rs %>% group_by(Method) %>%summarise(width = mean(Width, na.rm=TRUE))
# rs %>% group_by(Method) %>% summarise(bias = mean(phat, na.rm=TRUE))
# # bias




########################### WRITE LONG RESULTS  ###########################
setwd("/home/groups/manishad/RRR/sim_results/long")
write.csv( rs, paste( "long_results", jobname, ".csv", sep="_" ) )


# ########################### WRITE SHORT RESULTS ###########################
# # keep only 1 row per simulation rep
# keep.row = rep( c( TRUE, rep(FALSE, boot.reps - 1) ), sim.reps )
# 
# setwd("/home/groups/manishad/multTest/sim_results/short")
# write.csv( results[ keep.row, ], paste( "short_results", jobname, ".csv", sep="_" ) )

