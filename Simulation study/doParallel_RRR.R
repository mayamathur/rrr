

######### FOR CLUSTER USE #########

# because Sherlock 2.0 restores previous workspace
rm( list = ls() )

# load command line arguments
args = commandArgs(trailingOnly = TRUE)
jobname = args[1]
scen = args[2]  # this will be a letter

# get scen parameters
setwd("/home/groups/manishad/MAM")
scen.params = read.csv( "scen_params.csv" )
p = scen.params[ scen.params$scen.name == scen, ]

print(p)


# simulation reps to run within this job
# this need to match n.reps.in.doParallel in the genSbatch script
sim.reps = 5
boot.reps = 10000


# EDITED FOR C++ ISSUE WITH PACKAGE INSTALLATION
library(crayon, lib.loc = "/home/groups/manishad/Rpackages/")
library(dplyr, lib.loc = "/home/groups/manishad/Rpackages/")
library(foreach, lib.loc = "/home/groups/manishad/Rpackages/")
library(doParallel, lib.loc = "/home/groups/manishad/Rpackages/")
library(boot, lib.loc = "/home/groups/manishad/Rpackages/")
library(metafor, lib.loc = "/home/groups/manishad/Rpackages/")
library(data.table, lib.loc = "/home/groups/manishad/Rpackages/")


# for use in ml load R
# install.packages( c("doParallel", "foreach", "mvtnorm", "StepwiseTest", "matrixcalc"), lib = "/home/groups/manishad/Rpackages/" )

source("functions_MAM.R")

# set the number of cores
registerDoParallel(cores=16)
######### END OF CLUSTER PART #########


######### FOR LOCAL USE #########

rm(list=ls())

# isolate a bad scenario
# lower left of Supplement Panel C, where boot CI and theoretical both had ~85% coverage
k = 10  # running now on Sherlock
mu = 0.5
V = 0.5
minN = 800
muN = NA # placeholder only
sd.w = 1
tail="above"


# sim.reps = 500  # reps to run in this iterate; leave this alone!
# boot.reps = 1000
sim.reps = 100
boot.reps = 1000

# matrix of scenario parameters
scen.params = expand.grid(k, mu, V, q, muN, minN, tail, sd.w)
names(scen.params) = c("k", "mu", "V", "q", "muN", "minN", "tail", "sd.w" )
# avoid naming scenario capital "F" because gets interpreted as FALSE
my.letters = c(letters,
               paste(letters,letters,sep=""),
               paste(letters,letters,letters,sep=""),
               paste(letters,letters,letters,letters,sep=""),
               paste(letters,letters,letters,letters,letters,sep=""),
               paste(letters,letters,letters,letters,letters,letters,sep="")
)

#start.at = which( my.letters == "aaaa" )
start.at = 1
scen.params$scen.name = my.letters[ start.at : ( start.at + nrow(scen.params) - 1 ) ]
( n.scen = length(scen.params[,1]) )

# avoid doing all factorial combinations of muN and minN this way
scen.params$muN = scen.params$minN + 50


# WAS FOR LOCAL USE
library(foreach)
library(doParallel)
library(dplyr)
library(boot)

setwd("~/Dropbox/Personal computer/Independent studies/Meta-analysis metrics (MAM)/Linked to OSF (MAM)/Manuscript code/Simulation study/For Sherlock")
source("functions_MAM.R")

# # ~~~ DEBUGGING: FOR CLUSTER
# # EDITED FOR C++ ISSUE WITH PACKAGE INSTALLATION
# library(crayon, lib.loc = "/home/groups/manishad/Rpackages/")
# library(dplyr, lib.loc = "/home/groups/manishad/Rpackages/")
# library(foreach, lib.loc = "/home/groups/manishad/Rpackages/")
# library(doParallel, lib.loc = "/home/groups/manishad/Rpackages/")
# library(boot, lib.loc = "/home/groups/manishad/Rpackages/")
# library(metafor, lib.loc = "/home/groups/manishad/Rpackages/")
# library(data.table, lib.loc = "/home/groups/manishad/Rpackages/")
# setwd("/home/groups/manishad/MAM")
# source("functions_MAM.R")

# set the number of cores
registerDoParallel(cores=16)


scen = "a"  

######### END OF LOCAL PART #########


########################### THIS SCRIPT COMPLETELY RUNS 1 SIMULATION  ###########################


# j is the number of simulation iterations to run sequentially
# so for j=10, we are generating 10 observed datasets,
# each with reps original datasets
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
                          
                          # method of calculating CI: exponentiate logit or not?
                          Method = c( NA,
                                      NA,
                                      NA), 
                          
                          # CI performance
                          Cover = c( NA,
                                     NA,
                                     NA
                          ), # coverage; vector with length 3
                          
                          Width = c( NA,
                                     NA,
                                     NA )
)

placeholder$scen.name = scen
placeholder = merge( placeholder, scen.params )

setwd("/home/groups/manishad/MAM/sim_results/long")
write.csv( placeholder, paste( "long_results", jobname, ".csv", sep="_" ) )
# this will be overwritten if the rep finished successfully

######### end of writing placeholder file



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

    # true population proportion of studies with ES > q
    # DEBUGGING
    mytry = try( sum( d$Mi > p$q ) / length( d$Mi ) )
    if("try-error" %in% class(mytry)) browser()
    p.above = sum( d$Mi > p$q ) / length( d$Mi )
    
    ##### Compute Parametric Estimators #####
    # fit RE model
    m = rma.uni( yi = d$yi,
                 vi = d$vyi,
                 measure="SMD",
                 knha = TRUE,
                 method = "REML")
    M = m$b
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
    
    
    # fill in new row of summary dataframe with bias, coverage, and CI width for DM and bootstrap
    # dataframe with 3 rows, one for each method
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
                           Method = c( "Logit",
                                       "Original",
                                       "Boot"), 
                           
                           # CI performance
                           Cover = c( covers(expected, ours$lo.expon, ours$hi.expon),
                                      covers(expected, ours$lo, ours$hi),
                                      covers(expected, boot.lo, boot.hi)
                           ), # coverage; vector with length 3
                           
                           Width = c( ours$hi.expon - ours$lo.expon,
                                      ours$hi - ours$lo,
                                      boot.hi - boot.lo )
    )
    
    # add in scenario parameters
    rows$scen.name = scen
    rows = as.data.frame( merge(rows, scen.params) )
    rows
  
}  ### end parallelized loop

} )[3]  # end timer




head(rs)

# time in seconds
rep.time

# COMMENT OUT BELOW PART TO RUN ON CLUSTER
# see results
rs %>% group_by(Method) %>% summarise(coverage = mean(Cover, na.rm=TRUE))
rs %>% group_by(Method) %>%summarise(width = mean(Width, na.rm=TRUE))



########################### WRITE LONG RESULTS  ###########################
setwd("/home/groups/manishad/MAM/sim_results/long")
write.csv( rs, paste( "long_results", jobname, ".csv", sep="_" ) )


# ########################### WRITE SHORT RESULTS ###########################
# # keep only 1 row per simulation rep
# keep.row = rep( c( TRUE, rep(FALSE, boot.reps - 1) ), sim.reps )
# 
# setwd("/home/groups/manishad/multTest/sim_results/short")
# write.csv( results[ keep.row, ], paste( "short_results", jobname, ".csv", sep="_" ) )

