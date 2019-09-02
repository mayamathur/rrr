
# ensemble-bt version

# note: 2019-8-26_stitched_cumulative.csv has all 4 distributions

# are we running locally?
# DOESN'T WORK!
local = FALSE

######################################## FOR CLUSTER USE ######################################## 
#if (local == FALSE) {
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
  library(metRology, lib.loc = "/home/groups/manishad/Rpackages/")
  
  
  # for use in ml load R
  # install.packages( c("metRology"), lib = "/home/groups/manishad/Rpackages/" )
  
  path = "/home/groups/manishad/RRR"
  setwd(path)
  source("functions_RRR.R")
  
  # set the number of cores
  registerDoParallel(cores=16)
  
  ##### Write Blank CSV File #####
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
#}



# ######################################## FOR LOCAL USE ######################################## 
if ( local == TRUE ) {
  rm(list=ls())

  # helper fns
  setwd("~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)/Other RRR code (git)/Simulation study")
  source("functions_RRR.R")

  # isolate a bad scenario
  # row 1, upper panel #3
  ( scen.params = make_scen_params( k = c(5),
                                    mu = 0.5,  # mean of true effects (log-RR)
                                    V = c( 0.01 ),  # variance of true effects
                                    muN = NA, # just a placeholder; to be filled in later
                                    minN = c( 100 ),
                                    sd.w = 1,
                                    tail = "above",
                                    true.effect.dist = "unif2", # "expo", "normal", or "unif2"
                                    TheoryP = c(0.05) ) )
  n.scen = nrow(scen.params)


  # sim.reps = 500  # reps to run in this iterate; leave this alone!
  # boot.reps = 1000
  sim.reps = 2
  boot.reps = 50


  library(foreach)
  library(doParallel)
  library(dplyr)
  library(boot)
  library(purrr)


  # # ~~~ DEBUGGING: FOR CLUSTER
  # # EDITED FOR C++ ISSUE WITH PACKAGE INSTALLATION
  # library(crayon, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(dplyr, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(foreach, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(doParallel, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(boot, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(metafor, lib.loc = "/home/groups/manishad/Rpackages/")
  # library(data.table, lib.loc = "/home/groups/manishad/Rpackages/")
  # setwd("/home/groups/manishad/RRR")
  # source("functions_RRR.R")

  # set the number of cores
  registerDoParallel(cores=16)

  scen = 1
}


########################### THIS SCRIPT COMPLETELY RUNS 1 SIMULATION (LOCALLY) ###########################


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




########################### RUN THE ACTUAL SIMULATION ###########################

# global parameters for all scenarios
CI.level = 0.95

# which methods should we run?
# it always runs parametric 
# should list all of them unless we're re-running an existing scenario with a new method
#methods.to.run = c("Boot", "NP ensemble", "NP sign test")
methods.to.run = c("NP ensemble")

# if running NP sign test, should we bootstrap inference as well?
boot.ens = TRUE  

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
                  sd.w = p$sd.w,
                  true.effect.dist = p$true.effect.dist )

    
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
    #p$TheoryP = 1 - pnorm(p$q, mean=p$mu, sd=sqrt(p$V))
    
    # for checking coverage of tau^2 CI
    CIs = confint(m)
    
    rows =     data.frame( TrueMean = p$mu,
                           EstMean = M,
                           MeanCover = covers( p$mu, summary(m)$ci.lb, summary(m)$ci.ub ),
                           
                           TrueVar = p$V,
                           EstVar = t2,
                           VarCover = covers( p$V, CIs$random["tau^2", "ci.lb"],
                                              CIs$random["tau^2", "ci.ub"] ),
                           
                           #TheoryP = p$TheoryP,  # from Normal quantiles given mu, V
                           TruthP = p.above,   # based on generated data
                           phat = ours$Est,  # our estimator
                           phatBias = ours$Est - p$TheoryP, # phat estimator vs. true proportion above
                           
                           # method of calculating CI: exponentiate logit or not?
                           Method = "Parametric",
                           
                           # CI performance
                           Cover = covers(p$TheoryP, ours$lo, ours$hi),
                           
                           Width = ours$hi - ours$lo,
                           
                           Note = NA )
    
    
    ##### Bootstrap for Parametric #####
    if ( "Boot" %in% methods.to.run ) {
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
      
      # boot for parametric
      rows = add_row( rows,
                      TrueMean = p$mu,
                      EstMean = M, 
                      MeanCover = covers( p$mu, summary(m)$ci.lb, summary(m)$ci.ub ),
                      
                      TrueVar = p$V,
                      EstVar = t2,
                      VarCover = covers( p$V, CIs$random["tau^2", "ci.lb"],
                                         CIs$random["tau^2", "ci.ub"] ),
                      
                      #TheoryP = p$TheoryP,  # from Normal quantiles given mu, V
                      TruthP = p.above,   # based on generated data
                      phat = boot.median,  # note that this is the median of the bootstrap iterates
                      phatBias = boot.median - p$TheoryP, 
                      
                      # method of calculating CI: exponentiate logit or not?
                      Method = "Boot",
                      
                      # CI performance
                      Cover = covers(p$TheoryP, boot.lo, boot.hi),
                      
                      Width = boot.hi - boot.lo,
                      
                      Note = NA)
    }
    
    ##### Nonparametric Sign Test (Rui Wang) #####

    if ("NP sign test" %in% methods.to.run) {
      Phat.NP = prop_stronger_np( q = p$q,
                                  yi = d$yi,
                                  vi = d$vyi,
                                  tail = "above",
                                  R = 2000,
                                  return.vectors = FALSE)
      
      rows = add_row( rows,
                      TrueMean = p$mu,
                      EstMean = NA,
                      MeanCover = NA,
                      
                      TrueVar = NA,
                      EstVar = NA,
                      VarCover = NA,
                      
                      #TheoryP = p$TheoryP,  # from Normal quantiles given mu, V
                      TruthP = p.above,   # based on generated data
                      phat = Phat.NP$Est,  # nonparametric estimator
                      phatBias = Phat.NP$Est - p$TheoryP, # phat estimator vs. true proportion above
                      
                      # method of calculating CI: exponentiate logit or not?
                      Method = "NP sign test",
                      
                      # CI performance
                      Cover = covers(p$TheoryP, Phat.NP$lo, Phat.NP$hi),
                      
                      Width = Phat.NP$hi - Phat.NP$lo,
                      
                      Note = NA)
    }

  

    ##### Get Ensemble Phat and CI (Wang ensemble) #####
    #write.csv("nothing", "flag1.csv")
    # this method has the additional option to compute only the point estimate
    #  but not bootstrap a CI
    Note = "BCa"
    if ("NP ensemble" %in% methods.to.run) {
      ens = my_ens( yi = d$yi, 
                    sei = sqrt(d$vyi) )
      if ( p$tail == "above" ) Phat.NP.ens = sum(ens > c(p$q)) / length(ens)
      if ( p$tail == "below" ) Phat.NP.ens = sum(ens < c(p$q)) / length(ens)
      
      if ( boot.ens == TRUE ) {
        tryCatch({
          boot.res.ens = boot( data = d, 
                               parallel = "multicore",
                               R = boot.reps, 
                               statistic = function(original, indices) {
                                 
                                 b = original[indices,]
                                 
                                 ens.b = my_ens( yi = b$yi, 
                                                 sei = sqrt(b$vyi) )
                                 if ( p$tail == "above" ) return( sum(ens.b > c(p$q)) / length(ens.b) )
                                 if ( p$tail == "below" ) return( sum(ens.b < c(p$q)) / length(ens.b) )
                               }
                                )
          
          bootCIs.ens = boot.ci(boot.res.ens, type="bca")
          boot.lo.ens = bootCIs.ens$bca[4]
          boot.hi.ens = bootCIs.ens$bca[5]
          
        }, error = function(err){
          # browser()
          # # if BCa fails, use percentiles instead
          # boot.lo.ens <<- as.numeric( quantile( boot.res.ens$t, 0.025 ) )
          # boot.hi.ens <<- as.numeric( quantile( boot.res.ens$t, 0.975 ) )
          # Note <<- "Percentiles"
          
          boot.lo.ens <<- NA
          boot.hi.ens <<- NA
          Note <<- err$message
        })
        
      } else {
        boot.lo.ens = NA
        boot.hi.ens = NA
        Note = NA
      }
      
      # NP ensemble
      rows = add_row( rows,
                      TrueMean = p$mu,
                      EstMean = NA,
                      MeanCover = NA,
                      
                      TrueVar = NA,
                      EstVar = NA,
                      VarCover = NA,
                      
                      #TheoryP = p$TheoryP,  # from Normal quantiles given mu, V
                      TruthP = p.above,   # based on generated data
                      phat = Phat.NP.ens,  # nonparametric estimator
                      phatBias = Phat.NP.ens - p$TheoryP, # phat estimator vs. true proportion above
                      
                      # method of calculating CI: exponentiate logit or not?
                      Method = "NP ensemble",
                      
                      # CI performance
                      Cover = covers(p$TheoryP, boot.lo.ens, boot.hi.ens),
                      
                      Width = boot.hi.ens - boot.lo.ens,
                      
                      Note = Note)
    }
   

      # add in scenario parameters
      rows$scen.name = scen
      rows = as.data.frame( merge(rows, scen.params,
                                  by = "scen.name") )
      rows

}  ### end parallelized loop

} )[3]  # end timer


head(rs)

rs %>% filter(Method == "NP ensemble") %>%
group_by(Note) %>% 
  summarise( cover = mean(Cover),
             n = n() )


# time in seconds
rep.time

# if ( local == TRUE ) {
#   # ~~ COMMENT OUT BELOW PART TO RUN ON CLUSTER
#   # see results
#   rs %>% group_by(Method) %>%
#     summarise(coverage = mean(Cover, na.rm=TRUE),
#               prop.na = mean(is.na(Cover)),
#               n())
# 
#   rs %>% group_by(Method) %>%summarise(width = mean(Width, na.rm=TRUE))
#   rs %>% group_by(Method) %>% summarise(phat = mean(phat, na.rm=TRUE))
#    # bias
# }


########################### WRITE LONG RESULTS  ###########################
#if ( local == FALSE ) {
  setwd("/home/groups/manishad/RRR/sim_results/long")
  write.csv( rs, paste( "long_results", jobname, ".csv", sep="_" ) )
#}