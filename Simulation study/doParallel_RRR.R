# Bookmark: Maybe try V between 0.01 and 0.25?
# Maybe try to figure out if we can make rule of thumb based on I^2 instead of V?



# because Sherlock 2.0 restores previous workspace
rm( list = ls() )

# are we running locally?
run.local = FALSE

######################################## FOR CLUSTER USE ######################################## 
if (run.local == FALSE) {

  
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
  sim.reps = 512*2  # should be divisible by 16 since we parallelize in chunks of 16
  # was 10,000 in MAM paper
  boot.reps = 2000 
  
  n.cores = 16 
  n.loops = sim.reps / n.cores  # how many for-loops, each parallelized over 16 cores, should we run?

  
  # EDITED FOR C++ ISSUE WITH PACKAGE INSTALLATION
  library(crayon, lib.loc = "/home/groups/manishad/Rpackages/")
  library(vctrs, lib.loc = "/home/groups/manishad/Rpackages/")
  library(pillar, lib.loc = "/home/groups/manishad/Rpackages/")
  library(MetaUtility, lib.loc = "/home/groups/manishad/Rpackages/")
  library(dplyr, lib.loc = "/home/groups/manishad/Rpackages/")
  library(foreach, lib.loc = "/home/groups/manishad/Rpackages/")
  library(doParallel, lib.loc = "/home/groups/manishad/Rpackages/")
  library(boot, lib.loc = "/home/groups/manishad/Rpackages/")
  library(metafor, lib.loc = "/home/groups/manishad/Rpackages/")
  library(data.table, lib.loc = "/home/groups/manishad/Rpackages/")
  library(purrr, lib.loc = "/home/groups/manishad/Rpackages/")
  library(metRology, lib.loc = "/home/groups/manishad/Rpackages/")
  library(fansi, lib.loc = "/home/groups/manishad/Rpackages/")
  library(Replicate, lib.loc = "/home/groups/manishad/Rpackages/")
  library(robumeta, lib.loc = "/home/groups/manishad/Rpackages/")
  
  # for use in ml load R
  # install.packages( c("Replicate"), lib = "/home/groups/manishad/Rpackages/" )
  
  path = "/home/groups/manishad/RRR"
  setwd(path)
  source("functions_RRR.R")
  
  # set the number of cores
  registerDoParallel(cores=16)
  
  # ##### Write Blank CSV File #####
  #   # this records that the rep started in case there is a problem with the bootstrapping
  #   placeholder = data.frame( TrueMean = NA,
  #                             EstMean = NA,
  #                             MeanCover = NA,
  #                             
  #                             TrueVar = NA,
  #                             EstVar = NA,
  #                             VarCover = NA,
  #                             
  #                             TheoryP = NA,  # from Normal quantiles given mu, V
  #                             TruthP = NA,   # based on generated data
  #                             phat = NA,  # our estimator
  #                             phatBias = NA, # phat estimator vs. true proportion above
  #                             
  #                             Method = NA, 
  #                             
  #                             # CI performance
  #                             Cover = NA,
  #                             
  #                             Width = NA, 
  #                             
  #                             Note = "Sim failure",
  #                             rep.time = NA)
  # 
  # 
  # placeholder$scen.name = scen
  # placeholder = merge( placeholder, scen.params, by = "scen.name" )
  # 
  # setwd("/home/groups/manishad/RRR/sim_results/long")
  # write.csv( placeholder, paste( "long_results", jobname, ".csv", sep="_" ) )
  # # this will be overwritten if the rep finished successfully
}



######################################## FOR LOCAL USE ########################################
if ( run.local == TRUE ) {
  rm(list=ls())

  library(MetaUtility)
  library(crayon)
  library(dplyr)
  library(foreach)
  library(doParallel)
  library(boot)
  library(metafor)
  library(data.table)
  library(purrr)
  library(metRology)
  library(fansi)
  library(Replicate)
  #library(robumeta)

  # helper fns
  setwd("~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)/Other RRR code (git)/Simulation study")
  source("functions_RRR.R")

  # # this is the scenario that was previously evil
  # ( scen.params = make_scen_params( k = c(25),
  #                                   mu = 0.5,  # mean of true effects
  #                                   V = c(.25 ),  # variance of true effects
  #                                   muN = 150, # just a placeholder; to be filled in later by adding 50 to minN
  #                                   minN = c(100),
  #                                   sd.w = c(1),
  #                                   tail = "above",
  #                                   true.effect.dist = c("normal"), # # "expo", "normal", "unif2", "t.scaled"
  #                                   TheoryP = c(0.05),
  # 
  #                                   # for generating the single original study
  #                                   delta = c(0),  # difference between mu for replications and for original (0 for H0); can be interpreted as SMD
  #                                   N.orig = c(200),  # sample size of original study
  # 
  #                                   start.at = 1 ) )
  
  # this is the scenario that was previously evil
  ( scen.params = make_scen_params( k = c(15),
                                    mu = 0.5,  # mean of true effects
                                    V = c(.25 ),  # variance of true effects
                                    muN = 300, # just a placeholder; to be filled in later by adding 50 to minN
                                    minN = c(300),
                                    sd.w = c(1),
                                    tail = "above",
                                    true.effect.dist = c("normal"), # # "expo", "normal", "unif2", "t.scaled"
                                    TheoryP = c(.5),
                                    
                                    # for generating the single original study
                                    delta = c(0),  # difference between mu for replications and for original (0 for H0); can be interpreted as SMD
                                    N.orig = c(200),  # sample size of original study
                                    
                                    start.at = 1 ) )
  
  ( n.scen = nrow(scen.params) )


  # sim.reps = 500  # reps to run in this iterate; leave this alone!
  # boot.reps = 1000
  n.cores = 8  
  boot.reps = 20
  n.loops = 60  # don't change when running locally


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


########################### RUN THE ACTUAL SIMULATION ###########################

# global parameters for all scenarios
CI.level = 0.95


for( l in 1:n.loops ) {
  
  rep.time = system.time({
    # parallelize so that all 16 cores are being used, but no more  
    
    # debugging version
    #for ( j in 1:200) {
    rs = foreach( i = 1:n.cores, .combine=rbind ) %dopar% {
      
      # extract simulation params for this scenario (row)
      # exclude the column with the scenario name itself (col) 
      p = scen.params[ scen.params$scen.name == scen, names(scen.params) != "scen.name"]
      
      
      ##### Simulate Replication Studies #####
      d = sim_data( k = p$k, 
                    mu = p$mu, 
                    V = p$V,
                    muN = p$muN, 
                    minN = p$minN,
                    sd.w = p$sd.w,
                    true.effect.dist = p$true.effect.dist )
      
      # fit basic meta-analysis to get I^2
      # ~~~ just added KNHA, which will affect the SE
      meta0 = rma.uni( yi = d$yi,
                       vi = d$vyi,
                       knha = TRUE )
      
      ##### Simulate Single Original Study #####
      dO = sim_one_study(  mu = p$mu + p$delta, 
                           V = p$V,
                           muN = p$N.orig, 
                           minN = p$N.orig,
                           sd.w = p$sd.w,
                           true.effect.dist = p$true.effect.dist )  # same distribution type as replications, just shifted
      
      
      
      # # DEBUGGING
      # mytry = try( sum( d$Mi > p$q ) / length( d$Mi ) )
      # if("try-error" %in% class(mytry)) browser()
      
      # true population proportion of studies with ES > q
      p.above = sum( d$Mi > p$q ) / length( d$Mi )
      
      
      ##### Get Ensemble Phat and CI (Wang ensemble) #####
      #write.csv("nothing", "flag1.csv")
      # this method has the additional option to compute only the point estimate
      #  but not bootstrap a CI
      Note = NA
      
      # Phat.NP.ens = MetaUtility::prop_stronger( q = p$q, 
      #                                           tail = "above",
      #                                           estimate.method = "calibrated",
      #                                           ci.method = "calibrated",
      #                                           dat = d,
      #                                           R = boot.reps,
      #                                           yi.name = "yi",
      #                                           vi.name = "vyi" )
      
      # not using the package because we need to report median of boot estimates, which package
      #  does not return
      ens = MetaUtility::calib_ests( yi = d$yi,
                                     sei = sqrt(d$vyi) )
      if ( p$tail == "above" ) Phat.NP.ens = sum(ens > c(p$q)) / length(ens)
      if ( p$tail == "below" ) Phat.NP.ens = sum(ens < c(p$q)) / length(ens)
      
      tryCatch({
        
        boot.res.ens = boot( data = d,
                             parallel = "no", 
                             R = boot.reps,
                             statistic = function(original, indices) {
                               
                               b = original[indices,]
                               
                               ens.b = MetaUtility::calib_ests( yi = b$yi,
                                                                sei = sqrt(b$vyi) )
                               if ( p$tail == "above" ) return( sum(ens.b > c(p$q)) / length(ens.b) )
                               if ( p$tail == "below" ) return( sum(ens.b < c(p$q)) / length(ens.b) )
                             }
        )
        
        bootCIs.ens = boot.ci(boot.res.ens, type="bca")
        boot.lo.ens = bootCIs.ens$bca[4]
        boot.hi.ens = bootCIs.ens$bca[5]
        boot.phat.med = median(boot.res.ens$t)
        
        # ~~~ bm
        # capture string about all values of t being equal
        # which is neither an error nor a message, it seems
        possible.issue = capture.output( boot.ci(boot.res.ens, type="bca") )[1]
        if ( grepl("All values of t", possible.issue) ) {
            boot.lo.ens <<- NA
            boot.hi.ens <<- NA
            boot.phat.med <<- NA
            Note <<- paste("BCa-ens failed: ", possible.issue, sep = "")
        }
        
      }, error = function(err){
        boot.lo.ens <<- NA
        boot.hi.ens <<- NA
        boot.phat.med <<- NA
        Note <<- paste("BCa-ens failed: ", err$message, sep="")
      })

      
      ##### Get Porig #####
      
      # # faulty nonparametric version that over-rejects due to granularity
      # # note this is the percentile of dO among replications, so an extreme replication may have 1
      # temp = p_orig_NP( yi.orig = dO$yi, 
      #                   vi.orig = dO$vyi,
      #                   yi.rep = d$yi,
      #                   vi.rep = d$vyi )
      # p.orig = temp$pct
      # calib.o = temp$calib.o
      
      ##### parametric version
      # bm
      p.orig = suppressMessages( Replicate::p_orig( orig.y = dO$yi,
                                                          orig.vy = dO$vyi,
                                                          yr = meta0$b,
                                                          vyr = meta0$se^2,
                                                          t2 = meta0$tau2) )
      
      # ##### robumeta version
      # # remove library call to robumeta if deleting this
      # m.rob = robu( yi ~ 1,
      #               data = d,
      #               studynum = 1:nrow(d),
      #               var.eff.size = vyi,
      #               small = TRUE )
      # 
      # p.orig.rob = suppressMessages( Replicate::p_orig( orig.y = dO$yi,
      #                                                   orig.vy = dO$vyi,
      #                                                   yr = m.rob$reg_table$b.r,
      #                                                   vyr = m.rob$reg_table$SE^2,
      #                                                   t2 = m.rob$mod_info$tau.sq) )
      meta1 = rma.uni( yi = d$yi,
                       vi = d$vyi,
                       knha = TRUE,
                       method = "PM")
      p.orig.pm = suppressMessages( Replicate::p_orig( orig.y = dO$yi,
                                                    orig.vy = dO$vyi,
                                                    yr = meta1$b,
                                                    vyr = meta1$se^2,
                                                    t2 = meta1$tau2) )
      
      ##### Write Results #####
      # NP ensemble
      tryCatch({ rows = data.frame( POrig.Method = "reml",
                                    TrueMean = p$mu,
                                    EstMean = meta0$b,
                                    # MeanCover = NA,
                                    # 
                                    # TrueVar = NA,
                                    EstVar = meta0$tau2,
                                    # VarCover = NA,
                                    
                                    #TheoryP = p$TheoryP,  # from Normal quantiles given mu, V
                                    TruthP = p.above,   # based on generated data
                                    phat = Phat.NP.ens,  # nonparametric estimator
                                    phatBias = Phat.NP.ens - p$TheoryP, # phat estimator vs. true proportion above
                                    phat.bt.med = boot.phat.med, # median of bootstrap replicates
                                    
                                    # original study point estimate and variance
                                    # for debugging
                                    yi.orig = dO$yi, 
                                    vi.orig = dO$vyi,
                                    #calib.o = calib.o,
                                    
                                    # CI performance
                                    Cover = covers(p$TheoryP, boot.lo.ens, boot.hi.ens),
                                    
                                    #Width = boot.hi.ens - boot.lo.ens,  # will break if either is null
                                    
                                    # # Porig performance as hypothesis test
                                    # Porig = p.orig,
                                    # Reject = p.orig < 0.025 | p.orig > 0.975,
                                    
                                    Porig = p.orig,
                                    Reject = p.orig < 0.05,
                                    
                                    # I^2 estimate
                                    EstI2 = meta0$I2,
                                    
                                    Note = Note)
      
      # for PM method
      rows = add_row( rows, POrig.Method = "pm",
                         TrueMean = p$mu,
                         EstMean = meta1$b,
                         # MeanCover = NA,
                         # 
                         # TrueVar = NA,
                         EstVar = meta1$tau2,
                         # VarCover = NA,
                         
                         #TheoryP = p$TheoryP,  # from Normal quantiles given mu, V
                         TruthP = p.above,   # based on generated data
                         phat = Phat.NP.ens,  # nonparametric estimator
                         phatBias = Phat.NP.ens - p$TheoryP, # phat estimator vs. true proportion above
                         phat.bt.med = boot.phat.med, # median of bootstrap replicates
                         
                         # original study point estimate and variance
                         # for debugging
                         yi.orig = dO$yi, 
                         vi.orig = dO$vyi,
                         #calib.o = calib.o,
                         
                         # CI performance
                         Cover = covers(p$TheoryP, boot.lo.ens, boot.hi.ens),
                         
                         #Width = boot.hi.ens - boot.lo.ens,  # will break if either is null
                         
                         # # Porig performance as hypothesis test
                         # Porig = p.orig,
                         # Reject = p.orig < 0.025 | p.orig > 0.975,
                         
                         Porig = p.orig.pm,
                         Reject = p.orig.pm < 0.05,
                         
                         # I^2 estimate
                         EstI2 = meta1$I2,
                         
                         Note = Note )
      
      # # for robumeta
      # rows = add_row( rows, 
      #                 POrig.Method = "robu",
      #                             TrueMean = p$mu,
      #                             EstMean = m.rob$b.r,
      #                             # MeanCover = NA,
      #                             # 
      #                             # TrueVar = NA,
      #                             EstVar = m.rob$mod_info$tau.sq,
      #                             # VarCover = NA,
      #                             
      #                             #TheoryP = p$TheoryP,  # from Normal quantiles given mu, V
      #                             TruthP = p.above,   # based on generated data
      #                             phat = Phat.NP.ens,  # nonparametric estimator
      #                             phatBias = Phat.NP.ens - p$TheoryP, # phat estimator vs. true proportion above
      #                             phat.bt.med = boot.phat.med, # median of bootstrap replicates
      #                             
      #                             # original study point estimate and variance
      #                             # for debugging
      #                             yi.orig = dO$yi, 
      #                             vi.orig = dO$vyi,
      #                             #calib.o = calib.o,
      #                             
      #                             # CI performance
      #                             Cover = covers(p$TheoryP, boot.lo.ens, boot.hi.ens),
      #                             
      #                             #Width = boot.hi.ens - boot.lo.ens,  # will break if either is null
      #                             
      #                             # # Porig performance as hypothesis test
      #                             # Porig = p.orig,
      #                             # Reject = p.orig < 0.025 | p.orig > 0.975,
      #                             
      #                             Porig = p.orig.rob,
      #                             Reject = p.orig.rob < 0.05,
      #                             
      #                             # I^2 estimate
      #                             EstI2 = m.rob$mod_info$I.2,
      #                             
      #                             Note = Note)
      
      }, error = function(err) {
        browser()
      })
      
      
      # add in scenario parameters
      rows$scen.name = scen
      rows = as.data.frame( merge(rows, scen.params,
                                  by = "scen.name") )
      rows
      
    }  ### end parallelized loop
  } )[3]  ### end timer
  
  rs$foreach.time = rep.time
  
  # if running on cluster, write each file separately
  if ( run.local == FALSE ) {
    setwd("/home/groups/manishad/RRR/sim_results/long")
    write.csv( rs, paste( "long_results_job", jobname, "_loop", l, ".csv", sep="" ) )
  }
  
  # if running locally, 
  if ( run.local == TRUE ) {
    if ( l == 1 ) rs.all = rs
    else rs.all = bind_rows(rs.all, rs)
  }
  
}  ### end entire for-loop 




# # rename for convenience
# rs = rs.all
# 
# 
# rs %>% group_by(POrig.Method) %>% summarise( n(),
#                                              #Porig.mn = mean(Porig),
#                                              Reject.mn = mean(Reject),
#                                              Est.Mean = mean(EstMean),
#                                              Est.Var = mean(EstVar) )





