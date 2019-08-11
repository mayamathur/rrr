
# helper fns for simulation study
# the fn for computing the estimators themselves are in stronger_than_function.R
#  since users will want those


########################### FN: SIMULATE 1 STUDY ###########################

# mu = true effect size as raw mean difference
# V = true variance of true effects
# muN = mean sample size in each study
# minN = minimum sample size 
# sd.w = SD within each group (2-group experiment)

sim_one_study = function( mu,
                          V, 
                          muN,
                          minN,
                          sd.w ) {
  
  # simulate total N for each study
  N = round( runif( n = 1, min = minN, max = minN + 2*( muN - minN ) ) ) # draw from uniform centered on muN
  
  # draw population true effect for this study
  Mi = rnorm( n=1, mean=mu, sd=sqrt(V) )
  
  ###### Simulate Data For Individual Subjects ######
  
  # group assignments
  X = c( rep( 0, N/2 ), rep( 1, N/2 ) )
  
  # simulate continuous outcomes
  # 2-group study of raw mean difference with means 0 and Mi in each group
  # and same SD
  Y = c( rnorm( n = N/2, mean = 0, sd = sd.w ),
         rnorm( n = N/2, mean = Mi, sd = sd.w ) )
  
  # calculate ES for this study using metafor (see Viechtbauer "Conducting...", pg 10)
  require(metafor)
  ES = escalc( measure="SMD",
               n1i = N/2, 
               n2i = N/2,
               m1i = mean( Y[X==1] ),
               m2i = mean( Y[X==0] ),
               sd1i = sd( Y[X==1] ),
               sd2i = sd( Y[X==0] ) ) 
  yi = ES$yi
  vyi = ES$vi
  
  return( data.frame( Mi, yi, vyi ) )
}




########################### FN: SIMULATE 1 WHOLE DATASET ###########################
# Vw = within-study variances
# Vwv = variance of within-study variances
# p1 = P(X=1)
# p.int = P(Y=1 | X=0, U=0), i.e., intercept probability for logistic model

sim_data = function( k, 
                     mu, 
                     V,
                     muN, 
                     minN,
                     sd.w ) {
  
  
  # initialize estimated ES to values that will enter the while-loop
  t2 = 0  
  
  # if RE fit isn't apparently causative, or if denominator is going to be undefined, sample again
  # ~~~~~~ NOTE: NEED TO BE CAREFUL CHOOSING PARAMETERS TO AVOID SYSTEMATICALLY
  # REJECTING LOTS OF SAMPLES WITH LOWER HETEROGENEITY
  # ~~~ MAYBE DON'T NEED TO REJECT 
  while ( t2 == 0 ) {
    #while ( (M <= 0) | (V == 0) ) {   
    yi = c()
    vyi = c()
    Mi = c()
    
    # simulate k studies
    for (i in 1:k) {
      study = sim_one_study( mu = mu,
                             V = V, 
                             muN = muN,
                             minN = minN,
                             sd.w = sd.w )
      yi = c( yi, study$yi )  # append this study's ES to the list
      vyi = c( vyi, study$vyi )  # append this study's variance to the list
      Mi = c( Mi, study$Mi )  # append this study's mean to the list
    }
    
    # fit RE model in order to record t2
    temp = rma.uni( yi=yi,
                    vi=vyi,
                    measure="SMD",
                    knha = TRUE,
                    method = "REML" )
    t2 = temp$tau2
  }
  
  return( data.frame( Mi, yi, vyi ) )
}



##### Fn: Check CI coverage #####
covers = function( truth, lo, hi ) {
  return( (lo <= truth) & (hi >= truth) )
}



########################### FN: COMPUTE PERFORMANCE FOR 1 SCENARIO ###########################

# all the point estimates are on log scale
power = function(scen, 
                 CI.level = 0.95
) {
  
  
  # extract simulation params for this scenario (row)
  # exclude the column with the scenario name itself (col) 
  p = scen.params[ scen.params$scen.name == scen, names(scen.params) != "scen.name"]
  
  
  for (i in 1:p$reps) { 
    # monitor simulation progress
    if(i %% 100 == 0) cat( c("Rep ", i, " of ", p$reps, "; scenario ", scen), sep="", fill=TRUE) 
    
    ##### Simulate Dataset #####
    
    d = sim_data( k = p$k, 
                  mu = p$mu, 
                  V = p$V,
                  muN = p$muN, 
                  minN = p$minN,
                  sd.w = p$sd.w )
    
    # true population proportion of studies with ES > q
    
    # DEBUGGING
    mytry = try( sum( d$Mi > q ) / length( d$Mi ) )
    if("try-error" %in% class(mytry)) browser()
    p.above = sum( d$Mi > q ) / length( d$Mi )
    
    ##### Compute Our Estimators #####
    
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
    
    # for code-writing only
    # d = sim_data( k = 50, 
    #               mu = .5, 
    #               V = .4,
    #               muN = 350, 
    #               minN = 300,
    #               sd.w = 1 )
    
    boot.res = boot( data = d, 
                     parallel = "multicore",
                     R = 500, 
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
    
    bootCIs = boot.ci(boot.res, type="perc")
    boot.lo = bootCIs$percent[4]
    boot.hi = bootCIs$percent[5]
    
    
    
    # fill in new row of summary dataframe with bias, coverage, and CI width for DM and bootstrap
    # dataframe with 3 rows, one for each method
    rows =     data.frame( TrueMean = p$mu,
                           EstMean = M,
                           MeanCover = covers( p$mu, summary(m)$ci.lb, summary(m)$ci.ub ),
                           
                           TrueVar = p$V,
                           EstVar = t2,
                           VarCover = covers( p$V, CIs$random["tau^2", "ci.lb"], CIs$random["tau^2", "ci.ub"] ),
                           
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
    
    
    # concatenate new rows to the results data.table
    require(data.table)
    if (i == 1) dt = data.table(rows)
    else dt = data.table( rbind(dt, rows) )
    
  } # end of 1 simulation rep
  
  
  # take means by method
  # for final result with 3 rows, 1 per method
  # http://stackoverflow.com/questions/16513827/r-summarizing-multiple-columns-with-data-table
  #dt = dt[, lapply(.SD, mean, na.rm=TRUE), by=Method ]
  return( as.data.frame(dt) )
}


########################### FN: NONPARAMETRIC INFERENCE FROM RUI WANG PAPER ###########################

# verbatim from their paper

##########################################################################
##########################################################################
### theta: treatment effect estimates ###
### (usually a vector of length K, K is the number of studies) ###
### theta.sd: estimated standard error of theta ###
### (usually a vector of same length as theta) ###
### mu: the specified value in the null hypothesis. ###
### pct: the percentile of interest ###
### 0.5=median, ###
### 0.25=25th percentile, ###
### 0.75=75th percentile. ###
### nperm: number of realizations in the conditional test ###

##########################################################################
### Output a 2-sided p-value. ###
##########################################################################

phi <- function(theta=theta,
                theta.sd=theta.sd,
                mu=mu0,
                pct=0.5,
                nperm=2000) {
  
  K<-length(theta)
  
  # "score" is equivalent to kth contribution of sum in 
  #  first eq. on page 4 (see my note in sidebar for equivalence)
  score<-pnorm( (mu-theta)/theta.sd ) - 0.5  
  # OBSERVED test stat
  stat<-sum(score)
  
  # initialize what will be the test stat vector UNDER H0
  # i.e., Tstar in paper
  test.stat<-rep(0,nperm)
  
  # draw Deltas for nperm iterations
  # this is the H0 distribution
  i<-1
  while (i<=nperm) {
    # here they use "ref1" and "ref2" (0/1) instead of Delta (1/-1)
    #  for computational convenience
    ref1 <- rbinom(K,1,pct)  
    ref2 <- 1-ref1
    # this is the second eq. on page 4 of paper
    test.stat[i] <- sum( (abs(score)) * ref1 - (abs(score))*ref2 )
    i<-i+1
  }
  
  # compare test.stat (which is under H0) to observed one
  p1 <- mean(as.numeric(test.stat<=stat))
  p2 <- mean(as.numeric(test.stat>=stat))
  p3 <- mean(as.numeric(test.stat==stat))
  pval <- 2*min(p1,p2) - p3
  return(pval)
}


# this is my own fn
# a simpler grid search across Phat values than their search across percentiles
# since Phat is conveniently bounded
library(dplyr)
library(purrr)

prop_stronger_np = function(q,
                            yi,
                            vi,
                            CI.level = 0.95, 
                            tail = NA,
                            R = 2000,
                            return.vectors = FALSE ) {
  

  # warmup: p-values for the proportion below q = 0.1
  # parametrically, got 28% (0%, 63%) for proportion ABOVE
  pct.vec = seq( 0, 1, 0.001 )

  pvals = pct.vec %>% map( function(x) phi( theta = yi,
                                            theta.sd = sqrt(vi),
                                            mu = q,
                                            pct = x,
                                            nperm = R ) ) %>%
                          unlist # return a double-type vector instead of list
  
  # NPMLE: the value of Phat.below with the largest p-value?
  Phat.below.NP = pct.vec[ which.max( pvals ) ]
  
  # get CI limits
  alpha = 1 - CI.level
  ( CI.lo.NP = pct.vec[ pct.vec < Phat.below.NP ][ which.min( abs( pvals[ pct.vec < Phat.below.NP ] - alpha ) ) ] )
  ( CI.hi.NP = pct.vec[ pct.vec > Phat.below.NP ][ which.min( abs( pvals[ pct.vec > Phat.below.NP ] - alpha ) ) ] )
  
  # if user wanted the upper tail, reverse everything
  if ( tail == "below" ) {
    res = data.frame(Est = Phat.below.NP,
                       lo = CI.lo.NP,
                       hi = CI.hi.NP )
    pcts = pct.vec
  }
  
  # if user wanted the upper tail, reverse everything
  if ( tail == "above" ) {
    res = data.frame( Est = 1 - Phat.below.NP,
                       lo = 1 - CI.hi.NP,
                       hi = 1 - CI.lo.NP )
    pcts = 1 - pct.vec
  }
  
  if ( return.vectors == FALSE ) return(res)
  if ( return.vectors == TRUE ) invisible( list(res = res, 
                                             pcts = pcts,
                                             pvals = pvals) )
}


########################### FN: STITCH RESULTS FILES ###########################

# given a folder path for results and a common beginning of file name of results files
#   written by separate workers in parallel, stitch results files together into a
#   single csv.

stitch_files = function(.results.singles.path, .results.stitched.write.path=.results.singles.path,
                        .name.prefix, .stitch.file.name="stitched_model_fit_results.csv") {
  
  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  
  # grab variable names from first file
  names = names( read.csv(keepers[1] )[-1] )
  
  # initialize stitched dataframe
  s = as.data.frame( matrix(nrow=1, ncol=length(names)) )
  names(s) = names
  
  # stitch the files
  for ( i in 1:length(keepers) ) {
    new.chunk = read.csv(keepers[i])[,-1]
    s = rbind(s, new.chunk)
  }
  
  s = s[-1,]  # delete annoying NA row
  write.csv(s, paste(.results.stitched.write.path, .stitch.file.name, sep="/") )
  return(s)
}


prop_stronger = function( q,
                          M,
                          t2,
                          se.M = NA,
                          se.t2 = NA,
                          CI.level = 0.95,
                          tail = NA ) {
  
  
  
  
  ##### Helper Fns ######
  get_expit = function(x) exp(x) / (1 + exp(x))
  get_logit = function(x) log( x / (1-x) )
  
  ##### Check for Bad Input #####
  if ( t2 < 0 ) stop("Heterogeneity cannot be negative")
  
  # the second condition is needed for Shiny app:
  #  if user deletes the input in box, then it's NA instead of NULL
  if ( ! is.na(se.M) ) {
    if (se.M < 0) stop("se.M cannot be negative")
  }
  
  if ( ! is.na(se.t2) ) {
    if (se.t2 < 0) stop("se.t2 cannot be negative")
  }
  
  ##### Messages When Not All Output Can Be Computed #####
  #if ( is.na(se.M) | is.na(se.t2) ) message("Cannot compute inference without se.M and se.t2 Returning only point estimates.")
  
  
  ##### Point Estimates #####
  # same regardless of tail
  Z = (q - M) / sqrt(t2)
  
  if ( tail == "above" ) phat = 1 - pnorm(Z)
  else if ( tail == "below" ) phat = pnorm(Z)
  
  
  # do inference only if given needed SEs
  if ( !is.na(se.M) & !is.na(se.t2) ){
    
    
    ##### Delta Method Inference on Logit Scale ##### 
    term1 = ( dnorm(Z) / ( pnorm(Z) - pnorm(Z)^2 ) )^2
    term2 = se.M^2 / t2
    term3 = ( se.t2^2 * ( M - q )^2 ) / ( 4 * t2^3 )
    
    # note that this gives NaN if phat = 0 or 1 because logit is undefined
    
    # SE of logit(phat)
    SE1 = sqrt( term1 * ( term2 + term3 ) )
    
    # sanity check
    # compare to SE from R's DM function
    # for q > M
    
    # deltamethod( ~ log( ( 1 - pnorm( ( .63 - x1 ) / sqrt( x2 ) ) ) /
    # 					( 1 - ( 1 - pnorm( ( .63 - x1 ) / sqrt( x2 ) ) ) ) ),
    #                    mean = c( M, t2 ),
    #                    cov = diag( c( se.M^2, se.t2^2 ) ) )
    
    
    # confidence interval
    if ( phat > 0.99 | phat < 0.01 ) {
      lo1 = NA
      hi1 = NA
      warning("phat was close to 0 or 1, so logit-based CI is undefined.")
    } else {
      tail.prob = ( 1 - CI.level ) / 2
      lo1 = get_expit( get_logit(phat) - qnorm( 1 - tail.prob )*SE1 )
      hi1 = get_expit( get_logit(phat) + qnorm( 1 - tail.prob )*SE1 )
    }
    
    
    ##### Delta Method Inference on Original Scale #####
    term1.1 = se.M^2 / t2
    term1.2 = ( se.t2^2 * ( q - M )^2 ) / ( 4 * t2^3 )
    term1 = sqrt( term1.1 + term1.2 )
    
    SE2 = term1 * dnorm(Z)
    
    # confidence interval
    tail.prob = ( 1 - CI.level ) / 2
    lo2 = max( 0, phat + qnorm( tail.prob )*SE2 )
    hi2 = min( 1, phat - qnorm( tail.prob )*SE2 )
    
  } else {
    SE1 = SE2 = lo1 = lo2 = hi1 = hi2 = NA
  }
  
  
  # return results
  res = data.frame( Est = phat, 
                    SE.logit = SE1,
                    SE = SE2,
                    lo.expon = lo1, # exponentiated from logit scale
                    hi.expon = hi1,
                    lo = lo2, 
                    hi = hi2 ) 
  rownames(res) = NULL
  res
}



########################### FN: STITCH RESULTS FILES ###########################

# Given a folder path for results and a common beginning of file name of results files
#   written by separate workers in parallel, stitch results files together into a
#   single csv.

# Arguments: 
#  .results.singles.path: Path of unstitched single results files
#  .results.stitched.write.path: Path to which stitched data should be written
#  .name.prefix: String contained anywhere in name of each single results files, but
#   not in name of any other random files at .results.singles.path
#  .stitch.file.name: What to call the stitched file

stitch_files = function(.results.singles.path,
                        .results.stitched.write.path = .results.singles.path,
                        .name.prefix,
                        .stitch.file.name="stitched_model_fit_results.csv") {
  
  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  
  # grab variable names from first file
  names = names( read.csv(keepers[1] )[-1] )
  
  # initialize stitched dataframe
  s = as.data.frame( matrix(nrow=1, ncol=length(names)) )
  names(s) = names
  
  # stitch the files
  for ( i in 1:length(keepers) ) {
    new.chunk = try( read.csv(keepers[i])[,-1] )
    if ( inherits(new.chunk, 'try-error') ) warning( paste( keepers[i], "has a problem!") )
    s = rbind(s, new.chunk)
  }
  
  
  #s = s[-1,]  # delete annoying NA row
  write.csv(s, paste(.results.stitched.write.path, .stitch.file.name, sep="/") )
  return(s)
}


########################### FN: RETURN FILES THAT AREN'T COMPLETED ###########################

# Given a folder path for results and a common beginning of file name of results files
#   written by separate workers in parallel, stitch results files together into a
#   single csv.

# .max.sbatch.num: If not passed, defaults to largest number in actually run jobs.

sbatch_not_run = function(.results.singles.path,
                          .results.write.path,
                          .name.prefix,
                          .max.sbatch.num = NA ) {
  
  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  
  # extract job numbers
  sbatch.nums = as.numeric( unlist( lapply( strsplit( keepers, split = "_"), FUN = function(x) x[5] ) ) )
  
  # check for missed jobs before the max one
  if ( is.na(.max.sbatch.num) ) .max.sbatch.num = max(sbatch.nums)
  all.nums = 1 : .max.sbatch.num
  missed.nums = all.nums[ !all.nums %in% sbatch.nums ]
  
  # give info
  print( paste("The max job number is: ", max(sbatch.nums) ) )
  print( paste( "Number of jobs that weren't run: ",
                ifelse( length(missed.nums) > 0, length(missed.nums), "none" ) ) )
  
  if( length(missed.nums) > 0 ) {
    setwd(.results.write.path)
    write.csv(missed.nums, "missed_job_nums.csv")
  }
  
  return(missed.nums)
  
}

# missed.nums = sbatch_not_run( "/home/groups/manishad/multTest/sim_results/short",
#                 "/home/groups/manishad/multTest/sim_results",
#                 .name.prefix = "short_results" )
# scp mmathur@sherlock:/share/PI/manishad/multTest/sim_results/missed_job_nums.csv ~/Desktop


########################### SLURM FUNCTIONS ###########################

# These just generate the sbatch files

# DO NOT CHANGE THE INDENTATION IN THE BELOW OR ELSE SLURM 
#  WILL SILENTLY IGNORE THE BATCH COMMANDS DUE TO EXTRA WHITESPACE!!
sbatch_skeleton <- function() {
  return(
    "#!/bin/bash
    #################
    #set a job name  
    #SBATCH --job-name=JOBNAME
    #################  
    #a file for job output, you can check job progress
    #SBATCH --output=OUTFILE
    #################
    # a file for errors from the job
    #SBATCH --error=ERRORFILE
    #################
    #time you think you need; default is one hour
    #SBATCH --time=JOBTIME
    #################
    #quality of service; think of it as job priority
    #SBATCH --qos=QUALITY
    #################
    #submit to both owners and normal partition
    #SBATCH -p normal,owners
    #################
    #number of nodes you are requesting
    #SBATCH --nodes=NODENUMBER
    #################
    #memory per node; default is 4000 MB
    #SBATCH --mem=MEMPERNODE
    #you could use --mem-per-cpu; they mean what we are calling cores
    #################
    #get emailed about job BEGIN, END, and FAIL
    #SBATCH --mail-type=MAILTYPE
    #################
    #who to send email to; please change to your email
    #SBATCH  --mail-user=USER_EMAIL
    #################
    #task to run per node; each node has 16 cores
    #SBATCH --ntasks=TASKS_PER_NODE
    #################
    #SBATCH --cpus-per-task=CPUS_PER_TASK
    #now run normal batch commands
    
    ml load R
    srun R -f PATH_TO_R_SCRIPT ARGS_TO_R_SCRIPT")
}



generateSbatch <- function(sbatch_params, runfile_path = NA, run_now = F) {
  
  #sbatch_params is a data frame with the following columns
  #jobname: string, specifies name associated with job in SLURM queue
  #outfile: string, specifies the name of the output file generated by job
  #errorfile: string, specifies the name of the error file generated by job
  #jobtime: string in hh:mm:ss format, max (maybe soft) is 48:00:00 
  #specifies the amoung of time job resources should be allocated
  #jobs still running after this amount of time will be aborted
  #quality: kind of like priority, normal works
  #node_number, integer: the number of nodes (computers w/16 cpus each) to allocate 
  #mem_per_node, integer: RAM, in MB, to allocate to each node
  #mailtype, string: ALL, BEGIN, END, FAIL: what types of events should you be notified about via email
  #user_email string: email address: email address to send notifications
  #tasks_per_node: integer, number of tasks, you should probably use 1
  #cpus_per_task: integer, 1-16, number of cpus to use, corresponds to number of available cores per task
  #path_to_r_script: path to r script on sherlock
  #args_to_r_script: arguments to pass to r script on command line
  #write_path: where to write the sbatch file
  #server_sbatch_path: where sbatch files will be stored on sherlock
  #runfile_path is a string containing a path at which to write an R script that can be used to run
  #the batch files generated by this function. 
  #if NA, no runfile will be written
  #run_now is a boolean specifying whether batch files should be run as they are generated
  
  sbatches <- list()
  if (!is.na(runfile_path)) {
    outfile_lines <- c(paste0("# Generated on ",  Sys.time()))
  }
  for (sbatch in 1:nrow(sbatch_params) ) {
    gen_batch <- sbatch_skeleton()
    #set job name
    if (is.null(sbatch_params$jobname[sbatch])) { 
      gen_batch <- gsub("JOBNAME", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("JOBNAME", sbatch_params$jobname[sbatch], gen_batch) 
    }
    #set outfile name
    if (is.null(sbatch_params$outfile[sbatch])) { 
      gen_batch <- gsub("OUTFILE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("OUTFILE", sbatch_params$outfile[sbatch], gen_batch) 
    }
    #set errorfile name
    if (is.null(sbatch_params$errorfile[sbatch])) { 
      gen_batch <- gsub("ERRORFILE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("ERRORFILE", sbatch_params$errorfile[sbatch], gen_batch) 
    }
    #set jobtime
    if (is.null(sbatch_params$jobtime[sbatch])) { 
      gen_batch <- gsub("JOBTIME", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("JOBTIME", sbatch_params$jobtime[sbatch], gen_batch) 
    }
    #set quality
    if (is.null(sbatch_params$quality[sbatch])) { 
      gen_batch <- gsub("QUALITY", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("QUALITY", sbatch_params$quality[sbatch], gen_batch) 
    }
    #set number of nodes
    if (is.null(sbatch_params$node_number[sbatch])) { 
      gen_batch <- gsub("NODENUMBER", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("NODENUMBER", sbatch_params$node_number[sbatch], gen_batch) 
    }
    #set memory per node
    if (is.null(sbatch_params$mem_per_node[sbatch])) { 
      gen_batch <- gsub("MEMPERNODE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("MEMPERNODE", sbatch_params$mem_per_node[sbatch], gen_batch) 
    }
    #set requested mail message types
    if (is.null(sbatch_params$mailtype[sbatch])) { 
      gen_batch <- gsub("MAILTYPE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("MAILTYPE", sbatch_params$mailtype[sbatch], gen_batch) 
    }
    #set email at which to receive messages
    if (is.null(sbatch_params$user_email[sbatch])) { 
      gen_batch <- gsub("USER_EMAIL", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("USER_EMAIL", sbatch_params$user_email[sbatch], gen_batch) 
    }
    #set tasks per node
    if (is.null(sbatch_params$tasks_per_node[sbatch])) { 
      gen_batch <- gsub("TASKS_PER_NODE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("TASKS_PER_NODE", sbatch_params$tasks_per_node[sbatch], gen_batch) 
    }
    #set cpus per task
    if (is.null(sbatch_params$cpus_per_task[sbatch])) { 
      gen_batch <- gsub("CPUS_PER_TASK", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("CPUS_PER_TASK", sbatch_params$cpus_per_task[sbatch], gen_batch) 
    }
    #set path to r script
    if (is.null(sbatch_params$path_to_r_script[sbatch])) { 
      gen_batch <- gsub("PATH_TO_R_SCRIPT", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("PATH_TO_R_SCRIPT", sbatch_params$path_to_r_script[sbatch], gen_batch) 
    }
    #set args to r script
    if (is.null(sbatch_params$args_to_r_script[sbatch])) { 
      gen_batch <- gsub("ARGS_TO_R_SCRIPT", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("ARGS_TO_R_SCRIPT", sbatch_params$args_to_r_script[sbatch], gen_batch) 
    }
    
    #write batch file
    if (is.null(sbatch_params$write_path[sbatch])) { 
      cat(gen_batch, file = paste0("~/sbatch_generated_at_", gsub(" |:|-", "_", Sys.time()) ), append = F)
    } else { 
      cat(gen_batch, file = sbatch_params$write_path[sbatch], append = F)
    }
    
    if (!is.na(sbatch_params$server_sbatch_path[sbatch])) {
      outfile_lines <- c(outfile_lines, paste0("system(\"sbatch ", sbatch_params$server_sbatch_path[sbatch], "\")"))
    } 
    sbatches[[sbatch]] <- gen_batch
  }
  if (!is.na(runfile_path)) {
    cat(paste0(outfile_lines, collapse = "\n"), file = runfile_path)
  }
  if(run_now) { system(paste0("R -f ", runfile_path)) } 
  
  return(sbatches)
}

