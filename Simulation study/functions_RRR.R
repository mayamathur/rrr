

########################### FN: PORIG NONPARAMETRIC ###########################

# get calibrated estimates from replications
# also get calibrated estimate from original, using replications' mean and variance
# look up percentile of latter among former
p_orig_NP = function( yi.orig, 
                      vi.orig,
                      yi.rep,
                      vi.rep ) {
  
  # calculate ensemble estimates for replications only
  calib.r = MetaUtility::calib_ests( yi = yi.rep,
                                     sei = sqrt(vi.rep) )
  
  # meta-analyze the replications (for getting below calibrated ests)
  meta = rma.uni( yi = yi.rep,
                  sei = sqrt(vi.rep),
                  method = "DL" )
  
  # calibrated estimate for original
  calib.o = as.numeric( c(meta$b) + ( c(meta$tau2) / ( c(meta$tau2) + vi.orig ) )^(1/2) * ( yi.orig - c(meta$b) ) )
    
  # get percentile of original within replications
  pct =  ecdf(calib.r)(calib.o)
  
  # average over all calibrated estimates, weighting them equally
  return( data.frame(pct = pct,
                     calib.o = calib.o) )
}


# # this version is WRONG
# p_orig_NP = function( yi.orig, 
#                       vi.orig,
#                       N.orig,
#                       yi.rep,
#                       vi.rep ) {
#   
#   #browser()
#   
#   # calculate ensemble estimates for replications only
#   calib.r = MetaUtility::calib_ests( yi = yi.rep,
#                                      sei = sqrt(vi.rep) )
#   
#   # P( abs( point estimate - yi.rep ) > observed | true theta = calib ) for each calibrated estimate
#   t = ( c(yi.orig) - calib.r ) / sqrt(vi.orig)
#   # two-tailed p-value
#   # symmetric despite arbitrary distribution of true effects because of sampling dist
#   pval = 2 * ( 1 - pt( abs(t),
#                        df = N.orig - 1 ) )
#   
#   # average over all calibrated estimates, weighting them equally
#   return( mean(pval) )
# }


# ########################### FN: ENSEMBLE ESTIMATES ###########################
# 
# # my calculation of ensemble estimates
# # see Wang paper
# my_ens = function(yi,
#                   sei ) {
#   
#   meta = rma.uni( yi = yi, 
#                   sei = sei, 
#                   method = "DL" )
#   
#   muhat = meta$b
#   t2 = meta$tau2
#   
#   # return ensemble estimates
#   c(muhat) + ( c(t2) / ( c(t2) + sei^2 ) )^(1/2) * ( yi - c(muhat) )
# }

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
                          sd.w,
                          true.effect.dist = "normal"
                          ) {
  
  # simulate total N for each study
  N = round( runif( n = 1, min = minN, max = minN + 2*( muN - minN ) ) ) # draw from uniform centered on muN
  
  ##### Draw a Single Population True Effect for This Study #####
  if ( true.effect.dist == "normal" ) {
    Mi = rnorm( n=1, mean=mu, sd=sqrt(V) )
  }
  if ( true.effect.dist == "expo" ) {
    # set the rate so the heterogeneity is correct
    Mi = rexp( n = 1, rate = sqrt(1/V) )
    # now the mean is sqrt(V) rather than mu
    # shift to have the correct mean (in expectation)
    Mi = Mi + (mu - sqrt(V))
  }
  if ( true.effect.dist == "unif2") {
    Mi = runif2( n = 1,
                 mu = mu, 
                 V = V)$x
  }
  if (true.effect.dist == "t.scaled") {
    Mi = rt.scaled(n = 1,
                  df = 3,  # fixed for all scenarios to get very heavy tails
                  mean = mu,
                  sd = sqrt(V))
  }
  
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


##################### FNs FOR UNIFORM MIXTURE #####################

# generate from a uniform mixture with endpoints [-b, -a] and [a, b]
#  but shifted so that the grand mean is mu
runif2 = function(n,
                  mu,
                  V) {
  # calculate lower limit for positive distribution, a
  # arbitrary, but can't have too large or else it;s impossible to find
  #  a valid b
  a = sqrt(V)/2  
  
  # calculate upper endpoint for positive distribution, b
  b = abs( 0.5 * ( sqrt(3) * sqrt( 4*V - a^2 ) - a ) )
  
  # prior to mean shift
  components = sample(1:2,
                      prob=c(0.5, 0.5),
                      size=n,
                      replace=TRUE)
  
  mins = c( -b, a )
  maxes = c( -a, b )
  
  samples = runif(n=n,
                  min=mins[components],
                  max=maxes[components])
  
  # mean-shift them
  samples = samples + mu
  
  return( list(x = samples,
               a = a, 
               b = b) )
}

# # sanity check
# mu = 0.5
# V = 0.1^2
# fake = runif2( n = 10000,
#                mu = mu, 
#                V = V)$x
# 
# hist(fake)
# mean(fake)
# var(fake); V


# calculate quantile
qunif2 = function(p, 
                  mu,
                  V) {
  
  # calculate lower limit for positive distribution, a
  a = sqrt(V)/2  
  
  # calculate upper endpoint for positive distribution, b
  b = abs( 0.5 * ( sqrt(3) * sqrt( 4*V - a^2 ) - a ) )
  
  # in this case, easy because the q must be within
  #  the negative distribution
  if (p < 0.5) {
    # total length of support, not counting the gap, 
    #  is (b-a)*2
    # we want a point that is p% of the way through the support
    
    # from the lower endpoint of the negative dist, add the proportion of that interval
    q.shift = -b + p * (b-a)*2
  }
  
  else if (p == 0.5) q.shift = 0
  
  # now we're in the positive part of the distribution
  else if (p > 0.5) {
    # subtract the 0.5 that's used up by the negative dist
    # so we want a point that is (p-0.5)% of the way into the support
    #  of the positive part
    #  and the positive part has support length (b-a)
    #q.shift = a + (p - 0.5) * (b-a)
    
    #browser()
    q.temp = -b + p * (b-a)*2
    q.shift = q.temp + 2*a
  }
  
  q = q.shift + mu
  return(q)
}

# # sanity check
# mu = 0.5
# V = 0.1^2
# fake = runif2( n = 10000,
#                mu = mu,
#                V = V)
# 
# hist(fake$x)
# mean(fake$x)
# var(fake$x); V
# 
# p = 0.1
# ( q = qunif2( p = p, 
#         mu = mu, 
#         V = V) )
# sum(fake$x < q) / length(fake$x); p
# # works :) 


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
                     sd.w, 
                     true.effect.dist) {
  
  
  # initialize estimated ES to values that will enter the while-loop
  t2 = 0  
  
  # if denominator is going to be undefined, sample again
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
                             sd.w = sd.w,
                             true.effect.dist = true.effect.dist)
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

  if ( is.null(truth) | is.null(lo) | is.null(hi) ) return(NA)
  if ( is.na(truth) | is.na(lo) | is.na(hi) ) return(NA)
  
  return( (lo <= truth) & (hi >= truth) )
}

########################### FN: MAKE SCENARIO PARAMETERS ###########################

# if muN left unspecified, defaults to same as minN
# all arguments that are scen parameters can be vectors
#  for use in expand_grid
make_scen_params = function( k,
                             mu,  # mean of true effects (log-RR)
                             V,  # variance of true effects
                             muN = NA, # just a placeholder; to be filled in later
                             minN,
                             sd.w,
                             tail,
                             true.effect.dist, # "expo" or "normal"
                             TheoryP,
                             
                             delta, 
                             N.orig,
                             
                             # number to start scenario names 
                             start.at = 1) {
  
  # full set of scenarios
  scen.params = expand.grid( k = k,
                             mu = mu,  # mean of true effects (log-RR)
                             V = V,  # variance of true effects
                             muN = muN, # just a placeholder; to be filled in later
                             minN = minN,
                             sd.w = sd.w,
                             tail = tail,
                             true.effect.dist = true.effect.dist, # "expo" or "normal"
                             TheoryP = TheoryP,
                             delta = delta,
                            N.orig = N.orig )
  
  scen.params = scen.params %>% rowwise %>%
    mutate( q = calculate_q(true.effect.dist = true.effect.dist,
                            TheoryP = TheoryP, 
                            mu = mu, 
                            V = V) )
  
  # name the scenarios
  scen.params$scen.name = start.at : ( start.at + nrow(scen.params) - 1 )
  
  # avoid doing all factorial combinations of muN and minN this way
  if ( all(is.na(scen.params$muN) ) ) scen.params$muN = scen.params$minN
  
  return(scen.params)
}


########################### FN: QUANTILE CALCULATOR FOR A DESIRED THEORYP ###########################


# return the threshold q that is the TheoryP^th quantile
calculate_q = function(true.effect.dist, 
                       TheoryP,
                       mu,
                       V){
  
  if ( true.effect.dist == "normal" ) {
    return( qnorm( p = 1 - TheoryP,
                   mean = mu,
                   sd = sqrt(V) ) )
  }
  
  if ( true.effect.dist == "expo" ) {
    # we generate from a exponential, then shift to achieve the correct mean, 
    #  so this is the threshold BEFORE shifting
    # here is the data generation code from sim_data:
    # Mi = rexp( n = 1, rate = sqrt(1/V) )
    # Mi = Mi + (mu - sqrt(V))
    q0 = qexp( p = TheoryP, 
               rate = sqrt(1/V),
               lower.tail = FALSE)
    return( q0 + (mu - sqrt(V) ) )
  }
  
  if ( true.effect.dist == "unif2" ) {
    return( qunif2( p = 1 - TheoryP, 
                    mu = mu, 
                    V = V) )
  }
  
  if (true.effect.dist == "t.scaled") {
    # from metRology package
    return( qt.scaled(p = 1 - TheoryP,
                      df = 3,
                      mean = mu,
                      sd = sqrt(V) ) )
  }
  
  else stop("true.effect.dist not recognized.")
}
# # sanity check
# ( q = calculate_q( true.effect.dist = "expo",
#              TheoryP = 0.1, 
#              mu = 0.5,
#              V = 0.25^2 ) )
# Mi = rexp( n = 10000, rate = sqrt(1/(.25^2)) )
# Mi = Mi + (0.5 - sqrt(.25^2))
# sum(Mi > q) / length(Mi)



########################### FN: STITCH RESULTS FILES ###########################

# given a folder path for results and a common beginning of file name of results files
#   written by separate workers in parallel, stitch results files together into a
#   single csv.

stitch_files = function(.results.singles.path,
                        .results.stitched.write.path=.results.singles.path,
                        .name.prefix,
                        .stitch.file.name="stitched_model_fit_results.csv",
                        .start.num = 1,
                        .stop.num = NA) {
  
  # .results.singles.path = "/home/groups/manishad/RRR/sim_results/long"
  # .results.stitched.write.path = "/home/groups/manishad/RRR/sim_results/overall_stitched"
  # .name.prefix = "long_results"
  # .stitch.file.name="stitched.csv"
  
  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  
  # make sure stop num isn't longer than keepers
  .stop.num = min(.stop.num, length(keepers))
  
  # if limiting the number of files to stitch to keep job 
  #  size manageable
  if (!is.na(.stop.num)) {
    keepers = keepers[ .start.num : .stop.num ]
  }
  
  # # grab variable names from first file
  # names = names( read.csv(keepers[1] )[-1] )
  
  dfs = lapply(keepers, fread)
  
  # "fill" argument fills in NAs if column names don't line up
  s = rbindlist(dfs, fill = TRUE)
  s = as.data.frame(s)

  s = s[ , names(s) != "V1" ]

  if( is.na(s[1,1]) ) s = s[-1,]  # delete annoying NA row
  
  if (is.na(.stop.num)) {
    write.csv(s, paste(.results.stitched.write.path, .stitch.file.name, sep="/") )
  } else {
    # if breaking up the task, use a unique name to avoid overwriting
    name = paste(.stop.num, .stitch.file.name, sep="_")
    write.csv(s, paste(.results.stitched.write.path, name, sep="/") )
  }

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


########################### FN: RETURN FILES THAT AREN'T COMPLETED ###########################

# looks at results files to identify sbatches that didn't write a file
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
R -f PATH_TO_R_SCRIPT ARGS_TO_R_SCRIPT")
}



generateSbatch <- function(sbatch_params,
                           runfile_path = NA,
                           run_now = F) {
  
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


########################### ANALYSIS HELPER FNS ###########################

