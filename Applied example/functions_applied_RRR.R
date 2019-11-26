
##### Contact: Maya Mathur (mmathur@stanford.edu)
#####
##### Note: This code is provided to allow independent replication of the 
#####  Markdown manuscript. Users conducting their own analyses are encouraged
#####  to use the fully documented versions of these functions in the R package
#####  "Replicate", which are more versatile and conduct logic checks to ensure
#####  proper input.


################################ MISC. FNs ################################ 

# for reproducible manuscript-writing
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
# expects a global variable called "results.dir"
update_result_csv = function( name,
                              section = NA,
                              value = NA,
                              print = FALSE,
                              .results.dir = results.dir ) {
  setwd(.results.dir)
  
  new.rows = data.frame( name,
                         value = as.character(value),
                         section = as.character(section) )
  
  # to avoid issues with variable types when overwriting
  new.rows$name = as.character(new.rows$name)
  new.rows$value = as.character(new.rows$value)
  new.rows$section = as.character(new.rows$section)
  
  
  if ( "stats_for_paper.csv" %in% list.files() ) {
    res = read.csv( "stats_for_paper.csv",
                    stringsAsFactors = FALSE,
                    colClasses = rep("character", 3 ) )
    
    # if this entry is already in the results file, overwrite the
    #  old one
    if ( all(name %in% res$name) ) res[ res$name %in% name, ] = new.rows
    else res = rbind(res, new.rows)
  }
  
  if ( !"stats_for_paper.csv" %in% list.files() ) {
    res = new.rows
  }
  
  write.csv( res, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  if ( print == TRUE ) {
    View(res)
  }
}


##### Convert Between r (Correlation) and z (Fisher's) Units ##### 
r_to_z = Vectorize( function(r) {
  .5 * ( log(1 + r) - log(1 - r) )
}, vectorize.args = "r" )

z_to_r = Vectorize( function(z) {
  ( exp( 2 * z ) - 1 ) / ( exp( 2 * z ) + 1 )
}, vectorize.args = "z" )




################################ FNs FOR DIAGNOSTIC PLOTS ################################ 

# see section 3.11 of Veroniki (from Raudenbush 2009)
t2_lkl = function( yi, vi, t2, yi.orig, vi.orig ) {
  k = length(yi)
  wi = 1 / ( vi + t2 )
  
  # calculate the RE Mhat based on this particular t2
  Mhat = sum(yi * wi) / sum(wi)
  
  term1 = (k/2) * log(2*pi)
  term2 = 0.5 * sum( log(vi + t2) )
  term3 = 0.5 * sum( ( yi - Mhat )^2 / ( vi + t2 ) )
  term4 = 0.5 * log( sum( 1 / ( vi + t2 ) ) )
  
  return( -term1 - term2 - term3 - term4 )
}


diag_plots = function(yi, vi, yi.orig, vi.orig) {
  
  # fit meta-analysis
  .m = rma.uni( yi = yi,
                vi = vi,
                knha = TRUE ) 
  
  ##### Make Plotting Dataframe with Different Tau^2 #####
  t2.vec = seq(0, .m$tau2*10, .001)
  t2l = as.list(t2.vec)
  
  temp = lapply( t2l,
                 FUN = function(t2) {
                   suppressMessages( p_orig( orig.y = yi.orig,
                                             orig.vy = vi.orig,
                                             yr = .m$b,
                                             t2 = t2,
                                             vyr = .m$vb ) )
                 })
  
  # plotting df
  dp = data.frame( tau = sqrt(t2.vec),
                   V = t2.vec,
                   Porig = unlist(temp) )
  
  
  # fn: tau^2 vs. the estimated one
  g = function(x) x / .m$tau2
  ( breaks.x1 = seq( 0, max(dp$V), .005 ) )
  ( breaks.x2 = seq( 0, max( g(dp$V) ), 1 ) )
  
  ##### **Plot 1: Tau^2 vs. Porig #####
  p1 = ggplot( data = dp,
               aes(x = V, 
                   y = Porig) ) +
    
    geom_vline(xintercept = .m$tau2,
               lty = 2,
               color = "red") +  # observed one
    
    geom_line(lwd = 1.2) +
    theme_classic() +
    xlab( bquote( hat(tau)["*"]^2 ) ) +
    ylab( bquote(P[orig]) ) +
    scale_x_continuous( limits = c(0, max(breaks.x1)),
                        breaks = breaks.x1,
                        sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                                             name = bquote( hat(tau)["*"]^2 / hat(tau)^2 ),
                                             breaks=breaks.x2 ) ) +
    scale_y_continuous( breaks = seq(0,1,.05) )
  # 6 x 4
  
  ##### **Plot 2: Tau^2 vs. Mhat/Tau^2 ##### 
  p2 = ggplot( data = dp,
               aes(x = V, 
                   y = .m$b / V ) ) +
    
    geom_vline(xintercept = .m$tau2,
               lty = 2,
               color = "red") +  # observed one
    
    geom_line(lwd = 1.2) +
    theme_classic() +
    xlab( bquote( hat(tau)["*"]^2 ) ) +
    ylab( bquote( hat(mu) / hat(tau)["*"]^2 ) ) +
    scale_x_continuous( limits = c(0, max(breaks.x1)),
                        breaks = breaks.x1,
                        sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                                             name = bquote( hat(tau)["*"]^2 / hat(tau)^2 ),
                                             breaks=breaks.x2 ) ) +
    scale_y_continuous( breaks = seq(0,80,10) )
  # 6 x 4
  
  ##### **Plot 3: Marginal Log-Lkl of Tau^2 ##### 
  temp = lapply( t2l,
                 FUN = function(t2) {
                   t2_lkl( yi = yi,
                           vi = vi,
                           t2 = t2 )
                 })
  
  # plotting df
  dp = data.frame( tau = sqrt(t2.vec),
                   V = t2.vec,
                   lkl = unlist(temp) )
  
  p3 = ggplot( data = dp,
               aes(x = V, 
                   y = lkl ) ) +
    
    geom_vline(xintercept = .m$tau2,
               lty = 2,
               color = "red") +  # observed one
    
    geom_line(lwd = 1.2) +
    theme_classic() +
    xlab( bquote( hat(tau)["*"]^2 ) ) +
    ylab( "Marginal log-likelihood of " ~ hat(tau)["*"]^2 ) +
    scale_x_continuous( limits = c(0, max(breaks.x1)),
                        breaks = breaks.x1,
                        sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                                             name = bquote( hat(tau)["*"]^2 / hat(tau)^2 ),
                                             breaks=breaks.x2 ) )
  
  return( list(p1, p2, p3) )
  
}



################################ FN: COMPUTE PROPORTION OF EFFECTS STRONGER THAN THRESHOLD ################################ 

# q: threshold of scientific importance
# M: pooled point estimate of meta-analysis
# t2: estimated heterogeneity, tau^2
# se.M: estimated standard error of M
# se.t2: estimated standard error of t2
# CI.level: confidence interval level
# tail: "above" to compute proportion of effects above q; 
#  "below" for proportion below q

# VERSION THAT DOES BOOTSTRAPPING
# dat: dataframe of yi and vi (with those names) for bootstrapping
# R: bootstrap replicates 
# boot: "never" or "ifneeded"
# measure: to be passed to metafor if using bootstrapping
# method: ditto

prop_stronger2 = function( dat = NA,
                           q,
                           M,
                           t2,
                           se.M = NA,
                           se.t2 = NA,
                           CI.level = 0.95,
                           tail = NA,
                           R = 2000,
                           boot = "ifneeded",
                           yi.name = "yi",
                           vi.name = "vi",
                           measure = NA,
                           method = NA ) {
  
  
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
  if ( is.na(se.M) | is.na(se.t2) ) message("Cannot compute inference without se.M and \nse.t2.\n Returning only point estimates.")
  
  
  ##### Point Estimates #####
  # same regardless of tail
  Z = (q - M) / sqrt(t2)
  
  if ( tail == "above" ) phat = 1 - pnorm(Z)
  else if ( tail == "below" ) phat = pnorm(Z)
  
  extreme = (phat < 0.15 | phat > 0.85) 
  
  if ( extreme ) {
    if ( boot == "ifneeded" ) {
      warning("The estimated proportion is close to 0 or 1,\n so the theoretical CI may perform poorly. Using \nBCa bootstrapping instead.")
      
      if ( is.na(method) | is.na(measure) ) {
        stop("Must provide method and measure in order to fit meta-analysis model to bootstrap samples.")
      }
      
      boot.res = suppressWarnings( my_boot( data = dat, 
                                            parallel = "multicore",
                                            R = R, 
                                            statistic = get_stat,
                                            # remaining arguments are being passed to get_stat
                                            yi.name = yi.name,
                                            vi.name = vi.name,
                                            measure = measure,
                                            method = method,
                                            q = q,
                                            tail = tail ) )
      
      bootCIs = boot.ci(boot.res, type="bca")
      lo = round( bootCIs$bca[4], 2 )
      hi = round( bootCIs$bca[5], 2 )
      SE = NA  
    } 
    
    if ( boot == "never" ) {
      warning("The estimated proportion is close to 0 or 1,\n so the theoretical CI may perform poorly. Should use \nBCa bootstrapping instead.")
      SE = lo = hi = NA
    }
  }
  
  
  if ( !extreme ) {
    # do inference only if given needed SEs
    if ( !is.na(se.M) & !is.na(se.t2) ){
      
      ##### Delta Method Inference on Original Scale #####
      term1.1 = se.M^2 / t2
      term1.2 = ( se.t2^2 * ( q - M )^2 ) / ( 4 * t2^3 )
      term1 = sqrt( term1.1 + term1.2 )
      
      SE = term1 * dnorm(Z)
      
      # confidence interval
      tail.prob = ( 1 - CI.level ) / 2
      lo = max( 0, phat + qnorm( tail.prob )*SE )
      hi = min( 1, phat - qnorm( tail.prob )*SE )
    } else {
      SE = lo = hi = NA
    }
  }
  
  # return results
  res = data.frame( Est = phat, 
                    SE = SE,
                    lo = lo, 
                    hi = hi ) 
  rownames(res) = NULL
  res
}



################################### GET P-HAT STAT FOR USE WITH BOOT PACKAGE ################################### 

# defaults are for applied example #1
get_stat = function( original,
                     indices,
                     q, 
                     tail,
                     measure, 
                     method,
                     yi.name = yi.name, 
                     vi.name = vi.name ) {
  
  b = original[indices,]
  
  library(metafor)
  
  # keep track of whether there was an error fitting the model to these data
  # (Fisher convergence problems)
  got.error <<- FALSE
  
  tryCatch( {
    # meta-analyze the bootstrapped data
    mb = rma.uni( yi = b[[yi.name]],
                  vi = b[[vi.name]],
                  measure=measure,
                  knha = TRUE,
                  method = method,
                  # last argument helps prevent convergence problems
                  #control=list(stepadj=0.5, maxiter=1000) )
                  control=list(stepadj=0.5) )
    
    Mb = mb$b
    t2b = mb$tau2
    
    phat = suppressMessages( prop_stronger2( q = q,
                                             M = Mb,
                                             t2 = t2b,
                                             CI.level = CI.level,
                                             tail = tail,
                                             boot = "never" )$Est )  # this time don't bootstrap b/c we only need point est
    
  }, error = function(err) {
    message(err)
    # needs to be superassignment because inside fn
    errors <<- c( errors, err$message )
    got.error <<- TRUE
  } )
  
  if(got.error == FALSE) return(phat)
  
}



################################### MODIFIED FROM BOOT PACKAGE ################################### 

# see section called "MM additions"
# I minimally modified the function so that it can proceed even if some of the bootstrap iterates run into errors
# (in this case, Fisher convergence issues) because the boot package version gets confused about dimension mismatches

# source internal boot package functions
# source("bootfuns.R")

my_boot = function (data, statistic, R, sim = "ordinary", stype = c("i", 
                                                                    "f", "w"), strata = rep(1, n), L = NULL, m = 0, weights = NULL, 
                    ran.gen = function(d, p) d, mle = NULL, simple = FALSE, ..., 
                    parallel = c("no", "multicore", "snow"), ncpus = getOption("boot.ncpus", 
                                                                               1L), cl = NULL) 
{
  
  
  # MM added (to get internal functions)
  library(boot)
  
  call <- match.call()
  stype <- match.arg(stype)
  if (missing(parallel)) 
    parallel <- getOption("boot.parallel", "no")
  parallel <- match.arg(parallel)
  have_mc <- have_snow <- FALSE
  if (parallel != "no" && ncpus > 1L) {
    if (parallel == "multicore") 
      have_mc <- .Platform$OS.type != "windows"
    else if (parallel == "snow") 
      have_snow <- TRUE
    if (!have_mc && !have_snow) 
      ncpus <- 1L
    loadNamespace("parallel")
  }
  if (simple && (sim != "ordinary" || stype != "i" || sum(m))) {
    warning("'simple=TRUE' is only valid for 'sim=\"ordinary\", stype=\"i\", n=0', so ignored")
    simple <- FALSE
  }
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  n <- NROW(data)
  if ((n == 0) || is.null(n)) 
    stop("no data in call to 'boot'")
  temp.str <- strata
  strata <- tapply(seq_len(n), as.numeric(strata))
  t0 <- if (sim != "parametric") {
    if ((sim == "antithetic") && is.null(L)) 
      L <- empinf(data = data, statistic = statistic, stype = stype, 
                  strata = strata, ...)
    if (sim != "ordinary") 
      m <- 0
    else if (any(m < 0)) 
      stop("negative value of 'm' supplied")
    if ((length(m) != 1L) && (length(m) != length(table(strata)))) 
      stop("length of 'm' incompatible with 'strata'")
    if ((sim == "ordinary") || (sim == "balanced")) {
      if (boot:::isMatrix(weights) && (nrow(weights) != length(R))) 
        stop("dimensions of 'R' and 'weights' do not match")
    }
    else weights <- NULL
    if (!is.null(weights)) 
      weights <- t(apply(matrix(weights, n, length(R), 
                                byrow = TRUE), 2L, normalize, strata))
    if (!simple) 
      i <- boot:::index.array(n, R, sim, strata, m, L, weights)
    original <- if (stype == "f") 
      rep(1, n)
    else if (stype == "w") {
      ns <- tabulate(strata)[strata]
      1/ns
    }
    else seq_len(n)
    t0 <- if (sum(m) > 0L) 
      statistic(data, original, rep(1, sum(m)), ...)
    else statistic(data, original, ...)
    rm(original)
    t0
  }
  else statistic(data, ...)
  pred.i <- NULL
  fn <- if (sim == "parametric") {
    ran.gen
    data
    mle
    function(r) {
      dd <- ran.gen(data, mle)
      statistic(dd, ...)
    }
  }
  else {
    if (!simple && ncol(i) > n) {
      pred.i <- as.matrix(i[, (n + 1L):ncol(i)])
      i <- i[, seq_len(n)]
    }
    if (stype %in% c("f", "w")) {
      f <- freq.array(i)
      rm(i)
      if (stype == "w") 
        f <- f/ns
      if (sum(m) == 0L) 
        function(r) statistic(data, f[r, ], ...)
      else function(r) statistic(data, f[r, ], pred.i[r, 
                                                      ], ...)
    }
    else if (sum(m) > 0L) 
      function(r) statistic(data, i[r, ], pred.i[r, ], 
                            ...)
    else if (simple) 
      function(r) statistic(data, boot:::index.array(n, 1, sim, 
                                              strata, m, L, weights), ...)
    else function(r) statistic(data, i[r, ], ...)
  }
  RR <- sum(R)
  res <- if (ncpus > 1L && (have_mc || have_snow)) {
    if (have_mc) {
      parallel::mclapply(seq_len(RR), fn, mc.cores = ncpus)
    }
    else if (have_snow) {
      list(...)
      if (is.null(cl)) {
        cl <- parallel::makePSOCKcluster(rep("localhost", 
                                             ncpus))
        if (RNGkind()[1L] == "L'Ecuyer-CMRG") 
          parallel::clusterSetRNGStream(cl)
        res <- parallel::parLapply(cl, seq_len(RR), fn)
        parallel::stopCluster(cl)
        res
      }
      else parallel::parLapply(cl, seq_len(RR), fn)
    }
  }
  else lapply(seq_len(RR), fn)
  #t.star <- matrix(, RR, length(t0))  # ~~~ MM commented out
  
  
  # ~~~~~ MM added
  # number of non-NULL elements of the results vector
  #browser()
  RR = length(unlist(res))
  nulls = sapply( res, is.null)
  res = res[ !nulls ]
  t.star <- matrix(, RR, length(t0))
  
  # without this, boot.CI gets confused about number of replicates
  R = RR
  # ~~~~~ end of MM additions
  
  
  for (r in seq_len(RR)) t.star[r, ] <- res[[r]]
  if (is.null(weights)) 
    weights <- 1/tabulate(strata)[strata]
  boot:::boot.return(sim, t0, t.star, temp.str, R, data, statistic, 
              stype, call, seed, L, m, pred.i, weights, ran.gen, mle)
}









