

#' Compute marginal log-likelihood of tau^2
#'
#' Given point estimates and their variances, returns the marginal restricted log-likelihood of a specified tau^2
#' per Veroniki AA, et al. (2016), Section 3.11. Useful as a diagnostic for \code{p_orig} per Mathur VanderWeele (under review).
#' @param yi Vector of point estimates 
#' @param vi Vector of variances of point estimates 
#' Can be a vector for multiple replication studies.
#' @param t2 Heterogeneity value (tau^2) at which to compute the marginal log-likelihood
#' @export
#' @import
#' stats
#' ggplot2
#' metafor
#' @references 
#' 1. Veroniki AA et al. (2016). Methods to estimate the between-study variance and its uncertainty in meta-analysis. \emph{Research Synthesis Methods.}
#'
#' 2. Mathur MB & VanderWeele TJ (under review). New statistical metrics for multisite replication projects.
#' @examples
#' # replication estimates (Fisher's z scale) and SEs
#' # from moral credential example in Mathur & VanderWeele
#' # (under review)
#' yir = c(0.303, 0.078, 0.113, -0.055, 0.056, 0.073,
#'         0.263, 0.056, 0.002, -0.106, 0.09, 0.024, 0.069, 0.074,
#'         0.107, 0.01, -0.089, -0.187, 0.265, 0.076, 0.082)
#' 
#' seir = c(0.111, 0.092, 0.156, 0.106, 0.105, 0.057,
#'          0.091, 0.089, 0.081, 0.1, 0.093, 0.086, 0.076,
#'          0.094, 0.065, 0.087, 0.108, 0.114, 0.073, 0.105, 0.04)
#' 
#' vir = seir^2
#' 
#' # fit meta-analysis
#' .m = metafor::rma.uni( yi = yir,
#'               vi = vir,
#'               knha = TRUE ) 
#' 
#' # vector and list of tau^2 at which to compute the log-likelihood
#' t2.vec = seq(0, .m$tau2*10, .001)
#' t2l = as.list(t2.vec)
#' 
#' # compute the likelihood ratio vs. the MLE for each tau^2 in t2l
#' temp = lapply( t2l,
#'                FUN = function(t2) {
#'                  # log-lkl itself
#'                  t2_lkl( yi = yir,
#'                          vi = vir,
#'                          t2 = t2 )
#'                  
#'                  # lkl ratio vs. the MLE
#'                  exp( t2_lkl( yi = yir,
#'                               vi = vir,
#'                               t2 = t2 ) ) / exp( t2_lkl( yi = yir,
#'                                                          vi = vir,
#'                                                          t2 = .m$tau2 ) )
#'                })
#' 
#' # plotting dataframe
#' dp = data.frame( tau = sqrt(t2.vec),
#'                  V = t2.vec,
#'                  lkl = unlist(temp) )
#' 
#' # fn: ratio of the plotted tau^2 vs. the actual MLE (for secondary x-axis)
#' g = function(x) x / .m$tau2
#' 
#' # breaks for main and secondary x-axes
#' breaks.x1 = seq( 0, max(dp$V), .005 )
#' breaks.x2 = seq( 0, max( g(dp$V) ), 1 )
#' 
#' p = ggplot2::ggplot( data = dp,
#'         ggplot2::aes(x = V, 
#'             y = lkl ) ) +
#'   
#'   ggplot2::geom_vline(xintercept = .m$tau2,
#'              lty = 2,
#'              color = "red") +  # the actual MLE
#'   
#'   ggplot2::geom_line(lwd = 1.2) +
#'   ggplot2::theme_classic() +
#'   ggplot2::xlab( bquote( hat(tau)["*"]^2 ) ) +
#'   ggplot2::ylab( "Marginal likelihood ratio of " ~ hat(tau)["*"]^2 ~ " vs. " ~ hat(tau)^2 ) +
#'   ggplot2::scale_x_continuous( limits = c(0, max(breaks.x1)),
#'                       breaks = breaks.x1,
#'                       sec.axis = ggplot2::sec_axis( ~ g(.),  
#'                                            name = bquote( hat(tau)["*"]^2 / hat(tau)^2 ),
#'                                            breaks=breaks.x2 ) ) +
#'   ggplot2::scale_y_continuous( limits = c(0,1),
#'                       breaks = seq(0,1,.1) )
#' graphics::plot(p)


t2_lkl = function( yi, vi, t2 ) {
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



#' Compute probability of "significance agreement" between replication and original study
#'
#' Given point estimates and their variances for one or multiple original studies and variances for one or more
#' replication studies, 
#' returns a vector of probabilities that the replication estimate is "statistically significant" and
#' in the same direction
#' as the original. Can be computed assuming no heterogeneity or allowing for heterogeneity. 
#' @param yio Effect estimate in the original study. Can be a vector for multiple original studies.
#' @param vio Estimated variance of effect estimate in the original study (i.e., its squared standard error).
#' Can be a vector for multiple original studies.
#' @param vir Estimated variance of effect estimate in the replication study (i.e., its squared standard error).
#' Can be a vector for multiple replication studies.
#' @param t2 Optionally (if allowing for heterogeneity), the estimated variance of true effects across replication studies. 
#' @param null Null value for the hypothesis tests. 
#' @param alpha Alpha level for the hypothesis tests. 
#' @export
#' @import stats
#' @references 
#' 1. Mathur MB & VanderWeele TJ (under review). New statistical metrics for multisite replication projects.
#' @examples
#' # replication estimates (Fisher's z scale) and SEs
#' # from moral credential example in Mathur & VanderWeele
#' # (under review)
#' yir = c(0.303, 0.078, 0.113, -0.055, 0.056, 0.073,
#' 0.263, 0.056, 0.002, -0.106, 0.09, 0.024, 0.069, 0.074,
#' 0.107, 0.01, -0.089, -0.187, 0.265, 0.076, 0.082)
#' 
#' seir = c(0.111, 0.092, 0.156, 0.106, 0.105, 0.057,
#' 0.091, 0.089, 0.081, 0.1, 0.093, 0.086, 0.076,
#' 0.094, 0.065, 0.087, 0.108, 0.114, 0.073, 0.105, 0.04)
#' 
#' # how many do we expect to agree?
#' sum( prob_signif_agree( yio = 0.21, vio = 0.004, vir = seir^2 ) )


prob_signif_agree = Vectorize( function( yio, vio, vir, t2 = 0, null = 0, alpha = 0.05 ) {
  
  # check for bad input
  if (vio < 0) stop("Original study's variance cannot be negative")
  if (vir < 0) stop("Replication study's variance cannot be negative")
  if (t2 < 0) stop("Heterogeneity cannot be negative")
  
  pooled.SE = sqrt( 2 * t2 + vio + vir )
  
  crit = qnorm( 1 - (alpha/2) )
  
  # P( reject with effect in same direction as null )
  if( yio > null ) P = 1 - pnorm( ( crit * sqrt( vir ) + null - yio ) / pooled.SE )
  else P = pnorm( ( -crit * sqrt( vir ) + null - yio ) / pooled.SE )
  
  return(P)
}, vectorize.args = c( "yio", "vio", "vir", "null" ) )





#' Compute prediction interval for replication study given original
#'
#' Given point estimates and their variances for one or multiple original studies and one or more replication studies, 
#' returns a vector stating whether each replication estimate is in its corresponding prediction interval. Assumes no heterogeneity.
#' @param yio Effect estimate in the original study. Can be a vector for multiple original studies.
#' @param vio Estimated variance of effect estimate in the original study (i.e., its squared standard error).
#' Can be a vector for multiple original studies.
#' @param yir Effect estimate in the replication study.
#' Can be a vector for multiple replication studies. Can be omitted, in which case function returns only the prediction
#' interval. 
#' @param vir Estimated variance of effect estimate in the replication study (i.e., its squared standard error).
#' Can be a vector for multiple replication studies.
#' @param level Coverage level of prediction interval. Typically 0.95.
#' @export
#' @import stats
#' @examples
#' # calculate prediction interval for a single replication study
#' pred_int( yio = 1, vio = .5, yir = 0.6,
#' vir = .2 )
#' 
#' # calculate prediction intervals for a one-to-one design
#' pred_int( yio = c(1, 1.3), vio = c(.01, .6),
#' yir = c(.6, .7), vir = c(.01,.3) )
#' 
#' # no need to pass yir if you only want the intervals
#' pred_int( yio = c(1, 1.3), vio = c(.01, .6),
#' vir = c(.01,.3) )
#' 
#' # calculate prediction intervals for a many-to-one design
#' pred_int( yio = c(1), vio = c(.01), yir = c(.6, .7), vir = c(.01,.3) )

pred_int = function( yio, vio, yir = NULL, vir, level = 0.95 ) {
  
  # check for bad input
  if ( any( vio < 0) ) stop("Original study's variance cannot be negative")
  if ( any(vir < 0) ) stop("Replication study's variance cannot be negative")
  
  # check that we have a sensible number of originals and replications
  # should represent either a 1-1 design or a many-to-one design 
  if ( ! is.null(yir) ) {
    one.to.one = ( max( length(yio), length(vio), length(yir), length(vir) ) == 
                     min( length(yio), length(vio), length(yir), length(vir) ) )
    many.to.one = ( length(yio) == 1 ) & ( length(vio) == 1 ) & ( length(yir) == length(vir) )
  } else {
    one.to.one = ( max( length(yio), length(vio), length(vir) ) == 
                  min( length(yio), length(vio), length(vir) ) )
    many.to.one = ( length(yio) == 1 ) & ( length(vio) == 1 ) 
  }
  
  if ( one.to.one == FALSE & many.to.one == FALSE ) {
    stop( "\nLengths of arguments do not make sense. For a one-to-one design, all arguments should have same length. 
          For many-to-one design, first two arguments should be length 1 and second two arguments should have
          same length as each other.")
  }

  # compute the pred interval
  pooled.SE = sqrt( vio + vir )
  alpha = 1 - level
  lo = yio - qnorm(1 - alpha/2) * pooled.SE
  hi = yio + qnorm(1 - alpha/2) * pooled.SE
  
  # check if the replication is in the prediction interval
  if ( ! is.null(yir) ) {
    rep.inside = (yir > lo) & (yir < hi)
  } else rep.inside = NA
  
  return( list( int.lo = lo, int.hi = hi, rep.inside = rep.inside ) )
  
}




#' Statistical consistency of original study with replication
#'
#' Given the original study's effect estimate and its variance, the estimated average true effect size in the 
#' replications, and the estimated heterogeneity in the replications, computes estimated probability that 
#' the original study would have an effect estimate at least as extreme as the observed value if the original
#' and the replications in fact are statistically consistent. Allows for heterogeneity. 
#' @param yio Effect estimate in the original study. 
#' @param vio Estimated variance of effect estimate in the original study (i.e., its squared standard error).
#' @param yr Estimated average true effect size in the replications. 
#' @param t2 Estimated heterogeneity of true effect sizes in the replications. 
#' @param vyr Estimated variance of \code{yr} (i.e., its squared standard error).
#' @export
#' @import metafor
#' stats
#' @details
#' \code{yr}, \code{vyr}, and \code{t2} can be estimated through, for example, random-effects meta-analysis or
#' a mixed model fit to the individual subject data. See Mathur & VanderWeele's (under review) Appendix for details of how to specify
#' such models.  
#' @references 
#' 1. Mathur MB & VanderWeele TJ (under review). New statistical metrics for multisite replication projects.
#' @examples
#' # replication estimates (Fisher's z scale) and SEs
#' # from moral credential example in Mathur and VanderWeele
#' # (under review)
#' yir = c(0.303, 0.078, 0.113, -0.055, 0.056, 0.073,
#' 0.263, 0.056, 0.002, -0.106, 0.09, 0.024, 0.069, 0.074,
#' 0.107, 0.01, -0.089, -0.187, 0.265, 0.076, 0.082)
#' 
#' seir = c(0.111, 0.092, 0.156, 0.106, 0.105, 0.057,
#' 0.091, 0.089, 0.081, 0.1, 0.093, 0.086, 0.076,
#' 0.094, 0.065, 0.087, 0.108, 0.114, 0.073, 0.105, 0.04)
#' 
#' # meta-analyze the replications
#' m = metafor::rma.uni( yi = yir, vi = seir^2, measure = "ZCOR" ) 
#' 
#' p_orig( yio = 0.210, vio = 0.062^2, 
#' yr = m$b, t2 = m$se.tau2^2,  vyr = m$vb )
 
p_orig = function( yio, vio, yr, t2, vyr ) {

  # check for bad input
  if (vio < 0) stop("Original study's variance cannot be negative")
  if (vyr < 0) stop("Replication point estimate's variance cannot be negative")
  if (t2 < 0) stop("Heterogeneity cannot be negative")
  
  denom = sqrt( t2 + vio + vyr )
  Z = ( abs( yio - yr ) ) / denom
  
  pval = as.numeric( 2 * ( 1 - pnorm(Z) ) )
  return(pval)
}



#' Probability of true effect stronger than threshold of scientific importance
#'
#' This function is now deprecated. You should use \code{MetaUtility::prop_stronger} instead, which 
#' provides better, more general functionality. The below documentation is temporarily maintained before 
#' the \code{stronger_than} function is deprecated.
#' 
#' Given the original study's effect estimate and its variance, the estimated average true effect size in the 
#' replications, and the estimated heterogeneity in the replications, computes estimated probability that 
#' the original study would have an effect estimate at least as extreme as the observed value if the original
#' and the replications in fact are statistically consistent. Allows for heterogeneity. 
#' @param q True effect size that is the threshold for "scientific importance"
#' @param yr Average true effect estimated using replications
#' @param vyr Estimated variance of above estimate
#' @param t2 Heterogeneity of true effects estimated using replications
#' @param vt2 Estimated variance of above estimate
#' @param CI.level Confidence level as a proportion
#' @param tail \code{above} for the probability of an effect above \code{q}; \code{below} for
#' the probability of an effect below \code{q}.
#' @name stronger_than-deprecated
#' @seealso \code{\link{Replicate-deprecated}}
#' @keywords internal
#' @export

stronger_than = function( q, yr, vyr=NULL, t2, vt2=NULL,
                          CI.level=0.95, tail ) {
  .Deprecated("MetaUtility::prop_stronger")
  
  # # check for bad input
  # if ( !is.null(vyr) ) {
  #   if (vyr < 0) stop("Replication point estimate's variance cannot be negative")
  # }
  # 
  # if ( !is.null(vt2) ) {
  #   if (vt2 < 0) stop("Heterogeneity estimate's variance cannot be negative")
  # }
  # 
  # if (t2 < 0) stop("Heterogeneity cannot be negative")
  # 
  # 
  # if( tail == "above" ) {
  #   prob = as.numeric( 1 - pnorm( ( q - yr ) / sqrt(t2) ) )
  # }
  # 
  # if( tail == "below" ) {
  #   prob = as.numeric( pnorm( ( q - yr ) / sqrt(t2) ) )
  # }
  # 
  # # SE
  # term1 = sqrt( ( vyr / t2 ) + ( vt2 * ( q - yr )^2 ) / ( 4 * t2^3 ) )
  # term2 = dnorm( ( q - yr ) / sqrt(t2) )
  # SE = as.numeric( term1 * term2 )
  # 
  # # confidence interval
  # crit = abs( qnorm( (1 - CI.level)/2 ) )
  # CI.lo = max( 0, prob - crit * SE )
  # CI.hi = min( 1, prob + crit * SE )
  # 
  # return( list( prob = prob, SE = SE, CI.lo = CI.lo, CI.hi = CI.hi ) ) 
}




