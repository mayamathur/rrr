

#' Compute probability of "significance agreement" between replication and original study
#'
#' Given effect estimates and their variances for one or multiple original studies and variances for one or more
#' replication studies, 
#' returns a vector of probabilities that the replication estimate is "statistically significant" and
#' in the same direction
#' as the original. Can be computed assuming no heterogeneity or allowing for heterogeneity. 
#' @param orig.y Effect estimate in the original study. Can be a vector for multiple original studies.
#' @param orig.vy Estimated variance of effect estimate in the original study (i.e., its squared standard error).
#' Can be a vector for multiple original studies.
#' @param rep.vy Estimated variance of effect estimate in the replication study (i.e., its squared standard error).
#' Can be a vector for multiple replication studies.
#' @param t2 Optionally (if allowing for heterogeneity), the estimated variance of true effects across replication studies. 
#' @param null Null value for the hypothesis tests. 
#' @param alpha Alpha level for the hypothesis tests. 
#' @export
#' @import stats
#' @examples
#' # replication estimates (Fisher's z scale) and SEs
#' # from moral credential example in Mathur & VanderWeele
#' # (in preparation)
#' r.fis = c(0.303, 0.078, 0.113, -0.055, 0.056, 0.073,
#' 0.263, 0.056, 0.002, -0.106, 0.09, 0.024, 0.069, 0.074,
#' 0.107, 0.01, -0.089, -0.187, 0.265, 0.076, 0.082)
#' 
#' r.SE = c(0.111, 0.092, 0.156, 0.106, 0.105, 0.057,
#' 0.091, 0.089, 0.081, 0.1, 0.093, 0.086, 0.076,
#' 0.094, 0.065, 0.087, 0.108, 0.114, 0.073, 0.105, 0.04)
#' 
#' # how many do we expect to agree?
#' sum( prob_signif_agree( orig.y = 0.21, orig.vy = 0.004, rep.vy = r.SE^2 ) )


prob_signif_agree = Vectorize( function( orig.y, orig.vy, rep.vy, t2 = 0, null = 0, alpha = 0.05 ) {
  
  # check for bad input
  if (orig.vy < 0) stop("Original study's variance cannot be negative")
  if (rep.vy < 0) stop("Replication study's variance cannot be negative")
  if (t2 < 0) stop("Heterogeneity cannot be negative")
  
  pooled.SE = sqrt( 2 * t2 + orig.vy + rep.vy )
  
  crit = qnorm( 1 - (alpha/2) )
  
  # P( reject with effect in same direction as null )
  if( orig.y > null ) P = 1 - pnorm( ( crit * sqrt( rep.vy ) + null - orig.y ) / pooled.SE )
  else P = pnorm( ( -crit * sqrt( rep.vy ) + null - orig.y ) / pooled.SE )
  
  return(P)
}, vectorize.args = c( "orig.y", "orig.vy", "rep.vy", "null" ) )





#' Compute prediction interval for replication study given original
#'
#' Given effect estimates and their variances for one or multiple original studies and one or more replication studies, 
#' returns a vector stating whether each replication estimate is in its corresponding prediction interval. Assumes no heterogeneity.
#' @param orig.y Effect estimate in the original study. Can be a vector for multiple original studies.
#' @param orig.vy Estimated variance of effect estimate in the original study (i.e., its squared standard error).
#' Can be a vector for multiple original studies.
#' @param rep.y Effect estimate in the replication study.
#' Can be a vector for multiple replication studies. Can be omitted, in which case function returns only the prediction
#' interval. 
#' @param rep.vy Estimated variance of effect estimate in the replication study (i.e., its squared standard error).
#' Can be a vector for multiple replication studies.
#' @param level Coverage level of prediction interval. Typically 0.95.
#' @export
#' @import stats
#' @examples
#' # calculate prediction interval for a single replication study
#' pred_int( orig.y = 1, orig.vy = .5, rep.y = 0.6,
#' rep.vy = .2 )
#' 
#' # calculate prediction intervals for a one-to-one design
#' pred_int( orig.y = c(1, 1.3), orig.vy = c(.01, .6),
#' rep.y = c(.6, .7), rep.vy = c(.01,.3) )
#' 
#' # no need to pass rep.y if you only want the intervals
#' pred_int( orig.y = c(1, 1.3), orig.vy = c(.01, .6),
#' rep.vy = c(.01,.3) )
#' 
#' # calculate prediction intervals for a many-to-one design
#' pred_int( orig.y = c(1), orig.vy = c(.01), rep.y = c(.6, .7), rep.vy = c(.01,.3) )

pred_int = function( orig.y, orig.vy, rep.y = NULL, rep.vy, level = 0.95 ) {
  
  # check for bad input
  if ( any( orig.vy < 0) ) stop("Original study's variance cannot be negative")
  if ( any(rep.vy < 0) ) stop("Replication study's variance cannot be negative")
  
  # check that we have a sensible number of originals and replications
  # should represent either a 1-1 design or a many-to-one design 
  if ( ! is.null(rep.y) ) {
    one.to.one = ( max( length(orig.y), length(orig.vy), length(rep.y), length(rep.vy) ) == 
                     min( length(orig.y), length(orig.vy), length(rep.y), length(rep.vy) ) )
    many.to.one = ( length(orig.y) == 1 ) & ( length(orig.vy) == 1 ) & ( length(rep.y) == length(rep.vy) )
  } else {
    one.to.one = ( max( length(orig.y), length(orig.vy), length(rep.vy) ) == 
                  min( length(orig.y), length(orig.vy), length(rep.vy) ) )
    many.to.one = ( length(orig.y) == 1 ) & ( length(orig.vy) == 1 ) 
  }
  
  if ( one.to.one == FALSE & many.to.one == FALSE ) {
    stop( "\nLengths of arguments do not make sense. For a one-to-one design, all arguments should have same length. 
          For many-to-one design, first two arguments should be length 1 and second two arguments should have
          same length as each other.")
  }

  # compute the pred interval
  pooled.SE = sqrt( orig.vy + rep.vy )
  alpha = 1 - level
  lo = orig.y - qnorm(1 - alpha/2) * pooled.SE
  hi = orig.y + qnorm(1 - alpha/2) * pooled.SE
  
  # check if the replication is in the prediction interval
  if ( ! is.null(rep.y) ) {
    rep.inside = (rep.y > lo) & (rep.y < hi)
  } else rep.inside = NA
  
  return( list( int.lo = lo, int.hi = hi, rep.inside = rep.inside ) )
  
}




#' Statistical consistency of original study with replication
#'
#' Given the original study's effect estimate and its variance, the estimated average true effect size in the 
#' replications, and the estimated heterogeneity in the replications, computes estimated probability that 
#' the original study would have an effect estimate at least as extreme as the observed value if the original
#' and the replications in fact are statistically consistent. Allows for heterogeneity. 
#' @param orig.y Effect estimate in the original study. 
#' @param orig.vy Estimated variance of effect estimate in the original study (i.e., its squared standard error).
#' @param yr Estimated average true effect size in the replications. 
#' @param t2 Estimated heterogeneity of true effect sizes in the replications. 
#' @param vyr Estimated variance of \code{yr} (i.e., its squared standard error).
#' @export
#' @import metafor
#' stats
#' @details
#' \code{yr}, \code{vyr}, and \code{t2} can be estimated through, for example, random-effects meta-analysis or
#' a mixed model fit to the individual subject data. See Mathur & VanderWeele (Appendix) for details of how to specify
#' such models.  
#' @examples
#' # replication estimates (Fisher's z scale) and SEs
#' # from moral credential example in Mathur and VanderWeele
#' # (in preparation)
#' r.fis = c(0.303, 0.078, 0.113, -0.055, 0.056, 0.073,
#' 0.263, 0.056, 0.002, -0.106, 0.09, 0.024, 0.069, 0.074,
#' 0.107, 0.01, -0.089, -0.187, 0.265, 0.076, 0.082)
#' 
#' r.SE = c(0.111, 0.092, 0.156, 0.106, 0.105, 0.057,
#' 0.091, 0.089, 0.081, 0.1, 0.093, 0.086, 0.076,
#' 0.094, 0.065, 0.087, 0.108, 0.114, 0.073, 0.105, 0.04)
#' 
#' # meta-analyze the replications
#' library(metafor)
#' m = rma.uni( yi = r.fis, vi = r.SE^2, measure = "ZCOR" ) 
#' 
#' p_orig( orig.y = 0.210, orig.vy = 0.062^2, 
#' yr = m$b, t2 = m$se.tau2^2,  vyr = m$vb )
 
p_orig = function( orig.y, orig.vy, yr, t2, vyr ) {

  # check for bad input
  if (orig.vy < 0) stop("Original study's variance cannot be negative")
  if (vyr < 0) stop("Replication point estimate's variance cannot be negative")
  if (t2 < 0) stop("Heterogeneity cannot be negative")
  
  denom = sqrt( t2 + orig.vy + vyr )
  Z = ( abs( orig.y - yr ) ) / denom
  
  pval = as.numeric( 2 * ( 1 - pnorm(Z) ) )
  message("\nIf the original study were statistically consistent with the replications,
          the probability of an estimate in the original study as extreme or more extreme than actually observed is approximately:\n")
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




