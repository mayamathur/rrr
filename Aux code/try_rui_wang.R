
######################### HELPER #########################

my_ens = function(yi,
                  sei ) {
  
  meta = rma.uni( yi = yi, 
                  sei = sei, 
                  method = "DL" )
  
  muhat = meta$b
  t2 = meta$tau2
  
  # return ensemble estimates
  c(muhat) + ( c(t2) / ( c(t2) + sei^2 ) )^(1/2) * ( yi - c(muhat) )
}

######################### READ IN CREDENTIALS DATA #########################

library(metafor)
library(purrr)
library(dplyr)
library(ggplot2)
library(MetaUtility)

# read in ML3 data
# to reproduce, use the Credentials file in OSF repo
setwd("~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)/Manuscript with embedded analyses/Applied example data")
d = read.csv( "Credentials.csv", header = TRUE )

# as before
# convert main effect from Pearson r to Fisher's z
library(MetaUtility)
d$main.fis = r_to_z( d$rMain )
# add SE of Fisher's z
d$main.SE = sqrt( 1 / ( d$NT - 3 ) )


######################### APPLY RUI'S CODE OFF THE SHELF #########################

setwd("~/Dropbox/Personal computer/Miscellaneous/Statistics articles/Meta-analysis/Rui Wang's paper on inference for percentiles of meta-analysis Phat")
source("rui_wang_code_supplement.R")

# find CI for the 80th percentile
pct = 0.8
CI = searchci( theta = d$main.fis,
          theta.sd = d$main.SE,
          alpha = 0.05,
          pct = pct,
          nperm = 2000 )

# middle of CI: 15%
mean(CI)

# value with largest p-val
( Q.vec = seq( CI[1], CI[2], by = 0.005 ) )

pvals = Q.vec %>% map( function(x) phi( theta = d$main.fis,
                                        theta.sd = d$main.SE,
                                        mu = x,
                                        pct = pct,
                                        nperm=2000 ) ) %>%
                  unlist # return a double-type vector instead of list

# ** given the way the p-values are asymmetrical (i.e., likelihood is asymmetrical), 
#  the point estimate is not in the middle of the CI
est = Q.vec[ which.max( pvals ) ]

# ** plot the Q's vs. the p-values
ggplot( data = data.frame( Q = Q.vec, pval = pvals ),
        aes( x = Q, y = pval ) ) +
  geom_vline( xintercept = CI[1], color = "red" ) +
  geom_vline( xintercept = CI[2], color = "red" ) +
  geom_vline( xintercept = est, color = "red" ) +
  geom_point() + 
  theme_minimal()


##### Compare to Other Ways of Getting Point Estimate #####

# observed 80th percentile: 10% (obviously a bad estimate)
quantile( d$main.fis, probs = Q )

# ensemble estimate of 80th percentile: 8.5%
quantile( my_ens(yi = d$main.fis,
                 sei = d$main.SE), probs = Q )




######################### ANALOG FOR PHAT #########################


# warmup: p-values for the proportion below q = 0.1
# parametrically, got 28% (0%, 63%) for proportion ABOVE
q = 0.18
pct.vec = seq( 0, 1, 0.001 )

pvals = pct.vec %>% map( function(x) phi( theta = d$main.fis,
                                        theta.sd = d$main.SE,
                                        mu = q,
                                        pct = x,
                                        nperm=2000 ) ) %>%
  unlist # return a double-type vector instead of list

( Phat.NP = pct.vec[ which.max( pvals ) ] )
# sloppy version of CI
( CI.lo.NP = pct.vec[ pct.vec < Phat.NP ][ which.min( abs( pvals[ pct.vec < Phat.NP ] - 0.05 ) ) ] )
( CI.hi.NP = pct.vec[ pct.vec > Phat.NP ][ which.min( abs( pvals[ pct.vec > Phat.NP ] - 0.05 ) ) ] )


# compare estimate and CI to parametric one for proportion below
meta = rma.uni( yi = d$main.fis,
                sei = d$main.SE, 
                method = "REML" )

# for bootstrap
d$vi = d$main.SE^2
( Phat.ML = prop_stronger( q = q,
               M = meta$b,
               t2 = meta$tau2,
               se.M = meta$se,
               se.t2 = meta$se.tau2,
               tail = "below",
               
               dat = d, 
               yi.name = "main.fis",
               vi.name = "vi" ) )

# ** plot the pcts vs. the p-values
ggplot( data = data.frame( pct = pct.vec, pval = pvals ),
        aes( x = pct, y = pval ) ) +
  
  # nonparametric ones
  geom_vline( xintercept = CI.lo.NP, color = "red", lty = 2 ) +
  geom_vline( xintercept = CI.hi.NP, color = "red", lty = 2 ) +
  geom_hline( yintercept = 0.05, lty = 2, color = "gray" ) + # alpha
  geom_vline( xintercept = Phat.NP, color = "red" ) +
  
  # parametric ones
  geom_vline( xintercept = Phat.ML$lo, color = "blue", lty = 2 ) +
  geom_vline( xintercept = Phat.ML$hi, color = "blue", lty = 2 ) +
  geom_vline( xintercept = Phat.ML$Est, color = "blue" ) +
  
  geom_point() + 
  theme_minimal()

# HUH...NP is much narrower! (for q = 0.1, for which prop_stronger doesn't bootstrap)

# Next up: Adjust Rui's code directly to do more principled grid search.
#  Hopefully will be similar to above. 








