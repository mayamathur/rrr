


############################# LOAD FUNCTIONS AND DATA ############################# 

library(MetaUtility)
library(metafor)
library(ggplot2)

code.dir = "~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)/Other RRR code (git)/Applied example"
data.dir = "~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)/Applied example data"
# write results straight to Overleaf
results.dir = "~/Dropbox/Apps/Overleaf/RRR Manuscript (JRSSA_2)/R_objects"

setwd(code.dir)
source("functions_applied_RRR.R")

# read in ML3 data
# to reproduce, use the Credentials file in OSF repo
setwd(data.dir)
d = read.csv( "Credentials.csv", header = TRUE )

# bootstrap from scratch or read in existing results for reproducibility?
rerun.Phat.from.scratch = FALSE



############################# 1. PREPARE ORIGINAL-STUDY EFFECT SIZES ############################# 

# compute effect size for original from text of ML3
# "Monin and Miller (2001) observed a main effect of moral credentials, F(2, 194) = 4.4,
#  p = .014, eta^2 = .043, 95% CI = [.002, .103]"
# from Lakens paper (http://journal.frontiersin.org/article/10.3389/fpsyg.2013.00863/full): 
# eta^2 = r^2 (former is a generalization)
orig.eta2 = 0.043

# convert to Fisher's z
orig.fis = r_to_z( sqrt( orig.eta2 ) )

# approximate its SE via conversion of the CI to Fisher's z scale
# since Fisher's z is approximately normal, unlike eta^2
orig.hi.eta2 = 0.103  # CI upper bound on eta^2 scale
orig.SE = ( r_to_z( sqrt( orig.hi.eta2 ) ) - r_to_z( sqrt( orig.eta2 ) ) ) / qnorm(0.975)
# this SE is also on Fisher's z scale

# p-value of original
orig.pval = 2 * ( 1 - pnorm( orig.fis / orig.SE ) )


update_result_csv( "Orig Fisher",
                   section = 1,
                   value = round( orig.fis, 2 ),
                   print = FALSE )
update_result_csv( "Orig SE",
                   section = 1,
                   value = round( orig.SE, 2 ),
                   print = FALSE )
update_result_csv( "Orig r lo",
                   section = 1,
                   value = round( z_to_r( orig.fis - qnorm(0.975) * orig.SE ), 2 ),
                   print = FALSE )
update_result_csv( "Orig r hi",
                   section = 1,
                   value = round( z_to_r( orig.fis + qnorm(0.975) * orig.SE ), 2 ),
                   print = FALSE )
update_result_csv( "Orig pval",
                   section = 1,
                   value = format_stat( orig.pval ),
                   print = TRUE )

############################# 2. PREPARE REPLICATION EFFECT SIZES ############################# 

# convert main effect from Pearson r to Fisher's z
d$main.fis = r_to_z( d$rMain )

# add SE of Fisher's z
d$main.SE = sqrt( 1 / ( d$NT - 3 ) )

# add p-value
d$pval = 1 - pnorm( abs( d$main.fis ) / d$main.SE )

# add squared SE for use with get_stat (bootstrapping)
d$vi = d$main.SE^2


############################# 3. META-ANALYZE MORAL CREDENTIALING DATA ############################# 

m = rma.uni( yi = d$main.fis,
             vi = main.SE^2,
             data = d,
             measure = "ZCOR",
             knha = TRUE ) 
# I^2 = 28% vs. reported: 40%

# reproduce Ebersole's meta-analysis from "ML3 Meta Script"
# he used the raw correlation instead of Fisher's z
# Model<-escalc(ri=rInter,ni=NT,data=d,measure="UCOR",vtype="UB")
# Model2<-rma.uni(yi,vi,data=Model)
# HAVING TROUBLE INSTALLING REQUIRED DEPENDENCY (PACKAGE gsl) TO USE UCOR TYPE 

om3 = round( z_to_r(m$b), 2 )
om3.lo = round( z_to_r(m$ci.lb), 2 )
om3.hi = round( z_to_r(m$ci.ub), 2 )


# check normality assumption
# warning about recycling is expected
std.z = (d$main.fis - m$b) / sqrt(m$tau2 + d$vi)
norm.test = shapiro.test(std.z)
qqnorm(std.z)


update_result_csv( "Reps pooled r",
                   section = 3,
                   value = om3,
                   print = FALSE )
update_result_csv( "Reps pooled r lo",
                   section = 3,
                   value = om3.lo,
                   print = FALSE )
update_result_csv( "Reps pooled r hi",
                   section = 3,
                   value = om3.hi,
                   print = FALSE )
update_result_csv( "Reps pooled z",
                   section = 3,
                   value = round(m$b, 2),
                   print = FALSE )
update_result_csv( "Reps pooled z SE",
                   section = 3,
                   value = round(m$se, 2),
                   print = FALSE )
update_result_csv( "Reps tau2",
                   section = 3,
                   value = round(m$tau2, 3),
                   print = FALSE )
update_result_csv( "Reps Shapiro pval",
                   section = 3,
                   value = format_stat(norm.test$p.value, 2),
                   print = TRUE )



############################# 4. OLD METRIC #1: PROPORTION P-VALUES IN AGREEMENT ############################# 

# compute old metric #1
om1 = 100 * round( sum( d$pval < 0.05 & d$rMain > 0 ) / nrow(d), 2 )
# 24% agreement in p-values

# now do it with normalized version (as in Appendix)
# null is 0
library(Replicate)
probs = prob_signif_agree( orig.y = orig.fis,
                           orig.vy = orig.SE^2,
                           rep.vy = d$main.SE^2,
                           t2 = 0,
                           null = 0 )
expected = 100 * round( mean(probs), 2 )
# 62%

# conclusion: apparently much lower p-value agreement than we'd expect by chance


update_result_csv( "Obs signif agree",
                   section = 3,
                   value = om1,
                   print = FALSE )
update_result_csv( "Exp signif agree",
                   section = 3,
                   value = expected,
                   print = TRUE )

############################# 5. OLD METRIC #2: PROPORTION REPLICATIONS IN PREDICTION INTERVAL ############################# 

rep.inside = pred_int( orig.y = orig.fis,
                       orig.vy = orig.SE^2,
                       rep.y = d$main.fis,
                       rep.vy = d$main.SE^2 )$rep.inside

om2 = 100 * round( prop.table(table(rep.inside))[2], 2 )
# 76% inside vs. 95% expected

update_result_csv( "Obs pred int",
                   section = 3,
                   value = round( 100*mean(rep.inside) ),
                   print = FALSE )


############################# 6. NEW METRIC #1: CONSISTENCY OF ORIGINAL WITH HETEROGENEOUS RRR DISTRIBUTION ############################# 

# # report the statistics that go into the metric
# mu.hat = round( m$b, 2 )
# se.mu.hat = round( sqrt( m$vb ), 2 )
# t2 = round( m$tau2, 4 )


# compute metric
new1 = 100 * round( p_orig( orig.y = orig.fis,
                            orig.vy = orig.SE^2,
                            yr = m$b,
                            t2 = m$tau2,
                            vyr = m$vb ), 2 )
# 10%

# show how it becomes less generous if no heterogeneity
new1.no.het = 100 * round( p_orig( orig.y = orig.fis,
                                   orig.vy = orig.SE^2,
                                   yr = m$b,
                                   t2 = 0,
                                   vyr = m$vb ), 2 )
# 3%
# this helps explain why the prediction intervals looked pretty bad


update_result_csv( "Porig",
                   section = 6,
                   value = new1,
                   print = FALSE )
update_result_csv( "Porig no het",
                   section = 6,
                   value = new1.no.het,
                   print = FALSE )


############################# DIAGNOSTIC PLOTS FOR PORIG ############################# 

# ~~~ CHECK THE UNDERLYING CODE HERE
plots = diag_plots( yi = d$main.fis,
                    vi = d$vi,
                    yi.orig = orig.fis,
                    vi.orig = orig.SE^2)
# 6 x 4 for each plot

# with fewer replications to illustrate
plots2 = diag_plots( yi = d$main.fis[1:3],
                     vi = d$vi[1:3],
                     yi.orig = orig.fis,
                     vi.orig = orig.SE^2)

# with lots of replications
source("~/Dropbox/Personal computer/Independent studies/RRR estimators/Linked to OSF (RRR)/Other RRR code (git)/Simulation study/functions_RRR.R")
fake = sim_data( k = 500,
                 mu = m$b,
                 V = 0.001,
                 muN = round( mean(d$NT) ),
                 minN = min(d$NT),
                 sd.w = 1,  # ~~~ update this
                 true.effect.dist = "normal"
                 )

plots2 = diag_plots( yi = fake$yi,
                     vi = fake$vyi,
                     yi.orig = orig.fis,
                     vi.orig = orig.SE^2)


############################# 7. NEW METRIC #2: PROPORTION EFFECT SIZES ABOVE THRESHOLD ############################# 

###### P>0 #####
# CI limits may change a bit with different bootstrapping iterates
# so read in previous results for reproducibility
if ( rerun.Phat.from.scratch == TRUE ) {
  phat = MetaUtility::prop_stronger( 
    q = 0,
    estimate.method = "calibrated",
    ci.method = "calibrated",
    tail = "above",
    dat = d,
    R = 5000,
    yi.name = "main.fis",
    vi.name = "vi" )
  
  # save stochastic results
  setwd(results.dir)
  write.csv( phat, "Phat_above_0_stochastic_results.csv", row.names = FALSE )
} else {
  setwd(results.dir)
  phat = read.csv( "Phat_above_0_stochastic_results.csv" )
}

update_result_csv( "Perc>0 est",
                   section = 7,
                   value = format_stat( 100*phat$Est, 0 ),
                   print = FALSE )
update_result_csv( "Perc>0 lo",
                   section = 7,
                   value = format_stat( 100*phat$lo, 0 ),
                   print = FALSE )
update_result_csv( "Perc>0 hi",
                   section = 7,
                   value = format_stat( 100*phat$hi, 0 ),
                   print = TRUE )


###### P>0.10 #####
if ( rerun.Phat.from.scratch == TRUE ) {
  phat = MetaUtility::prop_stronger( 
    q = 0.10,
    estimate.method = "calibrated",
    ci.method = "calibrated",
    tail = "above",
    dat = d,
    R = 5000,
    yi.name = "main.fis",
    vi.name = "vi" )
  
  # save stochastic results
  setwd(results.dir)
  write.csv( phat, "Phat_above_0.10_stochastic_results.csv", row.names = FALSE )
} else {
  setwd(results.dir)
  phat = read.csv( "Phat_above_0.10_stochastic_results.csv" )
}

update_result_csv( "Perc>0.1 est",
                   section = 7,
                   value = format_stat( 100*phat$Est, 0 ),
                   print = FALSE )
update_result_csv( "Perc>0.1 lo",
                   section = 7,
                   value = round( 100*phat$lo, 0 ),
                   print = FALSE )
update_result_csv( "Perc>0.1 hi",
                   section = 7,
                   value = format_stat( 100*phat$hi, 0 ),
                   print = TRUE )

###### P>0.15 #####
if ( rerun.Phat.from.scratch == TRUE ) {
  phat = MetaUtility::prop_stronger( 
    q = 0.15,
    estimate.method = "calibrated",
    ci.method = "calibrated",
    tail = "above",
    dat = d,
    R = 5000,
    yi.name = "main.fis",
    vi.name = "vi" )
  
  # save stochastic results
  setwd(results.dir)
  write.csv( phat, "Phat_above_0.15_stochastic_results.csv", row.names = FALSE )
} else {
  setwd(results.dir)
  phat = read.csv( "Phat_above_0.15_stochastic_results.csv" )
}

update_result_csv( "Perc>0.15 est",
                   section = 7,
                   value = format_stat( 100*phat$Est, 0 ),
                   print = FALSE )
update_result_csv( "Perc>0.15 lo",
                   section = 7,
                   value = round( 100*phat$lo, 0 ),
                   print = FALSE )
update_result_csv( "Perc>0.15 hi",
                   section = 7,
                   value = format_stat( 100*phat$hi, 0 ),
                   print = TRUE )


###### P<0 #####
if ( rerun.Phat.from.scratch == TRUE ) {
  phat = MetaUtility::prop_stronger( 
    q = 0,
    estimate.method = "calibrated",
    ci.method = "calibrated",
    tail = "below",
    dat = d,
    R = 5000,
    yi.name = "main.fis",
    vi.name = "vi" )
  
  # save stochastic results
  setwd(results.dir)
  write.csv( phat, "Phat_below_0_stochastic_results.csv", row.names = FALSE )
} else {
  setwd(results.dir)
  phat = read.csv( "Phat_below_0_stochastic_results.csv" )
}

update_result_csv( "Perc<0 est",
                   section = 7,
                   value = format_stat( 100*phat$Est, 0 ),
                   print = FALSE )
update_result_csv( "Perc<0 lo",
                   section = 7,
                   value = round( 100*phat$lo, 0 ),
                   print = FALSE )
update_result_csv( "Perc<0 hi",
                   section = 7,
                   value = format_stat( 100*phat$hi, 0 ),
                   print = TRUE )


############################# 8. FOREST PLOT WITH CALIBRATED ESTIMATES ############################# 

# relative weight of each study in meta-analysis
d$rel.wt = NA
d$rel.wt = 100 * (1/d$vi) / sum(1/d$vi )

# add ensemble estimates
d$ens = calib_ests(yi = d$main.fis,
                   sei = d$main.SE)

# add lower and upper CI limits on Fisher's z scale
d$lo.fis = d$main.fis - qnorm(.975) * d$main.SE
d$hi.fis = d$main.fis + qnorm(.975) * d$main.SE

# lean plotting df
dp = d

# sort by ensemble estimate
dp = dp[ order(dp$ens, decreasing = FALSE), ]


# add pooled point estimates as first rows
# arbitrary relative weight
library(dplyr)
dp = add_row( dp,
              .before = 1,
              main.fis = m$b,
              ens = NA,
              vi = m$se^2,
              lo.fis = m$ci.lb,
              hi.fis = m$ci.ub,
              Site = "POOLED REPLICATIONS",
              rel.wt = 5 )  # arbitrary "weight" to get nice point size

# add original study point estimate
dp = add_row( dp,
              .before = 1,
              main.fis = orig.fis,
              ens = NA,
              vi = orig.SE^2,
              lo.fis = orig.fis - qnorm(.975) * orig.SE,
              hi.fis = orig.fis + qnorm(.975) * orig.SE,
              Site = "Original study",
              rel.wt = 5 )  # arbitrary "weight" to get nice point size

# make sure POOLED is the first level of the factor variable so it's displayed at bottom
correct.order = dp$Site
dp$Site = factor(dp$Site, levels = correct.order)
levels(dp$Site)  # should have POOLED at the end

dp$is.pooled = as.factor( c( "Original study",
                             "Pooled replications",
                             rep("Individual replication", nrow(dp) - 2) ) )
# fix legend order
dp$is.pooled = factor( dp$is.pooled,
                       levels = c("Individual replication",
                                  "Pooled replications",
                                  "Original study") )


#colors = c("red", "black", "blue")

shapes = c(10, 19, 15)
#breaks = seq(0.4, 6.0, .2)

breaks = c( seq(0.4, 1.3, .1),
            1.5,
            seq(1.5, 4, .5),
            seq(4, 6, 1) )

#breaks = exp( seq( log(0.4), log(6), .2 )

# for borderline status
colors = c("black", "darkorange", "deepskyblue4")

library(ggplot2)
forest = ggplot( data = dp, aes( x = z_to_r(main.fis), 
                               y = Site,
                               size = rel.wt,
                               shape = is.pooled,
                               color = is.pooled ) ) +
  geom_point() +
  #color = X.intensiveness ) ) +
  geom_errorbarh( aes(xmin = z_to_r(lo.fis),
                      xmax = z_to_r(hi.fis) ),
                  lwd = .5,
                  height = .001) +
  
  # calibrated estimates
  geom_point( data = dp, aes( x = z_to_r(ens),
                              y = Site ),
              size = 3,
              shape = 4,
              color = "red") +
  
  xlab( "Estimated correlation" ) +
  ylab("") +
  
  geom_vline(xintercept = 0, lty = 2) +
  
  guides(size = guide_legend("% weight in analysis") ) +
  
  scale_color_manual(values = colors,
                     name = "") +
  
  scale_shape_manual(values = shapes,
                     name = "") +
  #guide=FALSE) +
  
  scale_x_continuous( breaks = seq(-.5, .5, .1),
                      limits = c(-0.5, .5) ) +
  
  # scale_x_continuous( breaks = breaks, trans = "log10") +
  # # https://michaelbach.de/2012/07/22/R-ggplot2-and-axis-limits-unexpected-behaviour-solution.html
  # coord_cartesian( xlim = c(breaks[1], breaks[length(breaks)] ) ) +
  
  theme_bw()

#facet_wrap( ~Y.cat)  # to facet by outcome measure type

forest
# 11 x 8"















