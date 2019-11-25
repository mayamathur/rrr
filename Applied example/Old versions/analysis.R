


############################# LOAD FUNCTIONS AND DATA ############################# 

# functions to compute metrics
library(here)
source( here( "Code", "RRR_functions.R" ) )

results.dir = here("Results from R")

results.overleaf = "/Users/mmathur/Dropbox/Apps/Overleaf/JRSSA_2\ RRR\ Response\ Letter/R_objects/stats_for_paper.csv"

# read in ML3 data
# to reproduce, use the Credentials file in OSF repo
d = read.csv( here( "Applied example data", "Credentials.csv" ), header = TRUE )



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


setwd(results.overleaf)
update_result_csv( "Orig Fisher",
                   section = 1,
                   value = round( orig.fis, 2 ),
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

library(metafor)
m = rma.uni( yi = d$main.fis,
             vi = main.SE^2,
             data = d,
             measure = "ZCOR" ) 
# I^2 = 34% vs. reported: 40%

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
                   value = format_stat(m$tau2),
                   print = FALSE )
update_result_csv( "Reps Shapiro pval",
                   section = 3,
                   value = format_stat(norm.test$p.value, 2),
                   print = TRUE )



############################# OLD METRIC #1: PROPORTION P-VALUES IN AGREEMENT ############################# 

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


############################# OLD METRIC #2: PROPORTION REPLICATIONS IN PREDICTION INTERVAL ############################# 

rep.inside = pred_int( orig.y = orig.fis,
                       orig.vy = orig.SE^2,
                       rep.y = d$main.fis,
                       rep.vy = d$main.SE^2 )$rep.inside

om2 = 100 * round( prop.table(table(rep.inside))[2], 2 )
# 76% inside vs. 95% expected


############################# NEW METRIC #1: CONSISTENCY OF ORIGINAL WITH HETEROGENEOUS RRR DISTRIBUTION ############################# 

# report the statistics that go into the metric
mu.hat = round( m$b, 2 )
se.mu.hat = round( sqrt( m$vb ), 2 )
t2 = round( m$tau2, 4 )


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



############################# NEW METRIC #2: PROPORTION EFFECT SIZES ABOVE THRESHOLD ############################# 

# r > 0
s0 = prop_stronger2( dat = d,
                     q = 0,
                     M = m$b,
                     t2 = m$tau2,
                     se.M = sqrt(m$vb),
                     se.t2 = m$se.tau2,
                     tail = "above",
                     R = 2000,
                     boot = "ifneeded",
                     yi.name = "main.fis",
                     vi.name = "vi",
                     measure = "ZCOR",
                     method = "REML" )

# r > 0.10         
s1 = prop_stronger2( dat = d,
                     q = r_to_z(0.1),
                     M = m$b,
                     t2 = m$tau2,
                     se.M = sqrt(m$vb),
                     se.t2 = m$se.tau2,
                     tail = "above",
                     R = 2000,
                     boot = "ifneeded",
                     yi.name = "main.fis",
                     vi.name = "vi",
                     measure = "ZCOR",
                     method = "REML" )
# r > 0.2
s2 = prop_stronger2( dat = d,
                     q = r_to_z(0.2),
                     M = m$b,
                     t2 = m$tau2,
                     se.M = sqrt(m$vb),
                     se.t2 = m$se.tau2,
                     tail = "above",
                     R = 2000,
                     boot = "ifneeded",
                     yi.name = "main.fis",
                     vi.name = "vi",
                     measure = "ZCOR",
                     method = "REML" )
# r < -0.10
s3 = prop_stronger2( dat = d,
                     q = r_to_z(-0.1),
                     M = m$b,
                     t2 = m$tau2,
                     se.M = sqrt(m$vb),
                     se.t2 = m$se.tau2,
                     tail = "below",
                     R = 2000,
                     boot = "ifneeded",
                     yi.name = "main.fis",
                     vi.name = "vi",
                     measure = "ZCOR",
                     method = "REML" )


############################# SAVE ALL ANALYSIS OBJECTS ############################# 

save.image( file = "all_analysis_objects.RData" )



