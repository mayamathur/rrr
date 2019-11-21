
# run this interactively in ml load R

path = "/home/groups/manishad/RRR"
setwd(path)
source("functions_RRR.R")

######## STITCH LONG FILES ########

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

s = stitch_files(.results.singles.path = "/home/groups/manishad/RRR/sim_results/long",
                 .results.stitched.write.path = "/home/groups/manishad/RRR/sim_results/overall_stitched",
                 .name.prefix = "long_results",
                 .stitch.file.name="stitched.csv" )

s %>% group_by(POrig.Method, k, sd.w) %>%
  summarise( n(),
             mean(Reject),
             mean(EstMean),
             mean(EstVar))


# # temp only - stitch the 4 stitched ones
# s = stitch_files(.results.singles.path = "/home/groups/manishad/RRR/sim_results/overall_stitched",
#                  .results.stitched.write.path = "/home/groups/manishad/RRR/sim_results/overall_stitched",
#                  .name.prefix = "stitched",
#                  .stitch.file.name="stitched.csv" )


# stitch on Sherlock
# sbatch -p qsu,normal,owners /home/groups/manishad/RRR/stitch.sbatch
# sacct --jobs=49476370 --format=User,JobID,account,Timelimit,elapsed,ReqMem,MaxRss,ExitCode

# # move it to Desktop
# scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR/sim_results/overall_stitched/stitched.csv ~/Desktop

# ######## SNEAK PEEK AT RESULTS ########

# sneak peak at results
s$Reject = as.numeric(s$Reject)
s$Cover = as.numeric(s$Cover)
as.data.frame( s %>% group_by(scen.name)%>%
                 summarise_if( is.numeric, function(x) mean(x, na.rm = TRUE) ) )