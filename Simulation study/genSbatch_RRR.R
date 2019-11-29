
########################### SET SIMULATION PARAMETERS MATRIX ###########################

# FOR CLUSTER USE
path = "/home/groups/manishad/RRR"
setwd(path)
source("functions_RRR.R")

library(vctrs, lib.loc = "/home/groups/manishad/Rpackages/")
library(utf8, lib.loc = "/home/groups/manishad/Rpackages/")
library(cli, lib.loc = "/home/groups/manishad/Rpackages/")
library(MetaUtility, lib.loc = "/home/groups/manishad/Rpackages/")
library(crayon, lib.loc = "/home/groups/manishad/Rpackages/")
library(dplyr, lib.loc = "/home/groups/manishad/Rpackages/")
library(foreach, lib.loc = "/home/groups/manishad/Rpackages/")
library(doParallel, lib.loc = "/home/groups/manishad/Rpackages/")
library(boot, lib.loc = "/home/groups/manishad/Rpackages/")
library(metafor, lib.loc = "/home/groups/manishad/Rpackages/")
library(data.table, lib.loc = "/home/groups/manishad/Rpackages/")
library(purrr, lib.loc = "/home/groups/manishad/Rpackages/")
library(metRology, lib.loc = "/home/groups/manishad/Rpackages/")

# full set of scenarios
( scen.params = make_scen_params( k = c(25, 20, 15, 10, 5),
                                  mu = 0.5,  # mean of true effects
                                  V = c( .002, .01, .25 ),  # variance of true effects
                                  muN = NA, # just a placeholder; to be filled in later by setting equal to minN
                                  minN = c(300, 50),
                                  sd.w = c(1),
                                  tail = "above",
                                  true.effect.dist = c("normal", "expo", "t.scaled", "unif2"), # # "expo", "normal", "unif2", "t.scaled"
                                  TheoryP = c(0.05, 0.10, 0.5),

                                  # for generating the single original study
                                  delta = c(0, .2, .5, 1),  # difference between mu for replications and for original (0 for H0); can be interpreted as SMD
                                  N.orig = c(200, 50, 30),  # sample size of original study

                                  start.at = 1 ) )



( n.scen = nrow(scen.params) )
# look at it
head( as.data.frame(scen.params) )

# write the csv file of params (to Sherlock)
write.csv( scen.params, "scen_params.csv", row.names = FALSE )

# to add to existing scenario parameters rather than overwriting


########################### GENERATE SBATCHES ###########################

# load functions for generating sbatch files
source("functions_RRR.R")

# number of sbatches to generate (i.e., iterations within each scenario)
n.reps.per.scen = 512*2 
n.reps.in.doParallel = 512*2 # make this divisible by 16 so that there is an integer number of files! 

# this will be an everlasting love
( n.files = ( n.reps.per.scen / n.reps.in.doParallel ) * n.scen )

# where to start job names (usually 1)
start.at = 1
end.at = start.at + (n.files - 1)
path = "/home/groups/manishad/RRR"

scen.name = rep( scen.params$scen.name, each = ( n.files / n.scen ) )
jobname = paste("job", start.at:end.at, sep="_")
outfile = paste("rm_", start.at:end.at, ".out", sep="")
errorfile = paste("rm_", start.at:end.at, ".err", sep="")
write_path = paste(path, "/sbatch_files/", start.at:end.at, ".sbatch", sep="")
runfile_path = paste(path, "/testRunFile.R", sep="")

sbatch_params <- data.frame(jobname,
                            outfile,
                            errorfile,
                            jobtime = "2:00:00",
                            quality = "normal",
                            node_number = 1,
                            mem_per_node = 64000,
                            mailtype =  "NONE",
                            user_email = "mmathur@stanford.edu",
                            tasks_per_node = 16,
                            cpus_per_task = 1,
                            path_to_r_script = paste(path, "/doParallel_RRR.R", sep=""),
                            args_to_r_script = paste("--args", jobname, scen.name, sep=" "),
                            write_path,
                            stringsAsFactors = F,
                            server_sbatch_path = NA)

generateSbatch(sbatch_params, runfile_path)

n.files

# 4320
# max hourly submissions seems to be 300, which is 12 seconds/job
path = "/home/groups/manishad/RRR"
setwd( paste(path, "/sbatch_files", sep="") )
for (i in 1:4320) {
  #system( paste("sbatch -p owners /home/groups/manishad/RRR/sbatch_files/", i, ".sbatch", sep="") )
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/RRR/sbatch_files/", i, ".sbatch", sep="") )
  #Sys.sleep(2)  # delay in seconds
}




######## If Running Only Some Jobs To Fill Gaps ########

# bm: run more to fill in gaps
# run in Sherlock ml load R
path = "/home/groups/manishad/RRR"
setwd(path)
source("functions_RRR.R")

missed.nums = read.csv("need_more.csv")$need.more

missed.nums = sbatch_not_run( "/home/groups/manishad/RRR/sim_results/long",
                              "/home/groups/manishad/RRR/sim_results",
                              .name.prefix = "long_results",
                              .max.sbatch.num = 4320 )



setwd( paste(path, "/sbatch_files", sep="") )
for (i in missed.nums) {
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/RRR/sbatch_files/", i, ".sbatch", sep="") )
}







