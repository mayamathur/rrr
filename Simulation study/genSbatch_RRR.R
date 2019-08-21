########################### SET SIMULATION PARAMETERS MATRIX ###########################

# FOR CLUSTER USE
path = "/home/groups/manishad/RRR"
setwd(path)
source("functions_RRR.R")

library(crayon, lib.loc = "/home/groups/manishad/Rpackages/")
library(dplyr, lib.loc = "/home/groups/manishad/Rpackages/")
library(foreach, lib.loc = "/home/groups/manishad/Rpackages/")
library(doParallel, lib.loc = "/home/groups/manishad/Rpackages/")
library(boot, lib.loc = "/home/groups/manishad/Rpackages/")
library(metafor, lib.loc = "/home/groups/manishad/Rpackages/")
library(data.table, lib.loc = "/home/groups/manishad/Rpackages/")
library(purrr, lib.loc = "/home/groups/manishad/Rpackages/")

# full set of scenarios
( scen.params = make_scen_params( k = c(5, 10, 15, 20, 50),
                                  mu = 0.5,  # mean of true effects (log-RR)
                                  V = c( 0.5^2, 0.2^2, 0.1^2 ),  # variance of true effects
                                  muN = NA, # just a placeholder; to be filled in later
                                  minN = c(100, 800),
                                  sd.w = 1,
                                  tail = "above",
                                  true.effect.dist = c("expo", "normal"), # "expo" or "normal"
                                  TheoryP = c(0.05, 0.1, 0.2, 0.5) ) )
( n.scen = nrow(scen.params) )
# look at it
head( as.data.frame(scen.params) )

# temp only
# check difference in TheoryP between normal and exponential
1 - pnorm( q = calculate_q(true.effect.dist = "expo",
                           TheoryP = .05, 
                           mu = 0.5, 
                           V = 0.5^2),
           mean = 0.5,
           sd = sqrt(0.5^2) )


# write the csv file of params (to Sherlock)
write.csv( scen.params, "scen_params.csv", row.names = FALSE )


########################### GENERATE SBATCHES ###########################

# load functions for generating sbatch files
source("functions_RRR.R")

# number of sbatches to generate (i.e., iterations within each scenario)
n.reps.per.scen = 500
n.reps.in.doParallel = 5
( n.files = ( n.reps.per.scen / n.reps.in.doParallel ) * n.scen )


path = "/home/groups/manishad/RRR"

scen.name = rep( scen.params$scen.name, each = ( n.files / n.scen ) )
jobname = paste("job", 1:n.files, sep="_")
outfile = paste("rm_", 1:n.files, ".out", sep="")
errorfile = paste("rm_", 1:n.files, ".err", sep="")
write_path = paste(path, "/sbatch_files/", 1:n.files, ".sbatch", sep="")
runfile_path = paste(path, "/testRunFile.R", sep="")

sbatch_params <- data.frame(jobname,
                            outfile,
                            errorfile,
                            jobtime = "8:00:00",
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

#generateSbatch(sbatch_params, runfile_path)

n.files

#24,000 jobs
# max hourly submissions seems to be 300, which is 12 seconds/job
path = "/home/groups/manishad/RRR"
setwd( paste(path, "/sbatch_files", sep="") )
for (i in 13606:24000) {
  #system( paste("sbatch -p owners /home/groups/manishad/RRR/sbatch_files/", i, ".sbatch", sep="") )
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/RRR/sbatch_files/", i, ".sbatch", sep="") )
  Sys.sleep(2)  # delay in seconds
}




######## If Running Only Some Jobs To Fill Gaps ########

# run in Sherlock ml load R
path = "/home/groups/manishad/RRR"
setwd(path)
source("functions_RRR.R")

missed.nums = sbatch_not_run( "/home/groups/manishad/RRR/sim_results/long",
                              "/home/groups/manishad/RRR/sim_results",
                              .name.prefix = "long_results",
                              .max.sbatch.num = 24000 )



setwd( paste(path, "/sbatch_files", sep="") )
for (i in missed.nums) {
  system( paste("sbatch -p owners /home/groups/manishad/RRR/sbatch_files/", i, ".sbatch", sep="") )
}