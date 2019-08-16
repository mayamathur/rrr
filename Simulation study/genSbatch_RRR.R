########################### SET SIMULATION PARAMETERS MATRIX ###########################

# FOR CLUSTER USE
path = "/home/groups/manishad/RRR"
setwd(path)

# full set of scenarios
k = c(10)
mu = 0.5  # mean of true effects (log-RR)
V = c( 0.5^2 )  # variance of true effects
#V = c( 0.1^2, 0.2^2, 0.5^2 )  # variance of true effects
muN = NA # just a placeholder; to be filled in later
#minN = c( 100, 800 )
minN = c( 800 )
sd.w = 1
tail = "above"

# only running P = 0.20 to supplement previous sim results
# set q to be the quantiles such that TheoryP is 0.2 for every V
#TheoryP = c(0.05, 0.1, 0.2, 0.5)
TheoryP = 0.1

qmat = matrix( NA, nrow = length(V), ncol = length(TheoryP) )

for (i in 1:length(TheoryP) ) {
  new.qs = qnorm( p = 1 - TheoryP[i],
                  mean = mu,
                  sd = sqrt(V) )
  qmat[,i] = new.qs
}

q = unique( as.vector( qmat ))

# matrix of scenario parameters
scen.params = expand.grid(k, mu, V, q, muN, minN, tail, sd.w)
names(scen.params) = c("k", "mu", "V", "q", "muN", "minN", "tail", "sd.w" )


# only keep combos of V and q that lead to the TheoryPs we want
TheoryP2 = 1 - pnorm( q = scen.params$q,
                      mean = scen.params$mu,
                      sd = sqrt(scen.params$V) )
scen.params = scen.params[ round(TheoryP2,3) %in% TheoryP, ]
table(scen.params$q, scen.params$V ); qmat  # each 


#start.at = which( my.letters == "aaaa" )
start.at = 1
scen.params$scen.name = start.at : ( start.at + nrow(scen.params) - 1 )
( n.scen = length(scen.params[,1]) )

# avoid doing all factorial combinations of muN and minN this way
scen.params$muN = scen.params$minN + 50

# write the csv file of params (to Sherlock)
write.csv( scen.params, "scen_params.csv" )


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

generateSbatch(sbatch_params, runfile_path)

n.files

# max hourly submissions seems to be 300, which is 12 seconds/job
path = "/home/groups/manishad/RRR"
setwd( paste(path, "/sbatch_files", sep="") )
for (i in 1:1) {
  #system( paste("sbatch -p owners /home/groups/manishad/RRR/sbatch_files/", i, ".sbatch", sep="") )
  system( paste("sbatch -p manishad /home/groups/manishad/RRR/sbatch_files/", i, ".sbatch", sep="") )
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
                              .max.sbatch.num = 1080 )



setwd( paste(path, "/sbatch_files", sep="") )
for (i in missed.nums) {
  system( paste("sbatch -p owners /home/groups/manishad/RRR/sbatch_files/", i, ".sbatch", sep="") )
}