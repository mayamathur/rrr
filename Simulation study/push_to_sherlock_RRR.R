
# expect 10*3 = 30 rows in long dataset

####################### CHECK IN ####################### 
# see the sbatches
cd /home/groups/manishad/MAM/sbatch_files

sbatch -p manishad /home/groups/manishad/MAM/sbatch_files/1.sbatch

# check on my running or pending jobs
squeue -u mmathur -t RUNNING
squeue -u mmathur -t PENDING


# see the datasets
cd /home/groups/manishad/MAM/sim_results/long
ls -l . | egrep -c '^-'

# see the errors
nano /home/groups/manishad/MAM/sbatch_files/slurm*
nano /home/groups/manishad/MAM/sbatch_files/rm_1.err

# see the scen parameters
nano /home/groups/manishad/MAM/scen_params.csv

# see the stitched results
nano /home/groups/manishad/MAM/sim_results/overall_stitched/sti*
  
  
  
####################### CODE -> SHERLOCK ####################### 

# push all the individual files
scp /Users/mmathur/Dropbox/Personal\ computer/Independent\ studies/Meta-analysis\ metrics\ \(MAM\)/Linked\ to\ OSF\ \(MAM\)/Code/Simulation\ study/For\ Sherlock/* mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MAM


# see the sbatch files
cd /home/groups/manishad/MAM/sbatch_files
nano rm_1.err


####################### SHERLOCK -> DESKTOP (DEBUGGING) ####################### 

# move error file to Desktop
scp -r mmathur@sherlock:/home/groups/manishad/MAM/sbatch_files/rm_19.err ~/Desktop
scp -r mmathur@sherlock:/home/groups/manishad/MAM/sbatch_files/rm_19.out ~/Desktop

# move one sbatch file to Desktop
scp -r mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MAM/sbatch_files/2296.sbatch ~/Desktop


####################### RUN SBATCH ####################### 

# run one of them on Manisha's nodes
sbatch -p manishad /home/groups/manishad/MAM/sbatch_files/1.sbatch
# not on Manisha's nodes
sbatch -p normal,owners /home/groups/manishad/MAM/sbatch_files/1.sbatch




####################### RESULTS -> DESKTOP FOR ANALYSIS ####################### 

# move all datasets to local machine
scp -r mmathur@sherlock:/home/groups/manishad/MAM/sim_results.zip ~/Desktop

# move stitched dataset and parameters to local machine
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MAM/sim_results/overall_stitched/stitched.csv ~/Desktop
Vegemite2017
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MAM/scen_params.csv ~/Desktop
Vegemite2017

# move parameters to Desktop
scp mmathur@sherlock:/home/groups/manishad/MAM/scen_params.csv ~/Desktop

# zip the results files

cd /home/groups/manishad/MAM/sim_results
zip -r short.zip short
zip -r long.zip long

# move zipped results 
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MAM/sim_results/short.zip ~/Desktop
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/MAM/sim_results/long.zip ~/Desktop


####################### CLEAN UP ####################### 

# clean up the directory
rm /home/groups/manishad/MAM/sim_results/*
  rm /home/groups/manishad/MAM/sim_results/short/*
  rm /home/groups/manishad/MAM/sim_results/long/*
  
  rm /home/groups/manishad/MAM/sim_results/overall_stitched/*
  rm /home/groups/manishad/MAM/sbatch_files/rm*
  rm /home/groups/manishad/MAM/sbatch_files/slurm*
  
  rm -r /home/groups/manishad/MAM/sbatch_files/*
  