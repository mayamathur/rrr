
# NOTE: BE CAREFUL ABOUT PUSHING ALL LOCAL FILES IF YOU HAVE SCEN_PARAMS.CSV LOCALLY! 
#  IF SO, IT MIGHT OVERWRITE THE ONE ON SHERLOCK AND SCREW THINGS UP. 

####################### CHECK IN ####################### 
# see the sbatches
cd /home/groups/manishad/RRR/sbatch_files

sbatch -p qsu,owners,normal /home/groups/manishad/RRR/sbatch_files/1.sbatch
sbatch -p qsu,owners,normal /home/groups/manishad/RRR/stitch.sbatch

# check on my running or pending jobs
squeue -u mmathur -o%T -ST | uniq -c


# see the datasets
cd /home/groups/manishad/RRR/sim_results/long
nano long*
ls -l . | egrep -c '^-'
grep -l "long" * | wc -l

# see the errors
nano /home/groups/manishad/RRR/sbatch_files/rm_1455.err

# see the scen parameters
nano /home/groups/manishad/RRR/scen_params.csv

# see the stitched results
nano /home/groups/manishad/RRR/sim_results/overall_stitched/sti*
  
  
  
####################### CODE -> SHERLOCK ####################### 

# push all the individual files
# include "-r" after scp to push contents of any sub-folders (not doing this to avoid pushing results)
scp /Users/mmathur/Dropbox/Personal\ computer/Independent\ studies/RRR\ estimators/Linked\ to\ OSF\ \(RRR\)/Other\ RRR\ code\ \(git\)/Simulation\ study/* mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR

# just push doParallel.R
scp /Users/mmathur/Dropbox/Personal\ computer/Independent\ studies/RRR\ estimators/Linked\ to\ OSF\ \(RRR\)/Other\ RRR\ code\ \(git\)/Simulation\ study/doParallel_RRR.R mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR
sbatch -p qsu,owners /home/groups/manishad/RRR/sbatch_files/1.sbatch

sbatch -p qsu,owners /home/groups/manishad/RRR/stitch.sbatch

# stitching sbatch and R
scp /Users/mmathur/Dropbox/Personal\ computer/Independent\ studies/RRR\ estimators/Linked\ to\ OSF\ \(RRR\)/Other\ RRR\ code\ \(git\)/Simulation\ study/stitch.sbatch mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR
scp /Users/mmathur/Dropbox/Personal\ computer/Independent\ studies/RRR\ estimators/Linked\ to\ OSF\ \(RRR\)/Other\ RRR\ code\ \(git\)/Simulation\ study/stitch_on_sherlock_RRR.R mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR

# see the sbatch files
cd /home/groups/manishad/RRR/sbatch_files
nano rm_1.err


####################### SHERLOCK -> DESKTOP (DEBUGGING) ####################### 

# move error file to Desktop
scp -r mmathur@sherlock:/home/groups/manishad/RRR/sbatch_files/rm_19.err ~/Desktop
scp -r mmathur@sherlock:/home/groups/manishad/RRR/sbatch_files/rm_19.out ~/Desktop

# move one sbatch file to Desktop
scp -r mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR/sbatch_files/2296.sbatch ~/Desktop


####################### RUN SBATCH ####################### 

# run one of them on Manisha's nodes
sbatch -p manishad /home/groups/manishad/RRR/sbatch_files/1.sbatch
# not on Manisha's nodes
sbatch -p qsu,normal,owners /home/groups/manishad/RRR/sbatch_files/1.sbatch




####################### RESULTS -> DESKTOP FOR ANALYSIS ####################### 

# move all datasets to local machine
scp -r mmathur@sherlock:/home/groups/manishad/RRR/sim_results.zip ~/Desktop

# move stitched dataset and parameters to local machine
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR/sim_results/overall_stitched/stitched.csv ~/Desktop
Vegemite2017
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR/scen_params.csv ~/Desktop
Vegemite2017

# move parameters to Desktop
scp mmathur@sherlock:/home/groups/manishad/RRR/scen_params.csv ~/Desktop

# zip the results files
cd /home/groups/manishad/RRR/sim_results
zip -r short.zip short
zip -r long.zip long

# move zipped results 
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR/sim_results/short.zip ~/Desktop
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR/sim_results/long.zip ~/Desktop

# look at one results file
cd /home/groups/manishad/RRR/sim_results/long
nano long_results_job_1_.csv

####################### CLEAN UP ####################### 

# clean up the directory
rm /home/groups/manishad/RRR/sim_results/*
  rm /home/groups/manishad/RRR/sim_results/short/*
  rm /home/groups/manishad/RRR/sim_results/long/*
  echo /home/groups/manishad/RRR/sim_results/long/* | xargs /bin/rm -f # works if above says "argument list too long"

  
  rm /home/groups/manishad/RRR/sim_results/overall_stitched/*
  rm /home/groups/manishad/RRR/sbatch_files/rm*
  rm /home/groups/manishad/RRR/sbatch_files/slurm*
  
  # delete all sbatches
  rm -r /home/groups/manishad/RRR/sbatch_files/*
  echo /home/groups/manishad/RRR/sbatch_files/* | xargs /bin/rm -f  # works if above says "argument list too long"
  