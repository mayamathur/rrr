
# run this interactively in ml load R

path = "/home/groups/manishad/RRR"
setwd(path)
source("functions_RRR.R")

######## FOR STITCHING LONG FILES ########

s = stitch_files(.results.singles.path = "/home/groups/manishad/RRR/sim_results/long",
                 .results.stitched.write.path = "/home/groups/manishad/RRR/sim_results/overall_stitched",
                 .name.prefix = "long",
                 .stitch.file.name="stitched.csv")

# # how close are we to being done?
# n.reps.per.scen = 500
# nrow(s) / (n.reps.per.scen * nrow(scen.params))
# nrow(s)/26000
# table(s$scen)



# move it to Desktop
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR/sim_results/overall_stitched/stitched.csv ~/Desktop
Vegemite2017
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR/scen_params.csv ~/Desktop
Vegemite2017

