
# run this interactively in ml load R

path = "/home/groups/manishad/RRR"
setwd(path)
source("functions_RRR.R")

######## FOR STITCHING LONG FILES ########

s = stitch_files(.results.singles.path = "/home/groups/manishad/RRR/sim_results/long",
                 .results.stitched.write.path = "/home/groups/manishad/RRR/sim_results/overall_stitched",
                 .name.prefix = "long_results",
                 .stitch.file.name="stitched.csv")

# sneak peek
library(crayon, lib.loc = "/home/groups/manishad/Rpackages/")
library(fansi, lib.loc = "/home/groups/manishad/Rpackages/")
library(dplyr, lib.loc = "/home/groups/manishad/Rpackages/")
library(utf8, lib.loc = "/home/groups/manishad/Rpackages/")
library(cli, lib.loc = "/home/groups/manishad/Rpackages/")

# proportion of successful reps (no note)
s %>% group_by(scen.name) %>%
  summarise( mean( is.na(Note) ),
             n() )

agg = s %>% filter( !is.na(Method)) %>%
  group_by(scen.name, Method, true.effect.dist) %>%
  summarise( cover = mean(Cover, na.rm=TRUE),
             phat = mean(phat, na.rm = TRUE), 
             width = mean(Width, na.RM = TRUE),
             n = n())

# just sign test
agg = s %>% filter( Method == "NP sign test") %>%
  group_by(scen.name, Method, true.effect.dist) %>%
  summarise( cover = mean(Cover, na.rm=TRUE),
             phat = mean(phat, na.rm = TRUE), 
             width = mean(Width, na.RM = TRUE),
             n = n())

as.data.frame(agg)

# look at min and mean behavior by inference method
agg2 = s %>% filter( !is.na(Method)) %>%
  group_by(Method) %>%
  summarise( cover.mean = mean(Cover),
             #cover.min = min(Cover),
             width.mean = mean(Width),
             n = n())
as.data.frame(agg2)

# look at point estimates
agg3 = s %>% filter( !is.na(Method)) %>%
  group_by(Method) %>%
  summarise( phatBias = mean(phatBias))
as.data.frame(agg3)

# # how close are we to being done?
# n.reps.per.scen = 500
# nrow(s) / (n.reps.per.scen * nrow(scen.params))
# nrow(s)/26000
# table(s$scen)



# move it to Desktop
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR/sim_results/overall_stitched/stitched.csv ~/Desktop
Vegemite2019
scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/RRR/scen_params.csv ~/Desktop
Vegemite2019

