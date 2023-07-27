#Case-control studies help determine whether certain exposures are associated 
#with outcomes such as developing cancer. The built-in dataset esoph contains 
#data from a case-control study in France comparing people with esophageal 
#cancer (cases, counted in ncases) to people without esophageal cancer 
#(controls, counted in ncontrols) that are carefully matched on a variety of 
#demographic and medical characteristics. The study compares alcohol intake in 
#grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases 
#and controls grouped by age range (agegp).

#How many groups are in the study?

groups_count <- length(esoph$agegp)

#How many cases are there?

all_cases <- sum(esoph$ncases)

#How many controls are there?

all_controls <- sum(esoph$ncontrols)