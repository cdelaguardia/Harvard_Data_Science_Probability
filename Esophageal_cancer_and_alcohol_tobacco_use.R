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

#What is the probability that a subject in the highest alcohol 
#consumption group is a cancer case?

highest_alc <- esoph %>% group_by(alcgp) %>% 
    summarise(sum_cases = sum(ncases), sum_controls = sum(ncontrols) ) %>% 
    filter(alcgp == "120+")

Pr_highest_alc_case = highest_alc$sum_cases/(highest_alc$sum_cases + highest_alc$sum_controls)

#What is the probability that a subject in the lowest alcohol consumption 
#group is a cancer case

lowest_alc <- esoph %>% group_by(alcgp) %>% 
  summarise(sum_cases = sum(ncases), sum_controls = sum(ncontrols) ) %>% 
  filter(alcgp == "0-39g/day")

Pr_lowest_alc_case = lowest_alc$sum_cases/(lowest_alc$sum_cases + lowest_alc$sum_controls)


#Given that a person is a control, what is the probability that they smoke 
#10g or more a day?
controls_smoke_10g <- esoph %>% group_by(tobgp) %>% 
  summarise(sum_controls = sum(ncontrols)) %>% 
  filter(tobgp != "0-9g/day")

Pr_case_10g <- sum(controls_smoke_10g$sum_controls)/all_controls

#For cases, what is the probability of being in the highest alcohol group?

cases_highest_alc <- esoph %>% group_by(alcgp) %>% 
  summarise(sum_cases = sum(ncases)) %>% 
  filter(alcgp == "120+")

Pr_case_highest_alc <- cases_highest_alc$sum_cases/all_cases

#For cases, what is the probability of being in the highest tobacco group?
cases_highest_tobac <- esoph %>% group_by(tobgp) %>% 
  summarise(sum_cases = sum(ncases)) %>% 
  filter(tobgp == "30+")

Pr_case_highest_tobac <- cases_highest_tobac$sum_cases/all_cases

#For cases, what is the probability of being in the highest alcohol group and 
#the highest tobacco group?

cases_highest_alc_tobac <- esoph %>% filter(alcgp == "120+" & tobgp == "30+") %>% summarise(sum_cases = sum(ncases))

Pr_cases_highest_alc_tobac <- cases_highest_alc_tobac/all_cases                             