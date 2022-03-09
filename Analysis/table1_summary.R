rm(list=ls())
library("install.load")
install_load("tidyverse")
source("Analysis/loadData.R")

#Total screened
table(demog$study)

#Total Excluded
table(demog$status, demog$study)

#1. Baseline demog
nrow(demog_flt)
n_distinct(demog_flt$participant_id)
table(demog_flt$study)

#2. baseline PHQ-9 survey
nrow(baselinePHQ9)
n_distinct(baselinePHQ9$participant_id)


#3. GAD-7 - ERROR
nrow(gad7)
n_distinct(gad7$participant_id)
table(gad7$week)


#4. Other mobile apps used
nrow(other_apps_used)
n_distinct(other_apps_used$participant_id)

## AUDIT-C
nrow(auditC)
n_distinct(auditC$participant_id)


### Sleep
nrow(sleep)
n_distinct(sleep$participant_id)
table(sleep$week)


## MH Services
nrow(mh_services)
n_distinct(mh_services$participant_id)


## PHQ-9
nrow(phq9)
n_distinct(phq9$participant_id)
table(phq9$week)

## SDS
nrow(sds)
n_distinct(sds$participant_id)
table(sds$week)


### PHQ-2
nrow(phq2)
n_distinct(phq2$participant_id)

## Mood 
nrow(mood)
n_distinct(mood$participant_id)

## Prior Mental Health Screening 
nrow(prior_MH_screening)
n_distinct(prior_MH_screening$participant_id)

## Study App satisfaction
nrow(appSatisfaction)
n_distinct(appSatisfaction$participant_id)


##Passive data


