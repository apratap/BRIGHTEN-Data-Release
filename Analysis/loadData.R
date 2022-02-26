rm(list=ls())
library("install.load")
install_load("data.table", "gdata", "tidyverse")
library("synapser")
synLogin()


#Once
demog <- synTableQuery("select * from syn27082597")$asDataFrame() 
demog_flt <- demog %>% filter(!is.na(participant_id)) %>%
  dplyr::mutate(age_group = cut(age, breaks = c(18,30,40,50,60,70,100)))
demog_flt[is.na(demog_flt) | demog_flt == ''] = 'N/A'

auditC <- synTableQuery("select * from syn17021280")$asDataFrame() %>%
  mutate(dt_response = lubridate::ymd_hms(dt_response))

gad7 <- synTableQuery("select * from syn17022655")$asDataFrame() 

baselinePHQ9 <-  synTableQuery("select * from syn27082811")$asDataFrame() %>%
  mutate(sum_phq9 = phq9_1 + phq9_2 + phq9_3 + phq9_4 + phq9_5 + phq9_6 + phq9_7 + phq9_8 + phq9_9) %>%
  mutate(depression_categories =  cut(as.numeric(sum_phq9), breaks=c(0,4,9,14,19,27),
                                  labels = c('minimal', 'mild', 'moderate', 'moderately-severe', 'severe')))


sleep <- synTableQuery("select * from syn17022659")$asDataFrame()

mh_services <- synTableQuery("select * from syn17022660")$asDataFrame()

prior_MH_screening <- synTableQuery("select * from syn27051276")$asDataFrame() %>%  
  dplyr::mutate(dt_response = lubridate::ymd_hms(dt_response))

other_apps_used <- synTableQuery("select * from syn17025058")$asDataFrame() %>%
  dplyr::mutate(dt_response = lubridate::ymd_hms(dt_response)) %>%
  ### Summarizing the data to indicate whether the participant indicated using one of the apps across the study period
  gather(app_type, used, 6:14) %>%
  dplyr::group_by(participant_id,app_type) %>%
  dplyr::summarise(used = sum(used) > 0) %>%
  spread(app_type, used)

appSatisfaction <- synTableQuery("select * from syn17025202")$asDataFrame()

#daily
phq2 <- synTableQuery("select * from syn17020855")$asDataFrame() 

#weekly
phq9 <- synTableQuery("select * from 	syn27202355")$asDataFrame()
sds <- synTableQuery("select * from syn17022658")$asDataFrame()
mood <- synTableQuery("select * from syn17023313")$asDataFrame()


## Passive data
v1_passivedata <- synTableQuery("select * from 	syn17025500")$asDataFrame()
v2_mobilitydata <- synTableQuery("select * from 	syn17114662")$asDataFrame()
v2_phoneLogs <-synTableQuery("select * from 	syn17060502")$asDataFrame()

