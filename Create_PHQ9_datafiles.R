rm(list=ls())
library("install.load")
install_load("data.table", "gdata")
install_load("plyr", "tidyverse", "doMC", "scales")
install_load("gridExtra", "pheatmap", "printr", "ggthemes", "tableone")
library("synapser")
synLogin()


SYNPROJECT= "syn10848316"

#PHQ9
phq9_brighten_v1  <- fread(synGet("syn10236540")$path, data.table = F) 
phq9_brighten_v1 <- phq9_brighten_v1 %>% 
  dplyr::mutate(response_local = lubridate::ymd_hms(response_local),
                phq9Date = as.Date(response_local),
                brightenid = as.character(brightenid)) %>%
  dplyr::mutate(phq9_1 = ph1, phq9_2 = ph2, phq9_3 = ph3, phq9_4 = ph4, phq9_5 = ph5, 
                phq9_6 = ph7, phq9_7 = ph7, phq9_8 = ph8, phq9_9 = ph9) %>%
  dplyr::select(-user_id, -response_local, -study_arm, -sum_phq10, -phq9ResponseTimeSecs,
                -ph1, -ph2, -ph3, -ph4, -ph5, -ph6, -ph7, -ph8, -ph9, -start) %>%
  dplyr::rename(participant_id = brightenid)


phq9_brighten_v2   <- fread(synGet("syn10164248")$path) %>% 
  dplyr::mutate(phq9Date = lubridate::ymd(phq9_date), week = day)  %>% 
  dplyr::filter( ! (is.na(username) | username == "")) %>%
  dplyr::rename(participant_id = username) %>%
  dplyr::select(-phq9_hod, -day, -phq9_timestamp, -phq9_10, -phq9_date)

phq9_brighten <- rbind(phq9_brighten_v1, phq9_brighten_v2[,colnames(phq9_brighten_v1)])


#write.csv(phq9_brighten, file="tmp_BRIGHTEN_longitudial_PHQ9.csv", quote = F, row.names = F)
synStore(synapser::synBuildTable("PHQ-9", SYNPROJECT, phq9_brighten))
