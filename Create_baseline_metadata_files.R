rm(list=ls())
library("install.load")
install_load("data.table", "gdata")
install_load("plyr", "tidyverse", "doMC", "scales")
install_load("gridExtra", "pheatmap", "printr", "ggthemes", "tableone")
library("synapser")
synLogin()

SYNPROJECT= "syn10848316"


sideFill <- function(col1, col2, fillCol1=T, fillCol2=F){
  missing_data_col1 <- col1 == '' | is.na(col1)
  missing_data_col2 <- col2 == '' | is.na(col2)
  ### fill missing values from 
  if(fillCol1){
    col1[missing_data_col1] = col2[missing_data_col1]
    return(col1)
  } else if(fillCol2){
    col2[missing_data_col2] = col1[missing_data_col2]
    return(col2)
  } else {
    stop('No idea what to do -- sorry')
  }
}

brightv1_userid_to_emails <- read.xls("~/Dropbox/brighten/BRIGHTEN V1/BRIGHTEN V1 Data/Real Final BRIGHTEN V1 DATA/20180815_linking_ID_to_Emails_APcleaned_HAS_PHI.xls")
brightv1_fullscreening_data <- fread("~/Dropbox/brighten/BRIGHTEN V1/BRIGHTEN V1 Data/Real Final BRIGHTEN V1 DATA/20180815_enrollment_data_APcleaned_HAS_PHI.csv")
brightv1_fullscreening_data <- merge(brightv1_fullscreening_data, brightv1_userid_to_emails, all=T) %>% 
  dplyr::mutate(brightenid = id) %>%
  dplyr::select(-email, -id) %>%
  dplyr::mutate(age= as.numeric(age),
                age=replace(age, age > 120, NA),
                race = replace(race, hispanic == 'Yes',  'Hispanic_Latino'),
                race = recode(race, 'African-American' = 'African-American/Black', 
                              'Hispanic' = 'Hispanic_Latino', 'Non-Hispanic White' = 'Non-hispanic White')) %>%
  dplyr::select(-hispanic)

##subset only for folks that were enrolled 
mdata_brighten_v1  <- fread(synGet("syn10236547")$path, data.table = F) %>%
  dplyr::mutate(study = 'BRIGHTEN v1',
                working = employed,
                gender = Gender, age=as.numeric(Age), income_satisfaction = income1, 
                working = employed, userid = brightenid,
                age_group = cut(age, c(18,30,40,50,60,70,100)),
                phq9_1 = ph1, phq9_2 = ph2, phq9_3 = ph3,
                phq9_4 = ph4,phq9_5 = ph5,phq9_6 = ph6,
                phq9_7 = ph7,phq9_8 = ph8,phq9_9 = ph9,
                sum_phq9 = phq9_1 + phq9_2 + phq9_3 + phq9_4 + phq9_5 + phq9_6 + 
                  phq9_7 + phq9_8 + phq9_9,
                income_lastyear = income2, race = racerecoded, marital_status = marital) %>%
  dplyr::mutate(race = racerecoded) %>%
  dplyr::select(-Gender, -Age, -State, -ph1, -ph2, -ph3, -ph4, -ph5, -ph6, -ph7, -ph8, 
                -ph9, -employed,
                -survivaldays, -racerecoded, -hispanic, -income1, -income2, -ordinalincome) %>%
  dplyr::filter(!is.na(brightenid))


##### Bunch of custom data cleaning to make sure we have the largest and cleanest dataset being released
x <- merge(brightv1_fullscreening_data, mdata_brighten_v1, by = c('brightenid'), all=T)
######
## CRAZY amount of edits
######
brighten_v1 <- x %>%
  #Fix Age 
  dplyr::mutate(age = sideFill(age.x, age.y, fillCol1 = T))  %>% select(-age.x, -age.y) %>%
  # Fix gender
  dplyr::mutate(gender = sideFill(gender.x, gender.y, fillCol1 = T)) %>% select(-gender.x, -gender.y) %>%
  # Fix income_lastyear 
  dplyr::mutate(income_lastyear = sideFill(income_lastyear.x, income_lastyear.y, fillCol1 = T)) %>% 
    select(-income_lastyear.x, -income_lastyear.y) %>% 
  #Income levels
  dplyr::mutate(income_satisfaction = sideFill(income_comfort_level,income_satisfaction , fillCol1 = T)) %>% 
  select(-income_comfort_level) %>% 
  # Working & Race & Marital status
  dplyr::mutate(working = sideFill(working.x, working.y, fillCol1 = T),
                race = sideFill(race.x, race.y, fillCol1 = T),
                education = sideFill(education, highest_education, fillCol1 = T),
                device = phonetype,
                zipcode = sideFill(zipcode, Postal, fillCol1 = T),
                marital_status = sideFill(maritalstatus, marital, fillCol1 = T),
                race = recode(race, 'African-American' = 'African-American/Black', 
                              'Hispanic' = 'Hispanic_Latino', 'Caucasian' = 'Non-Hispanic White')) %>% 
  select(-working.x, -working.y, -race.x, -race.y, -marital, 
         -maritalstatus, -Postal) %>%
  # PHQ -9 
  dplyr::mutate(phq9_1 = sideFill(phq9_1.x, phq9_1.y, fillCol1 = T),
                phq9_2 = sideFill(phq9_2.x, phq9_2.y, fillCol1 = T),
                phq9_3 = sideFill(phq9_3.x, phq9_3.y, fillCol1 = T),
                phq9_4 = sideFill(phq9_4.x, phq9_4.y, fillCol1 = T),
                phq9_5 = sideFill(phq9_5.x, phq9_5.y, fillCol1 = T),
                phq9_6 = sideFill(phq9_6.x, phq9_6.y, fillCol1 = T),
                phq9_7 = sideFill(phq9_7.x, phq9_7.y, fillCol1 = T),
                phq9_8 = sideFill(phq9_8.x, phq9_8.y, fillCol1 = T),
                phq9_9 = sideFill(phq9_9.x, phq9_9.y, fillCol1 = T),
                sum_phq9 = sideFill(sum_phq9.x, sum_phq9.y, fillCol1 = T)) %>%
  select(-phq9_1.x, -phq9_2.x,-phq9_3.x, -phq9_4.x,-phq9_5.x, -phq9_6.x,-phq9_7.x,-phq9_8.x,-phq9_9.x,
         -phq9_1.y, -phq9_2.y,-phq9_3.y, -phq9_4.y,-phq9_5.y, -phq9_6.y,-phq9_7.y,-phq9_8.y,-phq9_9.y,
         -sum_phq9.x, -sum_phq9.y) %>%
  #heard_about_us vs referral
  dplyr::mutate(heard_about_us = sideFill(heard_about_us, referral, fillCol1 = T)) %>% 
  select(-referral) %>% 
  #FIX PHQ9_5 
  dplyr::mutate(phq9_5 = dplyr::recode(phq9_5, '3- Nearly every day' ='1')) %>%
  ##Delete other cols that wont be released
  select(-Language, -dropout, -user_id, -age_group, -readEnglish, -baseline_phq9,
         -specreferral, -minority, -phonetype, -highest_education)


#for the repeat participants get the data that is most complete 
#1. NA brighten id
brighten_v1_NAs <- brighten_v1 %>% filter(is.na(brightenid))

#2. brightenid 
brighten_v1_nonNAs <- brighten_v1 %>% filter(!is.na(brightenid))
brighten_v1_nonNAs <-  brighten_v1_nonNAs %>% ddply(.variables = c('brightenid'), .fun=function(df){
  index <- sort(rowSums(is.na(df)),index.return=T, decreasing = F)$ix
  df[index[[1]],]
})
brighten_v1 <- rbind(brighten_v1_NAs, brighten_v1_nonNAs)

#### Splitting out IMPACT MHS assessment
brighten_v1_IMPACT_MHS <- brighten_v1 %>% select(brightenid, primary, group, medication, psychiatrist, therapist, otherhelp)
brighten_v1_baseline_PHQ9 <- brighten_v1 %>% select(brightenid, contains('phq9'))
brighten_v1 <- brighten_v1 %>% select(-primary, -group, -medication, -psychiatrist, -userid,
                                      -therapist, -otherhelp, -contains('phq9'),
                                      -sideation, -book) %>% 
  dplyr::mutate('study' = 'Brighten-v1',
                startDate = lubridate::mdy_hm(startDate))
colnames(brighten_v1) <- tolower(colnames(brighten_v1))
brighten_v1 <- as.data.frame(brighten_v1)


###############
#BRIGHTEN v2 data
###############
#metadata
brighten_v2 <- fread(synGet("syn11687434")$path, data.table=F) %>% 
  dplyr::mutate(time_submitted = lubridate::ymd_hms(time_submitted),
                date_submitted = as.Date(time_submitted),
                brightenid = userId,
                startdate = time_submitted,
                status = Status,
                study_arm = treatment) %>%
  dplyr::select(-heard_about_us_language, -english_native_language, -userId,
                -read_in_spanish,
                -bilingual, -read_english, -Language, -sum_phq10, -date_submitted, -time_started, 
                -groupColor, -treatment, -onBoardingTime, -userAgent, -iphone5_or_newer,
                -longitude, -latitude, -Referer, -hispanic_latino, -time_submitted, -android,
                -spanish_mother_tongue, -spanish_primary_language, -ip_address, -Status, -baseline_depression_state,
                -ipad, -spanish_primary_language_phone, -speak_spanish, -age_group) 
brighten_v2$`Response ID` <- NULL
brighten_v2$study_arm[!brighten_v2$study_arm %in% c('HTips', 'EVO', 'iPST')] = NA
brighten_v2_baseline_PHQ9 <- brighten_v2 %>% select(brightenid, contains('phq9'))
brighten_v2 <- brighten_v2 %>% select(-contains('phq9')) %>% dplyr::mutate('study' = 'Brighten-v2')
colnames(brighten_v2) <- tolower(colnames(brighten_v2))


#rearrange cols
brighten_v1 <- brighten_v1[,colnames(brighten_v2)]


### Combined merged dataset
#FULL_BRIGHTEN_DATA
x = rbind(brighten_v1, brighten_v2) %>% 
  dplyr::mutate(zipcode = substr(zipcode,1,3)) %>%
  dplyr::select(-city)

#FIX NA and "" as same
dim(x)
x <- apply(x, 2, function(x){
  fixrows <- is.na(x) | x == '' | x == 'NA' | x == 'N/A'
  x[fixrows] = NA
  as.character(x)
}) %>% as.data.frame()

#Fix income levels
x <- x %>% 
  dplyr::mutate(income_lastyear = 
                  case_when( income_lastyear %in% c('< 9000', '10,000-20,000',  '$20,000 or less') ~ '< $20,000',
                             income_lastyear %in% c('20,000-30,000', '20,000-40,000', '30,000-40,000') ~ '20,000-40,000',
                             income_lastyear %in% c('40,000-50,000', '40,000-60,000', '50,000-60,000') ~ '40,000-60,000',
                             income_lastyear %in% c('60,000-70,000', '60,000-80,000', '70,000-80,000') ~ '60,000-80,000',
                             income_lastyear %in% c('90,000-100,000', '80,000-90,000', '80,000-100,000') ~ '80,000-100,000',
                             income_lastyear == '100,000+' ~ "100,000+"
                             ))

tmp_replace_cols <- function(col, to_replace, by ){
  col <- as.character(col)
  torep <- col %in% c(to_replace)
  col[torep] = by
  col
}

#Fix study arms
table(x$study_arm)
x$study_arm <- tmp_replace_cols(x$study_arm, c('EVO_on_iphone_only', 'EVO', 'Akili'), 'EVO' )
x$study_arm <- tmp_replace_cols(x$study_arm, c('HealthTips_on_Android', 'HTips', 'Health Tips'), 'HealthTips' )

#Fix device
table(x$device)
x$device <- tmp_replace_cols(x$device, c('iOS', 'iPhone'), 'iPhone' )
x$device <- tmp_replace_cols(x$device, c('ANDROID', 'Android & iPad'), 'Android' )

##race
x$race <- tmp_replace_cols(x$race, 'Hispanic_Latino', 'Hispanic/Latino' )
x$race <- tmp_replace_cols(x$race, 'Non-hispanic White', 'Non-Hispanic White' )

#heard_about_us 
sort(table(x$heard_about_us))
x$heard_about_us <- tmp_replace_cols(x$heard_about_us, 'Craigslist-Jobs', 'Craigslist' )
x$heard_about_us <- tmp_replace_cols(x$heard_about_us, c('A Friend'), 'friend/colleague' )
x$heard_about_us <- tmp_replace_cols(x$heard_about_us, c("I can 't remember", "work", "websites/forums",
                                                         "Other (please be specific!)", "Church"), 'others' )
x$heard_about_us <- tmp_replace_cols(x$heard_about_us, c('Twitter'), 'Twitter/Facebook' )
x$heard_about_us <- tmp_replace_cols(x$heard_about_us, c('MUNI Advertisement'), 'Advertisement' )
x$heard_about_us <- tmp_replace_cols(x$heard_about_us, c('Recruited from other study'), 'through other studies' )

#marital_status
x$marital_status <- tmp_replace_cols(x$marital_status, c('Divorced', 'Separated', 'Widowed'), 'Separated/Widowed/Divorced' )
x$marital_status <- tmp_replace_cols(x$marital_status, c('Single/Never Married'), 'Single' )



##########
# Full baseline data
##########
FULL_BRIGHTEN_DATA = x %>% mutate(participant_id = as.character(brightenid)) %>%
  select(-brightenid) %>% select(participant_id, everything()) %>%
  arrange(participant_id)


#### ONLY HAVE ENROLLED PARTICIPANTS
FULL_BRIGHTEN_DATA_ENROLLED_PARTICIPANTS_ONLY <-  FULL_BRIGHTEN_DATA %>% filter(!is.na(participant_id))

##REMOVE ZIP CODE - PER GOVERNANCE REQUEST FOR DATA RELEASE
FULL_BRIGHTEN_DATA_ENROLLED_PARTICIPANTS_ONLY <- FULL_BRIGHTEN_DATA_ENROLLED_PARTICIPANTS_ONLY %>% dplyr::select(-zipcode, -country, -state, -status)


#write.csv(FULL_BRIGHTEN_DATA, file="tmp_BRIGHTEN_mdata.csv", quote = F, row.names = F)

### Push the data to Synapse
brighen_demog_data_synid <- synStore(synapser::synBuildTable("Baseline Demographics", SYNPROJECT, FULL_BRIGHTEN_DATA_ENROLLED_PARTICIPANTS_ONLY))

synSetProvenance(entity = brighen_demog_data_synid,
                 activity = synapser::Activity(used = c("syn10236547", "syn11687434"),
                                executed = "https://github.com/apratap/BRIGHTEN-Data-Release/blob/master/Create_baseline_metadata_files.R"))


########################################################
### Clean File for creating screening & enrollment map
########################################################
brighten_participant_zipcodes <- FULL_BRIGHTEN_DATA %>% 
  dplyr::filter(country == 'United States') %>%
  dplyr::select(state, country, zipcode, participant_id) %>%
  dplyr::mutate(status = ifelse(is.na(participant_id), 'screened', 'enrolled'))


write.table( brighten_participant_zipcodes, file="brighten_participant_zipcodes.tsv",
            sep="\t", row.names = F)




#########
## BASELINE PHQ-9
#########
brighten_v1_baseline_PHQ9 <- brighten_v1_baseline_PHQ9 %>% filter(!is.na(brightenid)) %>% 
  dplyr::mutate('study' = 'Brighten-v1') %>% select(-phq9_10, -sum_phq9)
brighten_v2_baseline_PHQ9 <- brighten_v2_baseline_PHQ9 %>% filter(!is.na(brightenid)) %>%
  dplyr::mutate('study' = 'Brighten-v2') %>% select(-baseline_phq9)

baseline_PHQ9_data <- rbind(brighten_v1_baseline_PHQ9, brighten_v2_baseline_PHQ9 %>% 
                              select(colnames(brighten_v1_baseline_PHQ9)))
baseline_PHQ9_data <- baseline_PHQ9_data %>% mutate(participant_id = as.character(brightenid)) %>% 
  filter(!is.na(participant_id))  %>% select(-brightenid) %>%
  select(participant_id, everything())
to_delete <- baseline_PHQ9_data %>% group_by(participant_id) %>% apply(1, function(x){
  sum(is.na(x) | x == '') == 9
})
baseline_PHQ9_data = baseline_PHQ9_data[!to_delete,]
baseline_PHQ9_data <- baseline_PHQ9_data %>% inner_join(FULL_BRIGHTEN_DATA %>%  select(participant_id, startdate)) %>%
  mutate(baselinePHQ9date = as.Date(lubridate::ymd_hms(startdate))) %>% select(-startdate)

View(baseline_PHQ9_data)

brighten_baseline_phq9_synid<- synStore(synapser::synBuildTable("Baseline PHQ9 Survey", SYNPROJECT, baseline_PHQ9_data))

synSetProvenance(entity = brighten_baseline_phq9_synid,
                 activity = synapser::Activity(used = c("syn10236547", "syn11687434"),
                                               executed = "https://github.com/apratap/BRIGHTEN-Data-Release/blob/master/Create_baseline_metadata_files.R"))



#write.csv(baseline_PHQ9_data, file="tmp_BRIGHTEN_baseline_PHQ9.csv", quote = F, row.names = F)




apply(FULL_BRIGHTEN_DATA, 2, function(x){
  if(length(unique(x)) <= 20 ){
    unique(x)
  }
  else {
    length(unique(x))
  }
})

