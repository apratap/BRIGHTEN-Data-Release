rm(list=ls())
source("Analysis/loadData.R")

library("install.load")
install_load("data.table", "gdata")
install_load("tidyverse")
install_load("gridExtra", "ggthemes", "tableone")

tmp_baselinePHQ9 <- baselinePHQ9 %>% 
  select(study, participant_id, sum_phq9, depression_categories, phq9_9)


tmp_baselinePHQ9 %>% group_by(study) %>% summarise(n = n(),
                                                   ideation = sum(phq9_9 > 0 ),
                                                   prop.ideation = ideation / n)


tmp.df <- merge(demog_flt, tmp_baselinePHQ9, all.x=T) %>%
  filter(!depression_categories %in% c('minimal')) %>%
  mutate(depression_categories = as.character(depression_categories),
         suicidalIdeation = ifelse(phq9_9 >0, T, F),
         age = as.numeric(age))


#Create a variable list which we want in Table 1
listVars <- c("gender","age", "age_group",  "device", "working", "race", "income_lastyear",
              "income_satisfaction", "marital_status",  "education")
#Define categorical variables
catVars <- c("gender","age_group", "device", "education", "marital_status", "race",
             "income_lastyear", "income_satisfaction", "working")
#Demographics 
demogTable = CreateTableOne(data=tmp.df, vars=listVars, factorVars = catVars, strata ="suicidalIdeation")
demogTableMat <- print(demogTable, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
print(demogTableMat)
