rm(list=ls())
library("install.load")
install_load("tidyverse", "ggpubr")
source("Analysis/loadData.R")


annotation <- data.frame(app_type =  c('app_al', 'app_ct', 'app_ef',
                                       'app_md', 'app_mo','app_pm', 
                                       'app_rx', 'app_sl', 'app_wm'),
                         app_categ_fullname = c("Alcohol Management", 'Concentration', 'Exercise and Fitness',
                                                'Medical Care', 'Mood', 'Pain Mangement',
                                                'Relaxation', 'Sleep', 'Weight Management'))

other_apps_used <- other_apps_used %>% gather(app_type, used, 2:10) %>% inner_join(annotation)

other_apps_used_summary <- other_apps_used %>%
  group_by(app_categ_fullname) %>%
  summarise(n = sum(used),
            percent = (n / n_distinct(other_apps_used$participant_id)) * 100)  

p <- ggdotchart(other_apps_used_summary, x = "app_categ_fullname", y = "percent",
           color = "#FC4E07",                                
           sorting = "descending",                       
           add = "segments",                             
           rotate = TRUE,                               
           dot.size = 8,      
           xlab = "categories of apps used",
           label = round(other_apps_used_summary$percent),                        
           font.label = list(color = "white", size = 12, vjust = 0.5),              
           ggtheme = theme_pubr() )
p
ggsave(filename = "Figs_N_Tables/otherApps_used.png", plot=p, width = 5, height = 5, units="in", dpi = 250)



#######
##
#######
install_load("tableone")
tmp <- other_apps_used %>% select(-app_categ_fullname) %>% 
  spread(app_type, used) %>% 
  inner_join(demog) 

tmp$race <- relevel(factor(tmp$race), ref='Non-Hispanic White')


#Create a variable list which we want in Table 1
listVars <- c("gender","age", "race", 
              "income_lastyear", "marital_status",  "education")
#Define categorical variables
catVars <- c("gender", "education", "marital_status", "race",
             "income_lastyear", "working")

#Demographics 
demogTable = CreateTableOne(data=tmp, vars=listVars, factorVars = catVars, strata = "app_ct")
demogTableMat <- print(demogTable, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
print(demogTableMat)



