library(dplyr)
library(lubridate)
library(data.table)

##### LOAD DATA #####
here::i_am('R/07_add_var_prop.R')
case_perc <- readRDS(here::here("Processed_Data", "06_case_vacc_pct.rds"))
var_prop_data <- fread(here::here("Raw_Data","state_variant2.csv"))


##### PREP PROPORTION DATA FOR MERGE #####
just_prop <- var_prop_data %>% 
             mutate(time_interval = as.factor(ymd(WEEK_START))) %>%
             rename(state = STATE) %>%
             rename(pct_delta = PC_DELTA) %>%
             select(-c(NUM_SEQ,WEEK_START))


##### MERGE DATASETS #####
data_cleaned <- merge(case_perc,just_prop, by=c("time_interval","state"), all=F) %>%
     arrange(state,month_dist_used)


##### EXPORT DATASET #####
saveRDS(data_cleaned, here::here("Processed_Data", "07_var_added.rds"))
