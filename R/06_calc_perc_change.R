library(dplyr)
library(lubridate)
library(data.table)

##### LOAD DATA #####
here::i_am('R/06_calc_perc_change.R')
case_vacc <- readRDS(here::here("Processed_Data", "05_case_cc_vacc.rds"))
state_pops_df <- fread(here::here("Raw_Data","state_pop.csv"))

##### ADD POP AND CALC CASE INCIDENCE RATE PER 100K #####
state_pop_all <- state_pops_df %>% 
              mutate(state_pop = POP_EST_JUL_21) %>%
              mutate (state = State) %>%
              select(-State) %>%
              select(state, state_pop)

case_vacc_ci <- merge(case_vacc,state_pop_all, by="state", all=T) %>%
                mutate(incidence_per_100k = (case_tot/state_pop)*100000)


##### CALC % CHANGE IN CASE INCIDENCE RATE AND CASE COUNTS #####
case_perc <- case_vacc_ci %>%
                 arrange(state,time_interval) %>%
                 group_by(state) %>%
                 mutate(count_lag = lag(case_tot)) %>%
                 mutate(ci_lag = lag(incidence_per_100k)) %>%
                 mutate(case_count_pct_change = ((case_tot-count_lag)/count_lag)*100) %>%
                 mutate(incidence_pct_change = ((incidence_per_100k-ci_lag)/ci_lag)*100) %>%
                 ungroup() %>%
                 select(-c(count_lag,ci_lag))
                 
     
##### EXPORT DATASET #####
saveRDS(case_perc, here::here("Processed_Data", "06_case_vacc_pct.rds"))

