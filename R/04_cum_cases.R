library(dplyr)
library(lubridate)
library(data.table)

##### LOAD DATA #####
here::i_am('R/04_cum_cases.R')
state_cases <- fread(here::here("Raw_Data","United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv"),
                     select = c("submission_date","state","conf_cases",
                                "new_case","conf_death","new_death"))
prop_cases <- readRDS(here::here("Processed_Data", "03_proportioned_cases.rds"))


##### SET TIME PERIOD #####
#if not specified at run time
min_date = "2021-05-03"
max_date = "2021-10-31" 
#min_date = Sys.getenv("min_date")
#max_date = Sys.getenv("max_date")

time_cuts <- seq(ymd(min_date),ymd(max_date) + 13, by = '2 week')

##### US TERRITORIES TO BE DROPPED #####
drop_fips <- c("AS","FSM","GU","VI","MP","PR","PW","RMI")

#### PREP STATE LEVEL DATA TO GET CUMULATIVE CASES ####
cc_prep <- state_cases %>%
     mutate(sub_date = mdy(submission_date)) %>%
     mutate(state = case_when(
          state == "NYC" ~ "NY", TRUE ~ state))%>%
     select(-submission_date) %>%
     mutate(time_interval = cut(sub_date, breaks = time_cuts,
                                include.lowest = FALSE,
                                right = FALSE)) %>%
     mutate_all(na_if,"") %>% 
     transform(conf_cases = as.numeric(gsub(",","",conf_cases)),  # nix commas
               new_case = as.numeric(gsub(",","", new_case)),
               conf_death = as.numeric(gsub(",","",conf_death)), 
               new_death = as.numeric(gsub(",","",new_death)))

setkey(cc_prep, state)
cc_prep <- cc_prep[-cc_prep[drop_fips, which=TRUE]]

#### GET CUMULATIVE CASE NUMBERS ####
cc <- cc_prep %>%
      group_by(state) %>%
      arrange(state, sub_date) %>%
      mutate(cumulative_cases = cumsum(new_case)) %>%
      ungroup() %>%
      filter(!(is.na(time_interval))) %>%
      group_by(state,time_interval) %>%
      top_n(1, cumulative_cases) %>%
      filter(new_case != 0) %>%
      select(state, time_interval, cumulative_cases)

#### ADD IN CUMULATIVE CASES TO DATASET ####
case_cc <- merge(prop_cases,cc, by=c("time_interval","state"), all=T) %>%
     arrange(state,month_dist_used)

#### EXPORT DATASET ####
saveRDS(case_cc, here::here("Processed_Data", "04_case_cc.rds"))
