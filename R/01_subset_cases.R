library(dplyr)
library(lubridate)
library(data.table)
library(naniar)

##### LOAD DATA (ONLY READING IN NEEDED COLUMNS) #####
here::i_am('R/01_subset_cases.R')
state_cases <- fread(here::here("Raw_Data","United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv"),
                     select = c("submission_date","state","conf_cases",
                                "new_case","conf_death","new_death"))
county_cases <- fread(here::here("Raw_Data","COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv"),
                      select = c("case_month","res_state","state_fips_code",
                                 "age_group","current_status","death_yn"))


##### SET TIME PERIOD #####
#if not specified at run time
#min_date = "2021-05-31"
#max_date = "2021-12-31" 
min_date = Sys.getenv("min_date")
max_date = Sys.getenv("max_date")

time_cuts <- seq(ymd(min_date),ymd(max_date) + 13, by = '2 week')


##### US TERRITORIES AND DC TO BE DROPPED #####
drop_fips <- c("AS","DC","FSM","GU","MP","PR","PW", "RMI")


##### SUBSET STATE CASES #####
## subset by time & geography
## add new var for time interval (2wk periods matching variant prop data)
## NYC and NY combined
sub_state <- state_cases %>%
        mutate(sub_date = mdy(submission_date)) %>%
        mutate(state = case_when(
                state == "NYC" ~ "NY", TRUE ~ state))%>%
        select(-submission_date) %>%
        filter(sub_date >= as.Date(min_date) & sub_date <= as.Date(max_date)) %>%
        mutate(time_interval = cut(sub_date, breaks = time_cuts, 
                                   include.lowest = FALSE,
                                   right = FALSE))
setkey(sub_state, state)
sub_state <- sub_state[-sub_state[drop_fips, which=TRUE]]


##### SUBSET COUNTY CASE #####
## subset by time, geography and status
## missing and repressed (NA) data were combined for age_group
sub_county <- county_cases %>%
        mutate(sub_date = ym(case_month)) %>%
        mutate(age_group = case_when(
        age_group == "Missing" ~ NA_character_, TRUE ~ age_group)) %>%
        select(-case_month) %>%
        filter(sub_date >= as.Date(min_date) & sub_date <= as.Date(max_date) &
                       current_status == "Laboratory-confirmed case")
setkey(sub_county, res_state)
sub_county <- sub_county[!sub_county$res_state %in% drop_fips,]


##### EXPORTING SUBSETTED DATA #####
saveRDS(sub_state, here::here("Processed_Data", "sub_state.rds"))
saveRDS(sub_county, here::here("Processed_Data", "sub_county.rds"))
