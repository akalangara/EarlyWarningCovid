library(dplyr)
library(data.table)
library(lubridate)

##### LOAD DATA #####
here::i_am('R/05_add_vacc.R')
vaccine_data <- fread(here::here("Raw_Data","COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv"),
                     select = c("Date","Location","Series_Complete_Pop_Pct"))
case_cc <- readRDS(here::here("Processed_Data", "04_case_cc.rds"))


##### SET TIME PERIOD #####
#if not specified at run time
min_date = "2021-05-03"
max_date = "2021-10-31" 
#min_date = Sys.getenv("min_date")
#max_date = Sys.getenv("max_date")

time_cuts <- seq(ymd(min_date),ymd(max_date) + 13, by = '2 week')


##### PREP VACCINE DATA #####
# filter by time period and location, assign time intervals
state_list <- unique(case_cc$state)
vacc_prep <- vaccine_data %>% 
             filter(Location %in% state_list) %>%
             mutate(Date = mdy(Date)) %>%
             filter(Date >= as.Date(min_date) & Date <= as.Date(max_date))%>%
             mutate(time_interval = cut(Date, breaks = time_cuts, 
                                include.lowest = FALSE,
                                right = FALSE))

##### GET AVG % OF POP VACCINATED PER TIME INTERVAL FOR EACH STATE #####
# avg across days in 2 wk time interval
avg_vac <- vacc_prep %>%
           group_by(Location, time_interval) %>%
           summarise(avg_pct_vacc_pct = mean(Series_Complete_Pop_Pct)) %>%
           ungroup() %>%
           rename(state = Location) %>%
           arrange(state, time_interval)


##### ADD TO DATASET #####
case_cc_vacc <- merge(case_cc,avg_vac, by=c("time_interval","state"), all=T) %>%
                arrange(state,month_dist_used)

##### EXPORT DATASET #####
saveRDS(case_cc_vacc, here::here("Processed_Data", "05_case_cc_vacc.rds"))
           