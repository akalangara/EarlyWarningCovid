library(dplyr)
library(lubridate)
library(data.table)

##### LOAD DATA #####
here::i_am('R/08_add_indicators.R')
ds <- readRDS(here::here("Processed_Data", "07_var_added.rds"))


#### CREATE INDICATORS ####
# peak: where there was a max percentage change in incidence, 1st interval is NA
# pos: indicates + change in incidence from previous interval
# switches: all intervals where there is a shift from - to +, interval 1-2 are NA
# first switches: 1st time the shift from - to + occurs, interval 1-2 are NA
df <- ds %>% filter(age_group == "0 - 17 years") %>%
     filter(!(is.na(incidence_pct_change))) %>%
     group_by(state) %>%
     mutate(peak_indicator = if_else(incidence_pct_change == max(incidence_pct_change),1,0)) %>%
     mutate(pos_indicator = if_else(sign(incidence_pct_change) == -1, 0, 1)) %>%
     mutate(pos_lag = lag(pos_indicator)) %>%
     mutate(switches_indicator = if_else(pos_indicator-pos_lag == 1,1,0)) %>%
     ungroup()

first_switch_indicator <- ave(df$switches_indicator, df$state,
                              FUN = function(x) seq_along(x) == match(1, x, nomatch = 0))

df$first_switch_indicator <- first_switch_indicator
df$first_switch_indicator <- ifelse(is.na(df$switches_indicator),NA, 
                                     df$first_switch_indicator)


#### PREP AND MERGE ####
df <- df %>% 
     select(c(time_interval,state,peak_indicator,switches_indicator,first_switch_indicator))

merged_df <- merge(ds,df, by=c("time_interval","state"), all=T) %>%
        arrange(state,month_dist_used)


##### EXPORT DATASET #####
saveRDS(merged_df, here::here("Processed_Data", "08_indicator_added.rds"))
