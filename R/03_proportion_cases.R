library(dplyr)
library(lubridate)

#### LOAD DATASETS ####
here::i_am('R/03_proportion_cases.R')
sub_state <- readRDS(here::here("Processed_Data", "02_sub_state_clean.rds"))
county_props <- readRDS(here::here("Processed_Data", "02_age_prop_dropNA.rds"))

##### SET TIME PERIOD #####
# if not specified at run time
 min_date = "2021-05-03"
 max_date = "2021-10-31" 
# min_date = Sys.getenv("min_date")
# max_date = Sys.getenv("max_date")

##### PREP STATE DATA #####
# sum number of new cases and deaths in an interval
# if an interval begins on the 24th or later, use next month distr
sub_state_grouped <- sub_state %>% group_by(state, time_interval) %>%
                     summarise(case_tot = sum(new_case),
                               death_tot = sum(new_death)) %>%
                     mutate(year_month = case_when(
                        day(time_interval) >= 24 ~ ceiling_date(as.Date(time_interval), "month") ,
                        TRUE ~ floor_date(as.Date(time_interval), "month"),
                     ))

##### PREP COUNTY DATA #####
# rename variables to match state data
county_props_prep <- county_props %>% rename(state=res_state, 
                                             year_month = sub_date)

##### FINDING STATES THAT NEED DIFFERENT DISTRIBUTIONS ######
# states_missing: states that are missing a month entirely
# states_0_prop: states where there were no cases (most likely missing data)
# state_list:  all states that need different distributions
age_list <- unique(county_props$age_group)
time_list <- seq(min(sub_state_grouped$year_month),
                 max(sub_state_grouped$year_month), by = '1 month')
states_missing <- data.frame(table(county_props_prep$state)) %>% 
                  filter (Freq < length(time_list) * length(age_list)) %>%
                  mutate(state = as.character(Var1))
state_0_prop <- county_props_prep %>% filter(is.nan(prop_cases))
miss_list <- states_missing$state
list_0 <- unique(state_0_prop$state)
state_list <- union(miss_list,list_0)

##### FUNCTION TO GRAB APPROPRIATE DISTRIBTION ######
# creates the rows that are missing using borrowed distributions
grab_dist <- function(state_info, borrow, state_name, miss_ym) {
   some_rows <- vector("list", 12)
   i <- 0
   for (j in 1:length(age_list)) {
      prop <- state_info %>% 
         filter(age_group == age_list[j]) %>%
         filter(year_month == borrow)
      a_row <- list(state_name,
                    "dist",
                    age_list[j],
                    NA,
                    prop$prop_cases,
                    miss_ym)
      i <- i+1
      some_rows[[j]] <- a_row
   }
   some_rows <- some_rows[seq_len(i)]
   return(some_rows)
}

##### PROPORTION CASES #####
# for each state, finds which months are missing entirely and which have 0 proportions
# determines which month is closest to the missing months based on difference in number of days
# then grab distributions based on the closest month (borrow)
# returns list of lists
more_rows <- lapply(state_list,function(state_name) {
     state_rows <- vector("list",60)
     
     state_info <- county_props_prep %>% 
          filter(state == state_name) %>% filter(!(is.nan(prop_cases)))
     
     k <- 0
     
     no_info_months <- setdiff(as.character(time_list),as.character(state_info$year_month))
     nans <- state_info %>% 
          filter(is.nan(prop_cases))
     no_info_months <- append(no_info_months, unique(nans$year_month))
     
     for (m in 1:length(no_info_months)){
          miss_ym <- as.Date(no_info_months[m])
          diffs <- lapply(unique(state_info$year_month), function(x) {
             as.numeric(abs(difftime(miss_ym,x, units = "days")))
          })
          
          closest <- which.min(diffs)[1]
          borrow <- unique(state_info$year_month)[closest]
          some_rows <- grab_dist(state_info, borrow, state_name, miss_ym)
          k <- k + 1
          state_rows[[k]] <- some_rows
          
     }
   state_rows <- state_rows[seq_len(k)]
   return(state_rows)
})

# get list of list output into matrix format and then dataframe format
row_contents <- unlist(more_rows)
more_rows_m <-matrix(row_contents, ncol = 6, byrow=TRUE)
more_rows_m <-as.data.frame(more_rows_m)
colnames(more_rows_m) <- c("state","sub_date","age_group","case_n","prop_cases","ym")
more_rows_m <- more_rows_m %>% mutate(year_month = ymd("1970-01-01") + days(ym)) %>%
               select(-ym)

##### MERGE TO FULL DATA #####
# merge additional generated rows with the rest of the data
full_county <- merge(county_props_prep,more_rows_m, by=c("year_month","state","age_group"), all=T) %>%
               mutate(prop = ifelse(is.na(prop_cases.x),
                                    as.numeric(prop_cases.y),
                                    as.numeric(prop_cases.x)))%>%
               select(state, year_month, age_group, prop) %>%
               arrange(state,year_month)

# merge back into full dataset
merged_df <- merge(sub_state_grouped,full_county,by=c("year_month","state"), all=T) %>%
     mutate(case_per_age = round(case_tot*prop, digits=0)) %>%
     rename(month_dist_used = year_month) %>%
     mutate (month_dist_used = format(month_dist_used, "%Y-%m"))%>%
     arrange(state,month_dist_used)


##### EXPORTING DATASET #####
saveRDS(merged_df, here::here("Processed_Data", "03_proportioned_cases.rds"))
