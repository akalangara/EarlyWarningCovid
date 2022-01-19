library(dplyr)
library(lubridate)

#### LOAD DATASETS ####
here::i_am('R/03_proportion_cases.R')
sub_state <- readRDS(here::here("Processed_Data", "sub_state_clean.rds"))
county_props <- readRDS(here::here("Processed_Data", "age_prop_dropNA.rds"))

# if not specified at run time
 min_date = "2021-05-31"
 max_date = "2021-12-31" 
# min_date = Sys.getenv("min_date")
# max_date = Sys.getenv("max_date")

num_months = (interval(ymd(min_date), ymd(max_date)) %/% months(1)) + 1

sub_state_grouped <- sub_state %>% group_by(state, time_interval) %>%
                     summarise(case_tot = sum(new_case),
                               death_tot = sum(new_death)) %>%
                     mutate(month=month(time_interval))

county_props_prep <- county_props %>% rename(state=res_state) %>%
                    mutate(month=month(sub_date))

state_list <- unique(sub_state_grouped$state)
age_list <- unique(county_props$age_group)
month_list <- (as.numeric(month(min_date))):(as.numeric(month(max_date)))

grab_dist <- function(state_info, borrow, x, m_month) {
     some_rows <- vector("list", 12)
     i <- 0
     for (j in 1:length(age_list)) {
          prop <- state_info %>% 
               filter(age_group == age_list[j]) %>%
               filter(month == borrow)
          a_row <- list(x,
                        "dist",
                        age_list[j],
                        NA,
                        prop$prop_case,
                        m_month)
          i <- i+1
          some_rows[[j]] <- a_row
     }
     some_rows <- some_rows[seq_len(i)]
     return(some_rows)
     }

more_rows <- lapply(state_list,function(x) {
     state_rows <- vector("list",51)
     
     state_info <- county_props_prep %>% 
          filter(state == x)
     
     k <- 0
     
     no_info_months <- setdiff(month_list,state_info$month)
     nans <- state_info %>% 
          filter(is.nan(prop_case))
     no_info_months <- append(no_info_months, unique(nans$month))
     
     for (m in 1:length(no_info_months)){
          m_month <- no_info_months[m]
          
          if (!((m_month - 1) %in% no_info_months) 
              & no_info_months[m] != month(min_date)){
               
               borrow <- m_month - 1
               some_rows <- grab_dist(state_info, borrow, x, m_month)
               k <- k + 1
               state_rows[[k]] <- some_rows
          }
          else if (!((m_month + 1) %in% no_info_months)
                   & no_info_months[m] != month(max_date)){
                    
                    borrow <- m_month + 1
                    some_rows <- grab_dist(state_info, borrow, x, m_month)
                    k <- k + 1
                    state_rows[[k]] <- some_rows
          }
          else if (no_info_months[m] == month(max_date) &
                   !((m_month - 2) %in% no_info_months)) {
               borrow <- m_month - 2
               some_rows <- grab_dist(state_info, borrow, x, m_month)
               k <- k + 1
               state_rows[[k]] <- some_rows
          }
          else if (no_info_months[m] == month(min_date) &
                   !((m_month + 2) %in% no_info_months)) {
               borrow <- m_month + 2
               some_rows <- grab_dist(state_info, borrow, x, m_month)
               k <- k + 1
               state_rows[[k]] <- some_rows
          }
          else {
               borrow <- m_month - 2
               some_rows <- grab_dist(state_info, borrow, x, m_month)
               k <- k + 1
               state_rows[[k]] <- some_rows
          }
     }
   state_rows <- state_rows[seq_len(k)]
   return(state_rows)
})

row_contents <- unlist(more_rows)
more_rows_m <-matrix(row_contents, ncol = 6, byrow=TRUE)
more_rows_m <-as.data.frame(more_rows_m)
colnames(more_rows_m) <- c("state","sub_date","age_group","case_n","prop_case","month")

full_county <- merge(county_props_prep,more_rows_m, by=c("month","state","age_group"), all=T) %>%
               mutate(prop = ifelse(is.na(prop_case.x),
                                    as.numeric(prop_case.y),
                                    as.numeric(prop_case.x)),
                      month = as.numeric(month)) %>%
               select(state, month, age_group, prop) %>%
               arrange(state,month)

merged_df <- merge(sub_state_grouped,full_county,by=c("month","state"), all=T) %>%
     mutate(case_per_age = round(case_tot*prop, digits=0)) %>%
     arrange(state,month)


saveRDS(merged_df, here::here("Processed_Data", "proportioned_cases.rds"))
