library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(naniar)

##### LOAD DATA #####
here::i_am('R/09_add_sero.R')
ds <- readRDS(here::here("Processed_Data", "08_indicator_added.rds"))
sero_dat <- fread(here::here("Raw_Data","Nationwide__Blood_Donor_Seroprevalence_Survey_Infection-Induced_Seroprevalence_Estimates.csv"),
                     select = c("Region","Region Abbreviation","Year and Month",
                                "Median 
Donation Date","n [Total Prevalence]","Rate %[Total Prevalence]"))


##### PREP SERO DATA ######
# new column names, filter to  relavent time period and regions
sero_dat <- sero_dat %>% rename("region_abrv" = "Region Abbreviation",
                                "y_m" = "Year and Month",
                               "med_don_date"  = "Median 
Donation Date",
                                "n_sero_prevalence" = "n [Total Prevalence]",
                                "pop_sero_rate_16p" = "Rate %[Total Prevalence]") %>%
     filter(y_m >= min(ds$month_dist_used) & y_m <= max(ds$month_dist_used)) %>%
     filter(!(region_abrv %in% c("CR1","CR2","CR3","CR4","All")))


##### DEALING WITH SUBDIVIDED STATES ######
multi_sero_states <- c("AZ","CA","FL","IL","KS","KY","MD","MO","NV","NJ","NM","NY",
                       "OH","OR","PA","TN","TX","VA","WA","DC")

# states and their corresponding regions
multi_sero_dict <- list(
                    "AZ" = c("AZ-1","AZ-2","NV-2"),
                    "CA" = c("CA-1","CA-2","CA-3"),
                    "FL" = c("FL-1","FL-2"),
                    "IL" = c("IL-1","IL-2"),
                    "KS" = c("KS-1","KS-2"),
                    "KY" = c("IL-2","KY"),
                    "MD" = c("DE","MD","WV"),
                    "MO" = c("AR","IL-2","KS-2","MO"),
                    "NV" = c("NV-1","NV-2"),
                    "NJ" = c("NJ","NY-1"),
                    "NM" = c("NM-1","NM-2"),
                    "NY" = c("NY-1","NY-2","NY-3"),
                    "OH" = c("OH-1","OH-2"),
                    "OR" = c("OR-1","OR-2"),
                    "PA" = c("NJ","PA"),
                    "TN" = c("TN-1","TN-2"),
                    "TX" = c("NM-2","TX-1","TX-2"),
                    "VA" = c("VA","WV"),
                    "WA" = c("OR-1","WA-1","WA-2"),
                    "DC" = c("MD"))

# find the max seroprevalance among regions for subdivided states
i <- 0
calc_sds <- lapply(multi_sero_dict, function(x) {
     i <<- i+1
     state_ds <-sero_dat %>% filter(region_abrv %in% x)%>%
          select (-c(Region,med_don_date)) %>%
          group_by(y_m) %>%
          summarise(n_sero_prevalence = sum(n_sero_prevalence),
                 pop_sero_rate_16p = max(pop_sero_rate_16p))
     state_ds$region_abrv <- multi_sero_states[i]
     return(state_ds)
})

multi_region_df <- data.table::rbindlist(calc_sds)


##### PULL DATA FOR STATES THAT WEREN'T SUBDIVIDED #####
single_sero_state_df <- sero_dat %>% filter((nchar(region_abrv, type = "chars") == 2) 
                                            & !(region_abrv %in% multi_sero_states)) %>%
        select(-c(Region, med_don_date))


##### MERGE SINGLE, MULTI STATE DATA, AND DS #####
all_states <- rbind(single_sero_state_df,
                    multi_region_df, fill = TRUE)
all_states <- all_states %>% rename(month_dist_used = y_m,
                                    state = region_abrv)

all_data <- merge(ds, all_states, by=c("state","month_dist_used"), all=F)


##### REFORMAT DATA FOR ANALYSIS ######
no_repeats <- all_data %>% filter(age_group == "0 - 17 years")

long2wide_age <- all_data %>% select(state, time_interval, age_group, prop) %>%
        pivot_wider(names_from = age_group, values_from = prop) %>% 
        arrange(state,time_interval)
new_col_names <- c("case_prop_0_17", "case_prop_18_49", 
                   "case_prop_50_64", "case_prop_65p")
colnames(long2wide_age)[3:6]<-new_col_names


##### KEEPING ONLY RELEVANT COLUMNS FOR ANALYSIS #####
all_data_cleaned <- merge(no_repeats, long2wide_age,  by=c("state","time_interval")) %>% 
                    select(state, time_interval, avg_pct_vacc_pct, 
                           incidence_pct_change, pct_delta, peak_indicator,
                           switches_indicator, first_switch_indicator, 
                           pop_sero_rate_16p, case_prop_0_17,
                           case_prop_18_49,
                           case_prop_50_64, case_prop_65p)


##### EXPORT DATASET #####
saveRDS(multi_region_df, here::here("Processed_Data", "09_multi_region_df.rds"))
saveRDS(all_data_cleaned, here::here("Processed_Data", "09_prepped_data.rds"))


