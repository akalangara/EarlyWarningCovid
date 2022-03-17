library("geepack")
library("tidyr")
library("caret")

#### LOAD DATASETS ####
here::i_am('R/10_run_model.R')
ds <- readRDS(here::here("Processed_Data", "09_prepped_data.rds"))

#### PREP FOR RUNNING MODEL #####
# create state_id variable, get rid of NA rows
ds$state_id <- as.numeric(factor(ds$state))

ds_peak <- ds %>% drop_na(peak_indicator)
ds_switches <- ds %>% drop_na(switches_indicator)
ds_first <- ds %>% drop_na(first_switch_indicator)

##### MODEL FORMULAS #####
# model formulas  (case_prop_18_49 used as ref)
mf_first_switch <- formula(first_switch_indicator ~ pct_delta + pop_sero_rate_16p +
                                avg_pct_vacc_pct + case_prop_0_17 + case_prop_50_64 +
                                case_prop_65p)

mf_switches <- formula(switches_indicator ~ pct_delta + pop_sero_rate_16p +
                            avg_pct_vacc_pct + case_prop_0_17 + case_prop_50_64 +
                            case_prop_65p)

mf_peak <- formula(peak_indicator ~ pct_delta + pop_sero_rate_16p +
                        avg_pct_vacc_pct + case_prop_0_17 + case_prop_50_64 +
                        case_prop_65p)

##### COLLINEARITY #####
# no collinearity?
m1 <- as.matrix(ds_first[,5:13])
colnames (m1) <- NULL
lc <- findLinearCombos(m1)

##### RUNNING MODELS ######
geeAr1_first <- geeglm(mf_first_switch, 
                       id=state_id, 
                       data=ds_first, 
                       family=binomial, 
                       corstr="ar1")
summary(geeAr1_first)

geeAr1_switches <- geeglm(mf_switches, 
                       id=state_id, 
                       data=ds_switches, 
                       family=binomial, 
                       corstr="ar1")
summary(geeAr1_switches)

geeAr1_peak <- geeglm(mf_peak, 
                       id=state_id, 
                       data=ds_peak, 
                       family=binomial, 
                       corstr="ar1")
summary(geeAr1_peak)

