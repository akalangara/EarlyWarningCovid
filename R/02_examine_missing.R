library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(plotly)

#### LOAD DATASETS ####
here::i_am('R/02_examine_missing.R')
sub_state <- readRDS(here::here("Processed_Data", "sub_state.rds"))
sub_county <- readRDS(here::here("Processed_Data", "sub_county.rds"))



#### NEEDED FUNCTIONS ####
find_count_prop <- function(x) {
        counts <- table(is.na(x))
        props <- prop.table(counts)
        counts <- data.frame(unclass(counts))
        props <- data.frame(unclass(props))
        if (nrow(props) > 1) {
                props$label <- c("Complete","Missing")
                counts$label <- c("Complete","Missing")
                missing_df <- merge(counts,props,by="label")
        }
}

make_pies <- function(x) {
        i <<- i+1
        if(!is.null(x)) {
                total = sum(x$unclass.counts.)
                colors <- c('rgb(39, 163, 72)', 'rgb(128,133,133)')
                fig <- plot_ly(x, labels = ~label, values = ~unclass.props., 
                               type = 'pie',
                               textposition = 'inside',
                               textinfo = 'label+percent',
                               insidetextfont = list(color = '#FFFFFF'),
                               hoverinfo = 'text',
                               text = ~paste(unclass.counts., '/', total),
                               marker = list(colors = colors,
                                             line = list(color = '#FFFFFF', 
                                                         width = 1)),
                               showlegend = FALSE)
                fig <- fig %>% layout(title = title_list[i],
                                      xaxis = list(showgrid = FALSE, 
                                                   zeroline = FALSE, 
                                                   showticklabels = FALSE),
                                      yaxis = list(showgrid = FALSE, 
                                                   zeroline = FALSE, 
                                                   showticklabels = FALSE))
                
                fig
        }
}

graph_state <- function(x){
        scatter_xx <- age_prop_dropNA %>%
                filter (res_state == x) %>%
                ggplot(aes(x = sub_date, y = prop_cases*100, colour = age_group,
                           text = paste(
                                   "State: ", res_state, "\n",
                                   "Month: ", sub_date, "\n",
                                   "Age Group: ", age_group, "\n",
                                   "Case Count: ", case_n, "\n",
                                   "% of Cases: ", round((prop_cases*100),digits = 0), "%\n",
                                   sep = ""
                           ))) + 
                geom_point(alpha=0.7) + 
                labs(x = "Month", 
                     y = "% of Cases",
                     title = paste("% Cases Per Age Group Over Study Period: ", x)) +
                scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
                theme_classic()
        age_prop_scatter_xx <- ggplotly(scatter_xx, tooltip = "text")
        return(age_prop_scatter_xx)
}
        
create_buttons <- function(df, y_axis_var_names) {
        lapply(y_axis_var_names,
                FUN = function(var_name, df) {
                        button <- list(
                                method = 'restyle',
                                args = list('y', list(df[, var_name])),
                                label = sprintf('Show %s', var_name)
                        )
                },
                df
        )
        
}



#### EXAMINE MISSING DATA STATE ####
## some states don't list conf_death and conf_cases
## these are empty cells that are transformed to NA
## turn case counts to numeric
sub_state_clean <- sub_state %>% mutate_all(na_if,"") %>% 
     transform(conf_cases = as.numeric(gsub(",","",conf_cases)), 
               new_case = as.numeric(gsub(",","", new_case)),
               conf_death = as.numeric(gsub(",","",conf_death)), 
               new_death = as.numeric(gsub(",","",new_death)))

## tabulate the count and proportion of NAs for each variable
state_missing <- sapply(sub_state_clean, FUN=find_count_prop)

## create pie charts to show degree of missing
title_list <- names(state_missing)
i<-0
state_pies <- sapply(state_missing, FUN=make_pies)

## get rid of null pies (100% complete)
state_pies <- state_pies[!sapply(state_pies,is.null)]



#### EXAMINE MISSING DATA COUNTY ####
## combining missing, unknown, and NA for death_yn
sub_county_clean <- sub_county %>%
                    mutate(death_yn = case_when(
                            (death_yn == "Missing" | death_yn == "Unknown")
                            ~ NA_character_, TRUE ~ death_yn))

## tabulate the count and proportion of NAs for each variable
county_missing <- sapply(sub_county_clean, FUN=find_count_prop)

## create pie charts to show degree of missing
title_list <- names(county_missing)
i<-0
county_pies <- sapply(county_missing, FUN=make_pies)

## get rid of null pies (100% complete)
county_pies <- county_pies[!sapply(county_pies,is.null)]

# drop cases where state is NA
sub_county_clean <- sub_county_clean %>%
                    filter(!(is.na(res_state)))

## look at proportion of missing throughout time for each state
age_prop_NAs <- sub_county_clean %>% group_by(res_state, sub_date, age_group) %>%
     summarise(case_n = n()) %>%
     mutate(prop_cases = case_n / sum(case_n)) %>%
     filter(is.na(age_group)) %>%
     ungroup()

NA_scatter <- age_prop_NAs %>% 
        ggplot(aes(x = sub_date, y = prop_cases*100, 
                   text = paste(
                           "State: ", res_state, "\n",
                           "Month: ", sub_date, "\n",
                           "NA Case Count: ", case_n, "\n",
                           "% NA: ", round((prop_cases*100),digits = 0), "%\n",
                           sep = ""
                   ))) + 
        geom_point(alpha=0.7, colour = "#51A0D5") + 
        labs(x = "Month", 
             y = "% of Cases Where Age Group Was Not Documented",
             title = "Percentage of NAs Over Study Period") +
        scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
        theme_classic()

NA_scatter <- ggplotly(NA_scatter, tooltip = "text")

## drop age group NAs and recalculate proportions
age_prop_dropNA <- sub_county_clean %>% group_by(res_state, sub_date, age_group) %>%
        summarise(case_n = n()) %>% 
        ungroup() %>%
        complete(age_group, nesting(res_state, sub_date),
                 fill = list(case_n = 0)) %>%
        filter(!(is.na(age_group))) %>%
        group_by(res_state, sub_date) %>%
        mutate(prop_cases = case_n / sum(case_n)) %>%
        arrange(res_state, sub_date)
        ungroup()

## look at proportion for each age group throughout time
### all states
age_prop_scatter <- age_prop_dropNA %>% 
        ggplot(aes(x = sub_date, y = prop_cases*100, colour = age_group,
                   text = paste(
                           "State: ", res_state, "\n",
                           "Month: ", sub_date, "\n",
                           "Age Group: ", age_group, "\n",
                           "Case Count: ", case_n, "\n",
                           "% of Cases: ", round((prop_cases*100),digits = 0), "%\n",
                           sep = ""
                   ))) + 
        geom_point(alpha=0.7) + 
        labs(x = "Month", 
             y = "% of Cases",
             title = "Percentage of Cases Per Age Group Over Study Period") +
        scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
        theme_classic()
age_prop_scatter <- ggplotly(age_prop_scatter, tooltip = "text")

### selected states
MO <- graph_state("MO")
OR <- graph_state("OR")
TN <- graph_state("TN")
TX <- graph_state("TX")
CA <- graph_state("CA")
NY <- graph_state("NY")

#### EXPORTING CHARTS AND CLEANED DATA ####
saveRDS(sub_state_clean, here::here("Processed_Data", "sub_state_clean.rds"))
saveRDS(age_prop_dropNA, here::here("Processed_Data", "age_prop_dropNA.rds"))

saveRDS(state_pies, here::here("Exploratory_Analysis", "state_pies.rds"))
saveRDS(county_pies, here::here("Exploratory_Analysis", "county_pies.rds"))

saveRDS(NA_scatter, here::here("Exploratory_Analysis", "NA_scatter.rds"))

saveRDS(age_prop_scatter, here::here("Exploratory_Analysis", "age_prop_scatter.rds"))
saveRDS(age_prop_scatter_MO, here::here("Exploratory_Analysis", "age_prop_scatter_MO.rds"))
saveRDS(age_prop_scatter_OR, here::here("Exploratory_Analysis", "age_prop_scatter_OR.rds"))
saveRDS(age_prop_scatter_TN, here::here("Exploratory_Analysis", "age_prop_scatter_TN.rds"))
saveRDS(age_prop_scatter_TX, here::here("Exploratory_Analysis", "age_prop_scatter_TX.rds"))
saveRDS(age_prop_scatter_CA, here::here("Exploratory_Analysis", "age_prop_scatter_CA.rds"))
saveRDS(age_prop_scatter_NY, here::here("Exploratory_Analysis", "age_prop_scatter_NY.rds"))
