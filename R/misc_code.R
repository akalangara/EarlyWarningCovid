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

button_list <- list(list(method = "restyle",
                         args = list("state", age_prop_dropNA),
                         label = "All States"))

more_buttons <- lapply (as.factor(unique(age_prop_dropNA$res_state)),
                        FUN = function(x){
                             sub_df <- age_prop_dropNA %>% 
                                  filter (res_state == as.character(x))
                             a_state <- list(method = "restyle",
                                             args = list(sub_df),
                                             label = x)
                        })

button_list_full <- append(button_list,more_buttons)

age_prop_scatter <- ggplotly(age_prop_scatter, tooltip = "text") %>%
     layout(updatemenus = list(
          list(type = 'dropdown',
               active = 0,
               buttons = button_list_full)))