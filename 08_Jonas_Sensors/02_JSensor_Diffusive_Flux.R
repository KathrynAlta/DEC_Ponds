#######################################
# Jonas Sensors: Diffusive Flux 
#######################################
# Package on Jonas's Git Hub: https://github.com/JonasStage/FluxSeparator/tree/main 

#__________________________________________
# 0. Set Up R Environment 

  # Set Working Directory 
    setwd("~/DEC_Ponds/08_Jonas_Sensors")  # Desktop 

  # Install Jonas GitHUb
    library(FluxSeparator)

  # Install example data from Jonas 
    load(file='JSensor_Input_Data/DIY_sensor_data.rda')
    
  # Packages 
    
    library(lubridate)
    library(tidyverse)
    library(ggplot2)
    library(tibble)
    library(purrr)
    library(dplyr)
    library(stringr)
    library(TTR)
    library(ggpubr)
    
    install.packages("Rtools")
    install.packages("runVar")
    library(runVar)

#__________________________________________
# 1. Write Function for Diffusive Flux ( from Jonas's Code) 
    
    diffusive_flux <- function(data, concentration_values = "pred_CH4", station, runvar_cutoff = 0.5, remove_observations_prior = 200,
                               number_of_observations_used = 400, show_plots = T,IndexSpan = 30, cutoff_start_value,
                               number_of_observations_required = 50,number_of_pumpcycles_in_plot = 50, smooth_data = F,
                               look_for_bubbles = T) {
      
      GetIDsBeforeAfter = function(x,IndexSpan) {
        v = (x-IndexSpan) : (x+IndexSpan)
        v[v > 0]
      }
      
      if(smooth_data) {
        data %>%
          drop_na(concentration_values) %>%
          group_by(PumpCycle,sensor) %>%
          rename(concentration_raw = any_of(concentration_values)) %>%
          add_tally(name = "obs_in_PumpCycle") %>%
          filter(obs_in_PumpCycle > 100) %>%
          mutate(concentration_smooth = runMean(concentration_raw, 10),
                 concentration_smooth = runMean(concentration_smooth, 10),
                 concentration_smooth = runMean(concentration_smooth, 10),
                 concentration_smooth = runMean(concentration_smooth, 10),
                 concentration_smooth = runMean(concentration_smooth, 10)) -> data
      } else {
        data %>%
          rename(concentration_raw = contains(concentration_values)) -> data
      }
      
      data %>%
        colnames() %>%
        str_detect("concentration_smooth") %>%
        sum() -> smooth_present
      
      if(smooth_present == 1) {
        data %>% rename(concentration = concentration_smooth) -> data
      } else {
        data %>% rename(concentration = concentration_raw) -> data
      }
      
      if(look_for_bubbles) {
        data %>%
          add_count(PumpCycle) %>%
          filter(n > 100) %>%
          drop_na(concentration) %>%
          mutate(run_var5 = runVar(concentration, n = 5)) %>%
          ungroup() %>%
          mutate(row = row_number()) %>%
          {. ->> running_var_diff} %>%
          filter(run_var5 > runvar_cutoff) %>%
          mutate(time_diff = datetime -lag(datetime),
                 time_diff = as.numeric(time_diff)) %>%
          drop_na(time_diff) %>%
          mutate(gruppering =  1 + cumsum(time_diff>30)) %>%
          group_by(gruppering) %>%
          pull(row) %>%
          map(~GetIDsBeforeAfter(., IndexSpan)) %>%
          unlist() %>%
          unique() -> ids_to_remain_diff
        
        running_var_diff %>%
          filter(!row %in% ids_to_remain_diff) %>%
          mutate(time_diff = datetime -lag(datetime),
                 time_diff = as.numeric(time_diff)) %>%
          drop_na(time_diff) %>%
          mutate(gruppering =  1 + cumsum(time_diff>30)) %>%
          arrange(row) %>%
          {. ->> bubbles_diff} %>%
          group_by(PumpCycle,station) %>%
          mutate(first = first(concentration),
                 first = if_else(is.na(first),concentration,first)) %>%
          drop_na(time_diff) %>%
          mutate(min_grp = min(gruppering)) %>%
          filter(gruppering == min_grp) %>%
          drop_na(concentration) -> bubbles_removed_dataset
        
        bubbles_removed_dataset -> diffusive_dataset
        
      } else {
        data %>%
          group_by(PumpCycle,station) -> diffusive_dataset
      }
      
      diffusive_dataset %>%
        filter(between(row_number(),remove_observations_prior,(remove_observations_prior+number_of_observations_used))) %>%
        {. ->> dif_check} %>%
        mutate(time = datetime-min(datetime)) %>%
        nest() %>%
        mutate(model = map(data, ~lm(concentration ~ time, data = .)),
               slope = map(model, coef),
               start_value = map_dbl(data, ~min(.$concentration, na.rm=T)),
               n     = map(data, tally),
               r2    = map(model, summary),
               r2    = map_dbl(r2, "r.squared"),
               temp  = map_dbl(data, ~mean(.$tempC)),
               datetime_start = map_dbl(data, ~min(.$datetime, na.rm=T)),
               datetime_start = as_datetime(datetime_start),
               datetime_end = map_dbl(data, ~max(.$datetime, na.rm=T)),
               datetime_end = as_datetime(datetime_end)) %>%
        unnest_wider(slope) %>%
        unnest_wider(n) %>%
        rename(n_obs_included_in_lm = n) %>%
        filter(start_value < cutoff_start_value,
               n_obs_included_in_lm > number_of_observations_required) %>%
        {. ->> model_check} %>%
        mutate(slope_concentration_hr = time*3600,
               station = station) %>%
        select(station,datetime_start,datetime_end,slope_concentration_hr,n_obs_included_in_lm, r2,temp)-> diffusive_flux
      
      plotting_data <- dif_check %>%
        left_join(model_check, by = c("PumpCycle","station")) %>%
        mutate(plot_number = floor(PumpCycle/number_of_pumpcycles_in_plot))
      data_indelt <- data %>%
        mutate(plot_number = floor(PumpCycle/number_of_pumpcycles_in_plot))
      
      if(show_plots){
        for(i in unique(plotting_data$station)) {
          station_select = i
          for(j in unique(filter(plotting_data, station == station_select)$plot_number)){
            plot_number_select = j;
            plotting_data %>%
              filter(station == station_select , plot_number == plot_number_select) ->plot_lm
            data_indelt %>%
              filter(station == station_select , plot_number == plot_number_select) ->plot_raw
            par(ask=T)
            
            ggplot() +
              geom_point(data = plot_raw, aes(datetime, concentration, group = PumpCycle)) +
              geom_smooth(data = plot_lm, aes(datetime, concentration, group = PumpCycle, col = r2),
                          method = "lm", se =F, linewidth = 2, formula = y ~ x) +
              scale_color_gradient(limits = c(0,1), low = "red",high = "green") +
              labs(y = bquote("CH"[4]*" concentration (ppm)"),
                   x = "Datetime",
                   col = bquote("R"^2*"       "),
                   title = paste0("This is station: ",i)) +
              guides(col = guide_colourbar(barwidth = 20)) +
              theme(legend.position = "bottom") ->p
            print(p)
          }}} else {}
      par(ask=F)
      
      return(diffusive_flux) }
    
    
#__________________________________________
# 2. Run Diffusive flux function on example data from Jonas 
    
    result <- diffusive_flux(DIY_sensor_data, concentration_values = "pred_CH4",
                   station , runvar_cutoff = 0.5,
                   remove_observations_prior = 200,
                   number_of_observations_used = 400,
                   show_plots = TRUE, IndexSpan = 30,
                   cutoff_start_value = 5,
                   number_of_observations_required = 50,
                   number_of_pumpcycles_in_plot = 50,
                   smooth_data = FALSE,
                   look_for_bubbles = TRUE)
    
    
    JSensor_data_J1 <- JSensor_data[JSensor_data$sensor == "J1", ]
    JSensor_data_J2 <- JSensor_data[JSensor_data$sensor == "J2", ]
    JSensor_data_J3 <- JSensor_data[JSensor_data$sensor == "J3", ]
    
    diffusive_flux_result_J3 <- diffusive_flux(JSensor_data_J3, concentration_values = "pred_CH4",
                             station = sensor, runvar_cutoff = 0.5,
                             remove_observations_prior = 200,
                             number_of_observations_used = 400,
                             show_plots = TRUE, IndexSpan = 30,
                             cutoff_start_value = 5,
                             number_of_observations_required = 50,
                             number_of_pumpcycles_in_plot = 50,
                             smooth_data = FALSE,
                             look_for_bubbles = TRUE)

## Basic plot for Katie Brain 
    head(JSensor_data_J1)
    JSensor_data_J1_trim <- JSensor_data_J1[JSensor_data_J1$pred_CH4 <= 1000, ]
    JSensor_data_J2_trim <- JSensor_data_J2[JSensor_data_J2$pred_CH4 <= 1000, ]
    JSensor_data_J3_trim <- JSensor_data_J3[JSensor_data_J3$pred_CH4 <= 1000, ]
    
    J3_CH4 <- ggplot(JSensor_data_J3_trim, aes(x = datetime, y = pred_CH4)) +
      geom_point(color = "darkmagenta" ) + theme_bw() +
      labs(y = "CH4 concentration (ppm)",
           x = "Datetime",
           title = "J3")
    
    J3_CH4
    
    J3_CO2 <- ggplot(JSensor_data_J3, aes(x = datetime, y = K33_CO2)) +
      geom_point(color = "deepskyblue4" ) + theme_bw() +
      labs(y = "CO2 concentration (ppm)",
           x = "Datetime",
           title = "J3")
    
    J3_CO2
