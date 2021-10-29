rm(list = ls())
options(scipen = 999)

library(readr)
library(dplyr)

# `0_functions.R` contains to calculate distance between two observations
source("./0_source/0_functions.R")

# Creates a dataframe with the data to display
ships_df <- read_csv("./1_data/0_raw/ships_04112020.zip") %>%
   rename(ship_name = SHIPNAME,
          ship_id = SHIP_ID,
          curr_lat = LAT,
          curr_lon = LON,
          curr_dt = DATETIME,
          curr_speed = SPEED) %>%
   arrange(ship_type, ship_id, curr_dt) %>%
   group_by(ship_id) %>%
   mutate(prev_lat = lag(curr_lat, 1),
          prev_lon = lag(curr_lon, 1),
          prev_dt = lag(curr_dt, 1),
          prev_speed = lag(curr_speed, 1),
          dwt = mean(DWT, na.rm = TRUE),
          distance = haversine(curr_lat, curr_lon, prev_lat, prev_lon),
          n = n()) %>%
   # Keeps only the ships with more than one observation, and its longest journey
   filter(n > 1,
          distance == max(distance, na.rm = TRUE)) %>%
   # Keeps the most recent
   filter(curr_dt == max(curr_dt, na.rm = TRUE)) %>%
   ungroup() %>%
   # Ships 1071570 and 4497263 sailed 0 meters and got more than one observation,
   ## each with the same datetime.
   unique() %>%
   select(ship_type, ship_id, ship_name, curr_lat, curr_lon, curr_dt,
          prev_lat, prev_lon, prev_dt, curr_speed, prev_speed, dwt, distance) %>%
   tidyr::pivot_longer(!c(ship_type, ship_id, ship_name, distance, curr_dt,
                          prev_dt, curr_speed, prev_speed, dwt),
                       names_to = "coord",
                       values_to = "values") %>%
   tidyr::separate(coord, into = c("period", "coord"), sep = "_") %>%
   tidyr::pivot_wider(names_from = coord, values_from = values) %>%
   mutate(dt = as.POSIXct(ifelse(period == "curr",
                                 curr_dt,
                                 prev_dt),
                          origin = "1970-01-01",
                          tz = "UTC"),
          speed = ifelse(period == "curr",
                         curr_speed,
                         prev_speed)) %>%
   select(-c(curr_dt, prev_dt, curr_speed, prev_speed)) %>%
   write_csv("./1_data/1_processed/ships_df.csv")

# Creates a dataframe with unique ship types
ship_types <- ships_df %>%
   select(ship_type) %>%
   arrange(ship_type) %>%
   unique() %>%
   write_csv("./1_data/1_processed/ship_types.csv")

# Creates a dataframe with unique ships and its info (i.e. ship type and ID)
ships <- ships_df %>%
   select(ship_type, ship_id, ship_name) %>%
   arrange(ship_type, ship_id, ship_name) %>%
   unique() %>%
   write_csv("./1_data/1_processed/ship_list.csv")
