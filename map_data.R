library(flexdashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(shiny)
library(httr)
library(rvest)
library(sf)

cafe = read_csv(here::here("data/Sidewalk_Caf__Licenses_and_Applications_clean.csv")) %>% 
  left_join(read_csv(here::here("data/zipcode.csv"))) %>% 
  unite("adress",
        building:state,
        sep = ",") %>% 
  select(business_name,adress,starts_with("swc"),long,lat,borough)

parking = read_csv(here::here("data/parking_vio2021_cleanv1.csv")) %>% 
  select(id,long,lat,issue_date,fine_amount)

get_nearby_area =
  function(p_long = 0, p_lat = 0,area=100) {
    return(
      tibble(
        long_lwr = p_long - area / (6378137 * cos(pi * p_lat / 180)) * 180 / pi,
        long_upr = p_long + area / (6378137 * cos(pi * p_lat / 180)) * 180 /
          pi,
        lat_lwr = p_lat - area / 6378137 * 180 / pi,
        lat_upr = p_lat + area / 6378137 * 180 / pi
      )
    )
  }

get_data =
  function(area) {
    data =
      parking %>%
      bind_cols(area) %>%
      filter(long < long_upr,
             long > long_lwr,
             lat < lat_upr,
             lat > lat_lwr) %>% 
      select(id,issue_date,fine_amount)
    
    return(data)
  }

cafe =
  cafe %>%
  mutate(
    nearby = map2(.x = long, .y = lat, ~ get_nearby_area(.x, .y)),
    nearby = map(.x = nearby,  ~ get_data(.x)),
    ticket = map_dbl(nearby, nrow)) %>% 
  drop_na(borough) %>% 
  unnest(nearby) %>% 
  mutate(hour = lubridate::hour(issue_date),
         weekday = weekdays(issue_date),
         weekday = 
           forcats::fct_relevel(weekday,
                                c("Monday","Tuesday","Wednesday","Thursday",
                                  "Friday","Saturday","Sunday")),
         ptile = ntile(ticket,3),
         n_risk = case_when(
           ptile == 1 ~"almost safe",
           ptile == 2 ~"some risk",
           ptile == 3 ~ "BURNING RISKY"
         )) %>% 
  nest(-business_name,-hour,-weekday) %>% 
  mutate(hour_day_risk = map_dbl(data,nrow)) %>% 
  unnest(data) %>% 
  distinct(business_name,hour,weekday,.keep_all = T) %>% 
  group_by(business_name) %>% 
  mutate(fine_amount = mean(fine_amount)) %>% 
  select(-id,-issue_date)

cafe %>%
  select(business_name, adress:n_risk) %>%
  full_join(cafe %>% distinct(business_name, weekday) %>%
              mutate(hour = list(1:24))) %>%
  unnest(hour) %>% 
  left_join(cafe) %>% 
  fill(0)


write_csv(cafe,"data/map_data.csv")

rm(parking)