library(rvest);library(tidyverse)

# All data in this file are from the plugndrive.ca website
#read url
url = read_html('https://www.plugndrive.ca/electric-cars-available-in-canada/')

#extract names of cars
car_list <- url %>% html_nodes('.car-title') %>% html_text() %>% tolower() %>% 
    gsub(' ','-',.)

#extract prices
prices <- url %>% html_nodes('div.inner p.price-container span.starting-price') %>% html_text() %>% parse_number()

#extract each car url to use in pulls below
elec_urls <- url %>% html_nodes('div.evCar a') %>% html_attr('href')

## extract basic car info:
# category (PHEV vs full EV)
# Drive train (fwd, rwd, awd)
# Torque
# Number of passengers
# Cargo space in cubic feet
# Weight of the car in lbs
# Number of doors
# Motor (I don't know what this means)
the_car <- lapply(1:8, function(i) {map_chr(elec_urls, ~.x %>% 
                                                read_html() %>% html_nodes('div.feature-container') %>% html_nodes('div.specs p') %>% .[i] %>% 
                                                html_nodes('.value') %>% html_text())}) %>% 
    setNames(c('category','drive_train','torque','passengers','cargo_space','weight','doors','motor'))

## extract performance info:
# Horsepower
# Top speed
# Electric range
# 0-100 km/h time
# Total range (electric + gas)
performance <- lapply(1:5, function(i) {map_chr(elec_urls, ~.x %>% read_html() %>% 
                                                    html_nodes('div.other-features-container') %>% 
                                                    html_nodes('div.tab-content') %>% .[2] %>% 
                                                    html_nodes('.value') %>% html_text() %>% .[i])}) %>% 
    setNames(c('horsepower','top_speed','e_range','zero_to_hunnid','t_range'))

## extract energy efficiency info:
# City electric range (gas equivalent)
# Hwy electric range (gas equivalent)
# City gas range
# Hwy gas range
en_efficency <- lapply(1:4, function(i) {map_chr(elec_urls, ~.x %>% read_html() %>% 
                                                     html_nodes('div.other-features-container') %>% 
                                                     html_nodes('div.tab-content') %>% .[3] %>% 
                                                     html_nodes('.value') %>% html_text() %>% .[i])}) %>% 
    setNames(c('city_e','hwy_e','city_g','hwy_g'))

## extract battery info:
# Size in kWh
# Time to full charge (on level 2)
# Warranty
battery <- lapply(1:3, function(i) {map_chr(elec_urls, ~.x %>% read_html() %>% 
                                                html_nodes('div.other-features-container') %>% 
                                                html_nodes('div.tab-content') %>% .[4] %>% 
                                                html_nodes('.value') %>% html_text() %>% .[i])}) %>% 
    setNames(c('battery','charge_time','warranty'))


# extract emissions (CO2 kg)and cost ($) per 20,000 km driven for Ontario
emissions_and_cost <- lapply(list('span.emissions','span.cost'), function(i) {map_chr(elec_urls, ~.x %>% read_html() %>% 
                                                                                          html_nodes('div.emissions-table') %>% 
                                                                                          html_nodes('div.table-body') %>% 
                                                                                          html_nodes('div.electric') %>% 
                                                                                          html_nodes(i) %>% html_text() %>% .[5])}) %>% 
    setNames(c('emissions','cost'))

# Create table
cars_table <- data.frame(model = car_list, price = prices, 
                         lapply(list(the_car,performance,en_efficency,battery,emissions_and_cost),
                                function (i) {data.frame(sapply(i,c))}) %>% bind_cols())



