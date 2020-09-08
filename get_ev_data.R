library(rvest);library(tidyverse);library(formattable)

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
    setNames(c('category','drive_train','torque_lb_ft','passengers','cargo_space_cubic_feet','weight_lbs','doors','motor'))

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
    setNames(c('horsepower','top_speed_km_per_h','e_range_km','zero_to_hunnid_s','t_range_km'))

## extract energy efficiency info:
# City electric range (gas equivalent)
# Hwy electric range (gas equivalent)
# City gas range
# Hwy gas range
en_efficency <- lapply(1:4, function(i) {map_chr(elec_urls, ~.x %>% read_html() %>% 
                                                     html_nodes('div.other-features-container') %>% 
                                                     html_nodes('div.tab-content') %>% .[3] %>% 
                                                     html_nodes('.value') %>% html_text() %>% .[i])}) %>% 
    setNames(c('city_e_L100','hwy_e_L100','city_g_L100','hwy_g_L100'))

## extract battery info:
# Size in kWh
# Time to full charge (on level 2)
# Warranty
battery <- lapply(1:3, function(i) {map_chr(elec_urls, ~.x %>% read_html() %>% 
                                                html_nodes('div.other-features-container') %>% 
                                                html_nodes('div.tab-content') %>% .[4] %>% 
                                                html_nodes('.value') %>% html_text() %>% .[i])}) %>% 
    setNames(c('battery_kWh','charge_time_h_lvl2','warranty'))


# extract emissions (CO2 kg)and cost ($) per 20,000 km driven for Ontario
emissions_and_cost_elec <- lapply(list('span.emissions','span.cost'), function(i) {map_chr(elec_urls, ~.x %>% read_html() %>% 
                                                                                          html_nodes('div.emissions-table') %>% 
                                                                                          html_nodes('div.table-body') %>% 
                                                                                          html_nodes('div.electric') %>% 
                                                                                          html_nodes(i) %>% html_text() %>% .[5])}) %>% 
    setNames(c('emissions_e_kg','cost_e_cnd'))

emissions_and_cost_gas <- lapply(list('span.emissions','span.cost'), function(i) {map_chr(elec_urls, ~.x %>% read_html() %>% 
                                                                                               html_nodes('div.emissions-table') %>% 
                                                                                               html_nodes('div.table-body') %>% 
                                                                                               html_nodes('div.gas') %>% 
                                                                                               html_nodes(i) %>% html_text() %>% .[5])}) %>% 
    setNames(c('emissions_g_kg','cost_g_cnd'))

# Create table
cars_table <- data.frame(model = car_list, price_cnd = prices, 
                         lapply(list(the_car,performance,en_efficency,battery,emissions_and_cost_elec,emissions_and_cost_gas),
                                function (i) {data.frame(sapply(i,c))}) %>% bind_cols()) %>% select(-warranty) %>% 
    mutate(charge_time_h_lvl2 = gsub('Level 2','',charge_time_h_lvl2)) %>% 
    mutate_at(vars(matches('e_L100$')),~gsub('Litres Equivalent/100 km','',.)) %>% 
    mutate_at(vars(matches('g_L100$')),~gsub('Litres/100 km','',.)) %>% 
    mutate(torque_lb_ft = gsub('lb. ft.','',torque_lb_ft)) %>% 
    mutate_at(vars(-c(model,category,drive_train)),~as.numeric(gsub("[^0-9.-]+", "",.))) %>% 
    mutate(model = tools::toTitleCase(gsub('-',' ',model)),
           model = gsub(' Phev','',model)) %>% 
    mutate_at(vars(matches('L100$|lvl2$')),~comma(.,1))
    # mutate_at(vars(matches('cnd$')),~scales::dollar(.,largest_with_cents = 1))

write.csv(cars_table,'C:/Users/jimmy/OneDrive/Documents/repos/electric_cars/cars_table.csv', row.names = F)
