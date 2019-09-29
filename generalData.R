library(readr)
library(dplyr)
library(jsonlite)
final <- read.csv("database/final_data_set.csv")

# Continent with less earth footprint in the last decade
# Continent with biggest earth footprint in the last decade
continent_earth_footprint_decade <-
  final %>%
  select(continent_name, earth_footprint, year) %>%
  arrange(year) %>% 
  filter(year >= 2009 & year <= 2015) %>%
  mutate(earth_footprint = if_else(is.na(earth_footprint), 0, earth_footprint)) %>%
  group_by(continent_name) %>%
  summarize(n = sum(earth_footprint)) %>%
  arrange(n)

write_json(continent_earth_footprint_decade, "database/generalInfo/continent_earth_footprint_decade.json")
# percentage of countries that have some renewable energy production
continent_renewable_energy_production <-
  final %>%
  select(country_name, energy_value) %>%
  mutate(energy_value = if_else(is.na(energy_value), 0, energy_value)) %>%
  group_by(country_name) %>%
  mutate(sum_energy =  sum(energy_value))  %>%
  distinct() %>%
  ungroup() %>%
  mutate(biggerThanZero =  if_else(sum_energy == 0, FALSE, TRUE) )  %>%
  mutate(all = length(biggerThanZero))  %>%
  group_by(biggerThanZero) %>%
  summarise(percentage_of_countries = length(biggerThanZero)/head(all,1)*100) 
  

write_json(continent_renewable_energy_production, "database/generalInfo/continent_renewable_energy_production.json")


# percentage of countries that have at least one earth footprint in the last 10 years

country_name_earth_footprint_decade <-
  final %>%
  select(country_name, earth_footprint, year) %>%
  arrange(year) %>% 
  filter(year >= 2009 & year <= 2015) %>%
  select(-year) %>%
  mutate(earth_footprint = if_else(is.na(earth_footprint), 0, earth_footprint)) %>%
  group_by(country_name) %>%
  mutate(moreThanOne = if_else(earth_footprint > 1 , TRUE, FALSE)) %>%
  select(-earth_footprint) %>%
  distinct()%>%
  ungroup() %>%
  mutate(all = length(moreThanOne))  %>%
  group_by(moreThanOne) %>%
  summarise(percentage_of_countries = length(moreThanOne)/head(all,1)*100) 


write_json(country_name_earth_footprint_decade, "database/generalInfo/country_name_earth_footprint_decade.json")

