#for the 2 firsts do also slash by population and general

library(readr)
library(dplyr)
library(jsonlite)
final <- read.csv("final.csv")

#Biggest 5 producers of renewable energy
biggestRenewableEnergy <-
  final %>% 
  select(country_name, year, energy_value) %>%
  filter(year <= 2015) %>%
  group_by(year) %>%
  arrange(energy_value)  %>%
  top_n(5) %>%
  mutate(all = sum(as.numeric(energy_value))) %>%
  ungroup() %>%
  arrange(desc(year, energy_value))

write_json(biggestRenewableEnergy, "biggestRenewableEnergy.json")
#Biggest 5 PIB

biggestPIB <-
  final %>% 
  select(country_name, year, PIB) %>%
  group_by(year) %>%
  arrange(PIB)  %>%
  top_n(5) %>%
  mutate(all = sum(as.numeric(PIB))) %>%
  ungroup() %>%
  arrange(desc(year, PIB))

write_json(biggestPIB, "biggestPIB.json")


#Biggest 5 producers of earth footprint



biggestEarthFootprint <-
  final %>% 
  select(country_name, year, earth_footprint) %>%
  group_by(year) %>%
  arrange(earth_footprint)  %>%
  top_n(5) %>%
  mutate(all = sum(as.numeric(earth_footprint))) %>%
  ungroup() %>%
  arrange(desc(year, earth_footprint))

write_json(biggestEarthFootprint, "biggestEarthFootprint.json")
#Biggest 5 IDH


biggestIHD <-
  final %>% 
  select(country_name, year, IHD) %>%
  group_by(year) %>%
  arrange(IHD)  %>%
  top_n(5) %>%
  mutate(all = sum(as.numeric(IHD))) %>%
  ungroup() %>%
  arrange(desc(year, IHD))

write_json(biggestIHD, "biggestIHD.json")
