library("dplyr")
library("readODS")
library("readxl")
library(reshape)
library(RCurl)
library(XML)
library(jsonlite)
library(readr)
'%ni%' <- Negate('%in%')

renewable_energy <- as.data.frame(read.ods("database/Renewable_energy.ods"))
colnames(renewable_energy) <- c("country_code","measure","year","energy_value")

renewable_energy_filtered <- renewable_energy %>%
  filter(country_code %ni% c('OEU', 'WLD', 'G20', 'EU28', 'OECD', 'COG', 'TWN')) %>% 
  filter(measure == 'PC_PRYENRGSUPPLY') 


idh <- as.data.frame(read_ods("database/IDH.ods")) %>% 
  melt(id=c("country_name")) %>% 
  dplyr::rename(
    year = variable,
    IHD = value
  )


pib <-  as.data.frame(read_ods("database/PIB.ods")) %>% 
  melt(id=c("country_name", "country_code")) %>% 
  dplyr::rename(
    year = variable,
    PIB = value
  ) 

population_actif <-  as.data.frame(read_ods("database/POPULATION_ACTIVE.ods")) %>% 
  melt(id=c("country_name", "country_code")) %>% 
  dplyr::rename(
    year = variable,
    pop_active = value
  ) 


population_total <- as.data.frame(read_ods("database/POPULACAO_TOTAL.ods")) %>% 
  melt(id=c("country_name", "country_code")) %>% 
  dplyr::rename(
    year = variable,
    pop_total = value
  ) 

fromXML_to_dataSet <- function(file_name){
  doc <- xmlParse(paste0("database/", file_name, ".xml"))
  xmldf <- xmlToDataFrame(nodes = c(getNodeSet(doc, "//ArrayList/item/value")), stringsAsFactors = FALSE) %>% as_tibble()
  colnames(xmldf) <- c("earth_footprint")
  xmldf2 <- xmlToDataFrame(nodes = c(getNodeSet(doc, "//ArrayList/item/countryName")), stringsAsFactors = FALSE) %>% as_tibble()
  colnames(xmldf2) <- c("country_name")
  year_of_the_data <- data.frame("year" = file_name)
  earth_footprint_temp <- cbind(xmldf,xmldf2, year_of_the_data) 
}

earth_footprint <-lapply(c(1990:2016), fromXML_to_dataSet)  %>% bind_rows() 

country_codes_continents <- read_csv("database/continents.csv") %>%
  inner_join(read_csv("database/country_codes.csv") , by = "country_code")

earth_footprint_with_country_codes_continents <- 
  inner_join(earth_footprint, read_csv("database/country_codes.csv"), by = "country_name") %>%
  mutate(year = as.character(year)) 

pib_english_name <-
  pib %>% 
  dplyr::rename(french_country_name = country_name) %>%
  inner_join(read_csv("database/country_codes.csv"), by = "country_code") %>%
  select(-french_country_name)

idh_with_country_codes_continents <- 
  inner_join(idh, read_csv("database/country_codes.csv"), by = "country_name") %>%
  mutate(year = as.character(year)) 

final_data_set <- 
  pib_english_name %>% 
  inner_join(population_actif %>% select(-country_name), by = c("country_code", "year")) %>% 
  inner_join(population_total %>% select(-country_name), by = c("country_code", "year")) %>%
  left_join(earth_footprint_with_country_codes_continents %>% select(-country_name), by = c("country_code", "year")) %>%
  left_join(idh_with_country_codes_continents %>% select(-country_name), by = c("country_code", "year")) %>%
  mutate(pib_per_capta = PIB/pop_total) %>%
  left_join(renewable_energy_filtered, by = c("country_code", "year"))  

write_csv(final_data_set, "database/final_data_set.csv")
write_json(final_data_set, "database/final_data_set.json")

