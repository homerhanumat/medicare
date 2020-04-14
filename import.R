## download ----

url1 <- "https://www.cms.gov/Research-Statistics-Data-and-Systems/"
url2 <- "Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/"
url3 <- "Downloads/Inpatient_Data_"
url_end <- "_CSV.zip"

years <- 2011:2017

library(tidyverse)

if(!dir.exists("data")) {
  dir.create("data")
}
getData <- function(year) {
  url <- str_c(url1, url2, url3, year, url_end)
  destfile <- str_c("data/charges", year, ".zip")
  download.file(
    url = url,
    destfile = destfile
  )
  unzip(destfile, exdir = "data")
}

years %>% 
  purrr::map(getData)

## read in ----

charges_2011 <-
  read_csv("data/Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv", 
                         col_types = cols(`Provider Id` = col_character(), 
                                          `Provider Zip Code` = col_character())) %>%
  mutate(year = 2011)
charges_2012 <-
  read_csv("data/Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv", 
                         col_types = cols(`Provider Id` = col_character(), 
                                          `Provider Zip Code` = col_character())) %>% 
  mutate(year = 2012)
charges_2013 <-
  read_csv("data/Medicare_Provider_Charge_Inpatient_DRG100_FY2013.csv", 
                         col_types = cols(`Provider Zip Code` = col_character())) %>% 
  mutate(year = 2013)
charges_2014 <- 
  read_csv("data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv") %>% 
  mutate(year = 2014)
charges_2015 <- 
  read_csv("data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2015.csv", 
                         col_types = cols(`Provider Id` = col_character(), 
                                          `Provider Zip Code` = col_character())) %>% 
  mutate(year = 2015)

## 2016 data is bad:  numbers have commas; charges and payments
## also lead with a dollar sign

charges_2016 <- read_csv("data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2016.csv", 
                         col_types = cols(`Total Discharges` = col_character())) %>% 
  mutate(year = 2016) %>% 
  mutate(`Total Discharges` = as.numeric(
    str_replace_all(`Total Discharges`, ",", "")
  )) %>% 
  mutate(`Average Covered Charges` = as.numeric(
    str_replace_all(`Average Covered Charges`, "[,$]", "")
  )) %>% 
  mutate(`Average Total Payments` = as.numeric(
    str_replace_all(`Average Total Payments`, "[,$]", "")
  )) %>% 
  mutate(`Average Medicare Payments` = as.numeric(
    str_replace_all(`Average Medicare Payments`, "[,$]", "")
  ))

charges_2017 <- 
  read_csv("data/MEDICARE_PROVIDER_CHARGE_INPATIENT_DRGALL_FY2017.CSV", 
                         col_types = cols(`Provider Id` = col_character(), 
                                          `Provider Zip Code` = col_character())) %>% 
  mutate(year = 2017)

charges <-
  rbind(charges_2011, charges_2012, charges_2013,
        charges_2014, charges_2015, charges_2016,
        charges_2017)

rm(list = str_c("charges_", years))

names(charges) <-
  c(
    "drgdef",
    "idProvider",
    "nameProvider",
    "addressProvider",
    "cityProvider",
    "stateProvider",
    "zipProvider",
    "referralRegion",
    "totalDischarges",
    "aveCharges",
    "aveTotalPayments",
    "avePayments",
    "year"
  )

## split into drg and definition:
charges <-
  charges %>% 
  tidyr::extract(
    col = drgdef,
    into = c("drg", "drgDefinition"),
    regex = "(\\d+) - (.+)"
  )

## further munging ----

## find drgs that are present in all years
drg_common <-
  sort(unique(charges$year)) %>% 
  purrr::map(.f = function(x) {
    charges %>% 
      filter(year == x) %>% 
      pull(drg)
  }) %>% 
  purrr::reduce(intersect)

charges <-
  charges %>% 
  filter(drg %in% drg_common)

## DiagnosticRelatedGroups ----

DiagnosticRelatedGroups <-
  charges %>% 
  select(drg, drgDefinition) %>% 
  distinct()

## check for duplications

repeated <-
  DiagnosticRelatedGroups %>% 
  pull(drg) %>% 
  duplicated()

dupdrgs <-
  DiagnosticRelatedGroups %>% 
  pull(drg) %>% 
  .[repeated]

DiagnosticRelatedGroups %>% 
  filter(drg %in% dupdrgs) %>% 
  arrange(drg)

## should be OK to simply remove the duplicated drgs
## at the end of the data set:
DiagnosticRelatedGroups <-
  DiagnosticRelatedGroups %>% 
  filter(!repeated)


## MedicareCharges ----

MedicareCharges <- 
  charges %>% 
  select(drg, idProvider, year, totalDischarges,
         aveCharges, aveTotalPayments,
         avePayments)

## MedicareProviders ----

MedicareProviders <-
  charges %>% 
  select(idProvider,
         nameProvider,
         addressProvider,
         cityProvider,
         stateProvider,
         zipProvider,
         referralRegion,
         year)

## we'll just go with the descriptions
## from the first occurrence of the id number
## in the most recent year (i.e, most recent zip code)

MedicareProviders <-
  MedicareProviders %>% 
  group_by(idProvider) %>% 
  arrange(year) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(-year)

## check on zip codes
unique(nchar(MedicareProviders$zipProvider))

## hmm, some zips are only four character, what gives?

MedicareProviders %>% 
  filter(str_count(zipProvider) == 4) %>% 
  select(cityProvider, stateProvider, zipProvider) %>% 
  as.data.frame()

## oh yeah, these actually have a 0 in front, so:

MedicareProviders <-
  MedicareProviders %>% 
  mutate(zipProvider = str_pad(
    as.character(zipProvider), 
    pad = "0", side = "left", width = 5))

## try to get latitude and longitude from zip code
url_zips <-
  str_c(
    "https://public.opendatasoft.com/explore/dataset/",
    "us-zip-code-latitude-and-longitude/download/", 
    "?format=csv&timezone=America/New_York&use_labels_for_header=true"
  )
zip_loc <- read_delim(url_zips, 
                      ";", escape_double = FALSE, trim_ws = TRUE)
zip_loc2 <-
  zip_loc %>% 
  select(Zip, Latitude, Longitude)

stuff <-
  MedicareProviders %>% 
  left_join(zip_loc2, by = c("zipProvider" = "Zip"))

## there are 28 providers wihout a location
## let's just remove them

MedicareProviders <-
  MedicareProviders %>% 
  inner_join(zip_loc2, by = c("zipProvider" = "Zip"))

library(sf)
#library(maps)

# world <- ne_countries(scale = "medium", returnclass = "sf")
counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))

res <- st_as_sf(
  MedicareProviders, coords = c("Longitude", "Latitude"), 
  crs = 4326, agr = "constant")

MedicareProviders <- 
  st_join(res, counties, join = st_within)

## state names
## (we only want lower 48)
states_names <-
  read_csv("data/state_name.csv") %>%
  mutate(name = str_to_lower(name)) %>%
  filter(!(abbr %in% c(
    "AS","FM", "GU", "MH", "MP", "PR", "PW", "VI", "DC", "AK", "HI"
  )))


## Get map data
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(maps)
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))


## save the data ----

save(DiagnosticRelatedGroups, file = "data/DiagnosticRelatedGroups.Rda")
save(MedicareCharges, file = "data/MedicareCharges.Rda")
save(MedicareProviders, file = "data/MedicareProviders.Rda")
save(states_names, file = "data/states_names.Rda")
save(world, file = "data/world.Rda")
save(states, file = "data/states.Rda")
save(counties, file = "data/counties.Rda")
