# Clean Titles
library(tidyverse)
library(ggplot2)

data = read.csv(file = 'alt_fuel_stations (Jul 31 2019).csv')

data = data %>% 
  mutate(
    Open.Date = as.Date(Open.Date, '%Y-%m-%d')
  )

#selecting only the columns we need
data2 = data %>% select(
  Fuel.Type.Code,
  Street.Address,
  City,
  State,
  ZIP,
  Country,
  Status.Code,
  Expected.Date,
  Access.Code,
  Owner.Type.Code,
  CNG.On.Site.Renewable.Source,
  LNG.On.Site.Renewable.Source,
  EV.On.Site.Renewable.Source,
  EV.Connector.Types,
  EV.Pricing,
  Latitude,
  Longitude,
  Open.Date,
  Date.Last.Confirmed,
  ID,
  Facility.Type
)

data3 = data2 %>%
  mutate(
    Fuel.Type.Code = ifelse(grepl('BD', Fuel.Type.Code), 'Biodiesel',
                     ifelse(grepl('CNG', Fuel.Type.Code), 'Condensed Natural Gas',
                     ifelse(grepl('E85', Fuel.Type.Code), 'Ethanol',
                     ifelse(grepl('ELEC', Fuel.Type.Code), 'Electric',
                     ifelse(grepl('LNG', Fuel.Type.Code), 'Liquefied Natural Gas',
                     ifelse(grepl('HY', Fuel.Type.Code), 'Hydrogen',
                     ifelse(grepl('LPG', Fuel.Type.Code), 'Propane', '')))))))
  )

data4 = data3 %>%
  filter(
    Latitude > 23 & Latitude < 50,
    Longitude > -127 & Longitude < -61
  )

data5 = data4 %>% 
  filter(Access.Code == 'public')

saveRDS(data4, "./data.rds")

#### Adding Cities ####

station_cities = data5 %>% 
  group_by(City, State) %>% 
  summarise(
    'Electric' = sum(Fuel.Type.Code == 'Electric'),
    'Ethanol' = sum(Fuel.Type.Code == 'Ethanol'),
    'Condensed Natural Gas' = sum(Fuel.Type.Code == 'Condensed Natural Gas'),
    'Liquefied Natural Gas' = sum(Fuel.Type.Code == 'Liquefied Natural Gas'),
    'Propane' = sum(Fuel.Type.Code == 'Propane'),
    'Biodiesel' = sum(Fuel.Type.Code == 'Biodiesel'),
    'Hydrogen' = sum(Fuel.Type.Code == 'Hydrogen'),
      )
names(station_cities)[1:2] = c('city', 'state_id')

cities_raw = read.csv('cities_lat_lng.csv')

cities = inner_join(station_cities, cities_raw, by = c('city', 'state_id'))

saveRDS(cities, "./cities.rds")

temp = cities %>% 
  ungroup() %>% 
  select(city, names(cities)[names(cities) %in% c('Electric', 'Hydrogen')], lat, lng, population, density)

names_use = names(temp)[names(temp) %in% c('Electric', 'Hydrogen')]

temp$number = rowSums(temp[,names_use])

