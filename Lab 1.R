library(tidyverse)
library(ggplot2)
library(sf)

p.counties <- "../data/CBW/County_Boundaries.shp"
p.stations <- "../data/CBW/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"

d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)

glimpse(d.counties)
glimpse(d.stations)

d.stations %>% sf::st_is_valid()
d.counties %>% sf::st_is_valid()

d.counties <- d.counties %>% sf::st_make_valid()

#select specific attributes(or remove them)
d.counties %>% dplyr::select(GEOID10, ALAND10) %>% head()
d.counties %>% dplyr::select(-NAME10) %>% head()

#select a range or range to remove
d.counties %>% dplyr::select(GEOID10:CLASSFP10) %>% head() 
d.counties %>% dplyr::select(-(GEOID10:CLASSFP10)) %>% head()

d.counties %>% dplyr::select(starts_with("C"))

#Grouping data
d.counties %>% group_by(STATEFP10) %>% mutate(stateLandArea = sum(ALAND10))

d.counties %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% # this line converts the data because of wonky geometry
  group_by(STATEFP10) %>% 
  summarise(stateLandArea = sum(ALAND10))

#Plotting
d.counties %>% 
  ggplot(., aes(x = as.factor(STATEFP10), y = ALAND10)) +
  geom_boxplot(aes(fill = STATEFP10))

d.counties %>% 
  ggplot(., aes(x = ALAND10)) +
  geom_histogram(aes(fill = STATEFP10)) +
  labs(title = "not the most useful plot, but you get the idea")

#make sure the data are on the same coordinate system
d.counties %>% sf::st_crs()
d.stations %>% sf::st_crs()
d.counties %>% sf::st_crs() == d.stations %>% sf::st_crs()

#select only counties in delaware
del.counties <- d.counties %>% dplyr::filter(STATEFP10 == 10)
#then select the stations that intersect with the delaware counties
del.stations <- sf::st_intersection(d.stations, del.counties)

glimpse(del.stations)
plot(del.stations)

#calculate area of all counties in delaware
del.counties %>% st_area()

#Task 1: Basic Data Manipulation
#1.1
LandArea <- d.counties %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% # this line converts the data because of wonky geometry
    group_by(STATEFP10) %>% 
    summarise(stateLandArea = sum(ALAND10 + AWATER10))

CountyArea <- d.counties %>%
  left_join(LandArea, by = "STATEFP10") %>%
  mutate(percent_land_area = (ALAND10 / stateLandArea) * 100)

CountyArea %>%
select(STATEFP10, NAME10, percent_land_area)

#1.2
d.counties <- d.counties %>%
  mutate(water_proportion = AWATER10 / (AWATER10 + ALAND10))

max_water_county <- d.counties %>%
  filter(water_proportion == max(water_proportion))

max_water_county %>% select(STATEFP10, NAME10, water_proportion)

#1.3
d.counties %>% group_by(STATEFP10) %>% mutate(stateLandArea = sum(ALAND10))

d.counties %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% # this line converts the data because of wonky geometry
  group_by(STATEFP10) %>% 
  summarise(number_counties = n())

#1.4
shortest_name <- d.stations %>%
  mutate(name_length = nchar(STATION_NA)) %>%  
  arrange(name_length)

shortest_name

##Task 2
#2.1
ggplot(data = d.counties,
      mapping = aes(x = ALAND10, y = AWATER10, color = STATEFP10))+
      geom_point()+
      labs(title = "Relationship Between Land and Water Area by State")+
      xlab("Land Area")+
      ylab("Water Area")

#2.2
ggplot(d.stations, aes(x = Drainage_A))+
    geom_histogram(binwidth = 2500)+
    labs(title = "Drainage Area by Monitoring Station")+
    xlab("Drainage Area")

#2.3
merged_data <- st_join(d.stations, d.counties, join = st_intersects)

ggplot(merged_data, aes(x = Drainage_A, fill = STATEFP10))+
  geom_histogram()+
  labs(title = "Drainage Area by Monitoring Station")+
  xlab("Drainage Area")

##Task 3
#functions to give mean, median, max, and min and sort

math <- function(x){
  if(is.numeric(x)){
  mean <- mean(x)
  median <- median(x)
  max <-  max(x)
  min <- min(x)
  sort <- sort(x)
  return(c(mean = mean, median = median, max = max, min = min, sort))
 } else{
  return("Uh oh stinky")
 }
}

#test it
math(c("a","b","c"))
  
#Task 4
#4.1
merged_data <- st_join(d.stations, d.counties, join = st_intersects)

stations_count_by_state <- merged_data %>%
  group_by(STATEFP10) %>% 
  summarise(num_stations = n())

stations_count_by_state

#4.2
d.counties %>%
  select(STATEFP10 = 36)

df[d.counties$COUNTYFP10 == 36]

NY <- d.counties %>% 
 filter(STATEFP10 == 36) %>%
  summarise(average_land_area = mean(ALAND10, na.rm = TRUE))
NY

#4.3
# Find the station with the largest drainage area
largest_drainage <- d.stations %>%
  arrange(desc(Drainage_A)) %>% 
  slice(1)
largest_drainage
