
# PART 1: MAIN ANALYSIS - 11 SITES WITH CLOSET STATIONS
# Choose the closet weather station for sites with multiple stations

library(geosphere)
library(readxl)
library(dplyr)

source("utility_fun.R")
source("config.R")

# Site lon/lat
site_addr <- file.path(climate_mh_path, "ABCD_11sites_multi_stations.xlsx") %>% read_excel(sheet = "multi stations")

# Get all stations
station_multi <- site_addr %>% filter(!is.na(station)) %>% pull(station)

# Get longitude and latitude of stations
data_station_multi <- download_normal_climate(stations = station_multi, mainURL = normal_climate_data_path)
station_coord <- data_station_multi %>% distinct(station, .keep_all = T) %>% select(station, latitude, longitude) %>% 
  setNames(c("station", "station_lat", "station_lon"))

# Merge data so that lon/lat of sites and stations are in the same data
site_station_coord <- site_addr %>% left_join(station_coord)

# Calculate distance from each site and station
site_station_coord$distance <- NA
for(i in 1:nrow(site_station_coord)) {
  site_station_coord$distance[i] <- distm(c(site_station_coord$station_lon[i], site_station_coord$station_lat[i]), 
                                          c(site_station_coord$site_lon[i], site_station_coord$site_lat[i]), fun = distHaversine)
}

# Choose the closet station for each site
site_station_coord_distance <- site_station_coord %>% 
  select(site, station, distance) %>% 
  group_by(site) %>%
  arrange(site, distance) %>% 
  filter(row_number() == 1)




# PART 2: Increase the bounding box of the website by d, select closet stations for all sites

# choose the closet station for each site
multi_stations <- file.path(climate_mh_path, "ABCD_21sites_coordinates_converted_max_distance_012023_ev.xlsx") %>% read_excel(sheet = "multi_stations")

# names of all stations
all_stations <- multi_stations %>% 
  select(contains("station")) %>% 
  unlist(., use.names = FALSE) %>% 
  na.omit() %>% 
  stringr::str_trim() %>% 
  unique()

# find the lon & lat of each station
data_all_stations <- download_normal_climate(stations = all_stations, mainURL = "https://www.ncei.noaa.gov/data/global-summary-of-the-month/access/")
# data_all_stations <- read.csv("data/data_all_stations_21_sites_020123.csv")
# write.csv(data_all_stations, "data/data_all_stations_21_sites_020123.csv")


coord_all_stations <- data_all_stations %>% 
  clean_names() %>% 
  select(station, station_lat = latitude, station_lon = longitude) %>% 
  distinct(station, .keep_all = T)

# merge with multi_stations
multi_stations_long <- multi_stations %>% 
  tidyr::pivot_longer(., cols = `station 1`:`station 32`, values_to = "station") %>% 
  left_join(coord_all_stations)

# calculate distance between each station to the site
multi_stations <- multi_stations_long %>% 
  left_join(site_addr %>% select(contains("site"))) %>% 
  filter(!is.na(station))

multi_stations$distance <- NA
for(i in 1:nrow(multi_stations)) {
  multi_stations$distance[i] <- distm(c(multi_stations$station_lon[i], multi_stations$station_lat[i]), 
                                 c(multi_stations$site_lon[i], multi_stations$site_lat[i]), fun = distHaversine)
}

# write.csv(multi_stations, "data/ABCD_21sites_all_stations_distances_020123.xlsx")

# choose the min distance for each site # final closet station
multi_stations_closet <- multi_stations %>% 
  select(site, station, distance) %>% 
  group_by(site) %>%
  arrange(site, distance) %>%
  filter(row_number() == 1)


# write.csv(multi_stations_closet, "data/ABCD_21sites_all_stations_closet_020123.csv")


# Get the data of 11 closet stations
## 11 closet stations
closet_stations_11 <- site_station_coord_distance %>% distinct(station) %>% pull() %>% na.omit()
## data of those 11 stations
data_monthly_11sites <- data_all_stations %>% 
  filter(station %in% closet_stations_11)

# Get the data for 21 closet stations
closet_stations_21 <- multi_stations_closet %>% distinct(station) %>% pull() %>% na.omit()
## data of those 21 stations
data_monthly_21sites <- data_all_stations %>% 
  filter(station %in% closet_stations_21)

write_csv(data_monthly_11sites, "data/data_monthly.csv")
write_csv(data_monthly_21sites, "data/data_monthly_21sites.csv")










