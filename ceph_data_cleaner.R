# Purpose: This code cleans ceph data from AK bottom trawl data
# Author: Martin A Gonzalez
# Date Created: 6/14/2022
# Date Updated: 6/15/2022


# notes and comments ------------------------------------------------------

# Goals:
## filter for performance with performance >= 0
## filter for hault_type == 3
## select out the NA 's (and prolly the classification up to specific specific name)

# if doing just presence/ absence, can use raw data
# anything else, use clean data

# Resources for analysis: 
## R4ds chap 5, chap 5.5 mutate() and 5.6 summarize()
# group by location and species name to see how many species in that location. 
# then see that by year to see yearly showings

# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(stringr)
library(dplyr)

# get_data ----------------------------------------------------------------

ceph_data <- read_csv(here::here("data", "ceph_data_ak.csv"))
haul_data <- read_csv(here::here("data", "ceph_all_hauls_ak.csv"))
slope_octo_data <- read_csv(here::here("data", "octo_BS_slope_all_data.csv"))
slope_squid_data <- read_csv(here::here("data", "squid_BS_slope_all_data.csv"))

# clean_data --------------------------------------------------------------

# filter rows and select columns ceph_data_clean 

ceph_data_clean <- ceph_data %>% 
  dplyr::filter(performance >= 0) %>% 
  dplyr::filter(haul_type == 3) %>% 
  dplyr::select(-kingdom_taxon, -subkingdom_taxon, -phylum_taxon, -subphylum_taxon, 
         -superclass_taxon, -class_taxon, -subclass_taxon, -infraclass_taxon,
         -division_taxon, -subdivision_taxon, -superorder_taxon, -order_taxon, 
         -suborder_taxon, -infraorder_taxon, -superfamily_taxon, 
         -subfamily_taxon, -tribe_taxon, -subtribe_taxon, -genus_taxon, 
         -subgenus_taxon, -section_taxon, -subsection_taxon, -species_taxon,
         -subspecies_taxon) %>% 
  dplyr::select(-catchjoin, -subsample_code, -voucher, -auditjoin.x, -auditjoin.y, 
         -net_measured, -net_height, -end_latitude, -end_longitude,
         -gear_depth, -wire_length) %>% 
  dplyr::filter(region == "BS" | region == "GOA" | region == "AI") %>% 
  # makes year column and filters for years > 1982
  mutate(year = str_extract(cruise, "^\\d{4}")) %>% 
  mutate(year = as.numeric(year)) %>% 
  dplyr::filter(year > 1982)
  
# calculate cpue

#  1: use the haul data to generate a list of all stations 
#       surveyed during the cruise

station_data <- haul_data %>% 
  dplyr::filter(performance >= 0) %>% 
  dplyr::select(region, cruise, cruisejoin, hauljoin, distance_fished, 
                net_width, stratum, start_longitude, start_latitude,
                bottom_type) %>% 
  mutate(year = str_extract(cruise, "^\\d{4}")) %>% 
  mutate(year = as.numeric(year)) %>% 
  dplyr::filter(year > 1982) 

# 2: combine your catch data with the haul data so that you have
    # all stations filled with either 0 (for none of the species cuaght), 
    # or a number of species cuaght

all_spec <- ceph_data %>%
  # dplyr::filter(species_code == 79210) %>%
  full_join(station_data) %>%
  mutate(sample_counts = if_else(condition = !is.na(subsample),
                                true = subsample,
                                false = 0)) %>%
  dplyr::filter(!is.na(distance_fished), !is.na(net_width)) %>% 
  mutate(effort = distance_fished * net_width/10,
         CPUE_KGHA = sample_counts/effort) %>% 
  dplyr::filter(!is.na(CPUE_KGHA)) %>% 
  dplyr::select(-kingdom_taxon, -subkingdom_taxon, -phylum_taxon, -subphylum_taxon, 
                -superclass_taxon, -class_taxon, -subclass_taxon, -infraclass_taxon,
                -division_taxon, -subdivision_taxon, -superorder_taxon, -order_taxon, 
                -suborder_taxon, -infraorder_taxon, -superfamily_taxon,
                -subfamily_taxon, -tribe_taxon, -subtribe_taxon, -genus_taxon, 
                -subgenus_taxon, -section_taxon, -subsection_taxon, -species_taxon,
                -subspecies_taxon) %>% 
  mutate(LONGITUDE = start_longitude,
         LATITUDE = start_latitude) %>% 
  dplyr::select(-end_longitude, - end_latitude)

# 
# summary_cpue <- some_spec %>% 
#   group_by(cruise, region, stratum, report_name_scientific) %>% 
#   summarize(mean_cpue_stratum = mean(CPUE_KGHA, na.rm = TRUE))


# clean_haul_data

clean_haul_data <- haul_data %>% 
  dplyr::filter(performance >= 0) %>% 
  dplyr::select(region, cruise, cruisejoin, hauljoin, distance_fished, 
                net_width, stratum, start_longitude, start_latitude, 
                gear_temperature, surface_temperature) %>% 
  mutate(year = str_extract(cruise, "^\\d{4}")) %>% 
  mutate(year = as.numeric(year)) %>% 
  dplyr::filter(year > 1982)


## BS slope squid and octo cleaning and combinging data

# fixing column titles & removing "cephalopod egg" & "Decapodiform egg"

colnames(slope_octo_data) <- c('year', 'region', 'survey', 'survey_ID', 'cruise', 'haul', 
                               'stratum', 'station', 'vessel_name', 'vessel',
                               'date_time', 'latitude', 'longitude', 'species_code',
                               'common_name', 'report_name_scientific', 'taxon_confidence',
                               'cpue_kg_ha', 'cpue_kg_km2', 'cpue_kg_1000_km2', 
                               'cpue_num_ha', 'cpue_num_km2', 'cpue_num_1000_kg2', 
                               'weight', 'count', 'gear_temperature', 'surface_temperature',
                               'bottom_depth', 'distance_fished', 'net_width', 
                               'net_height', 'area_swept_ha', 'duration_hr')
clean_slope_octo_data <- slope_octo_data %>% 
  dplyr::select(-taxon_confidence, -cpue_kg_1000_km2, -cpue_num_km2, -cpue_num_1000_kg2,
         -net_height, -duration_hr, -date_time) %>% 
  mutate(region = "BS Slope") %>% 
  dplyr::filter(report_name_scientific != "cephalopod egg" & 
                report_name_scientific != "Decapodiform egg")

colnames(slope_squid_data) <- c('year', 'region', 'survey', 'survey_ID', 'cruise', 'haul', 
                                'stratum', 'station', 'vessel_name', 'vessel',
                                'date_time', 'latitude', 'longitude', 'species_code',
                                'common_name', 'report_name_scientific', 'taxon_confidence',
                                'cpue_kg_ha', 'cpue_kg_km2', 'cpue_kg_1000_km2', 
                                'cpue_num_ha', 'cpue_num_km2', 'cpue_num_1000_kg2', 
                                'weight', 'count', 'gear_temperature', 'surface_temperature',
                                'bottom_depth', 'distance_fished', 'net_width', 
                                'net_height', 'area_swept_ha', 'duration_hr')
clean_slope_squid_data <- slope_squid_data %>% 
  dplyr::select(-taxon_confidence, -cpue_kg_1000_km2, -cpue_num_km2, -cpue_num_1000_kg2,
                -net_height, -duration_hr, -date_time) %>% 
  mutate(region = "BS Slope") %>% 
  dplyr::filter(report_name_scientific != "cephalopod egg" & 
                report_name_scientific != "Decapodiform egg")

clean_slope_octo_squid <- clean_slope_octo_data %>% 
  full_join(clean_slope_squid_data)


# data_output -------------------------------------------------------------

write_csv(x = ceph_data_clean, 
          file = here::here("output_data", "clean_ceph_data.csv"))
write_csv(x = station_data,
          file = here::here("output_data", "station_data.csv"))
write_csv(x = all_spec,
          file = here::here("output_data", "all_spec.csv"))
write_csv(x = clean_haul_data,
          file = here::here("output_data", "clean_haul_data.csv"))
write_csv(x = clean_slope_octo_squid,
          file = here::here("output_data", "clean_slope_octo_squid.csv"))


