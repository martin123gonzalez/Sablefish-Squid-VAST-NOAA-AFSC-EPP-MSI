# Purpose: This code analyzes  ceph data from AK bottom trawl data from clean_ceph_data
# Author: Martin A Gonzalez
# Date Created: 6/15/2022
# Date Updated: 6/21/2022


# goals and comments ------------------------------------------------------

# Resources for analysis: 
#   R4ds chap 5, chap 5.5 mutate() and 5.6 summarize()
# group by location and species name to see how many species in that location. 
# then see that by year to see yearly showings

# Goals: 
  # look at different groupings of data and getting total counts
  # ex:
      # group by species id, region, year, get total count for number_fish
      # for year, create new column and use first 4 digits of cruise number to 
      # get year (throuh stringr)

# notes:
  #  change up average_sst_per_year_region to refelct average sst for each year 
  # and region then compare that to total catch numbers, regardless of species

# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(stringr)
library(dplyr)

# get_data ----------------------------------------------------------------

ceph_data_clean <- read_csv(here::here("output_data", "clean_ceph_data.csv"))

# ceph_data_year_region_unite <- ceph_data_clean %>% 
#   tidyr::unite(col = "year_region",
#                c(year, region),
#                sep = "-")


# functions ---------------------------------------------------------------

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# analyze_data ------------------------------------------------------------

#  CHECK 1: count_fish_per_year_region =====================================
  # groups by species id, region, year for total fish counts, arranged by year 

count_fish_per_year_region <- ceph_data_clean %>% 
  group_by(year, region, report_name_scientific) %>% 
  dplyr::summarize(count_per_year_region = sum(number_fish, na.rm = TRUE)) %>% 
  arrange(year)

#  CHECK 2: count_fish_region ==============================================
  # counts species caught over all time, per region, arranged by desc count 

count_fish_region <- ceph_data_clean %>% 
  group_by(region, report_name_scientific) %>% 
  dplyr::summarize(count_per_region = sum(number_fish, na.rm = TRUE)) %>% 
  # filter(count_per_region >100) %>% 
  # arranged by descending count per region
  arrange(desc(count_per_region))

# CHECK 3: count_fish_year ================================================
  # counts species caught over all regions, per year, arranged by desc count
count_fish_year <- ceph_data_clean %>% 
  group_by(year, report_name_scientific) %>% 
  dplyr::summarize(tot_num_fish = sum(number_fish, na.rm = TRUE)) %>% 
  arrange(year)


# 4: count_unique_spec ====================================================
  # count number of unique species over all time, per region 
count_unique_spec <- distinct(ceph_data_clean, report_name_scientific, region) %>% 
  dplyr::count(region, name = "count_of_unique_species") 

# 5.1: berry_through_time_region =========================================
  # getting only Berryteuthis magister to see changes throughout time (all regions)
  # can be used as template for this type 

berry_through_time_region <- count_fish_per_year_region %>% 
  tidyr::unite(col = "year_region",
               c(year, region),
               sep = "-") %>% 
  group_by(year_region, berry_per_year_all_region 
           = count_per_year_region) %>% 
  dplyr::filter(report_name_scientific == "Berryteuthis magister") %>% 
  dplyr::summarize() %>% 
  arrange(year_region)

# 5.2: dory_through_time_region =========================================
  # getting only Doryteuthis opalescens to see changes throughout time (all regions)

dory_through_time_region <- count_fish_per_year_region %>% 
  tidyr::unite(col = "year_region",
               c(year, region),
               sep = "-") %>% 
  group_by(year_region, dory_per_year_all_region 
           = count_per_year_region) %>% 
  dplyr::filter(report_name_scientific == "Loligo (=Doryteuthis) opalescens") %>% 
  dplyr::summarize() %>% 
  arrange(year_region)

# CHECK 6.1: summary_sst_region =============================================== 
  # mean, median, std, max, min, range, mode of sst per region

summary_sst_region <- ceph_data_clean %>% 
  dplyr::filter(na.rm = TRUE) %>% 
  group_by(region, report_name_scientific) %>% 
  dplyr::summarize(count = sum(number_fish, na.rm = TRUE), 
                   mean = mean(surface_temperature, na.rm = TRUE), 
                   median = median(surface_temperature, na.rm = TRUE),
                   standard_deviation = sd(surface_temperature, na.rm = TRUE),
                   max_temp = max(surface_temperature, na.rm = TRUE),
                   min_temp = min(surface_temperature, na.rm = TRUE),
                   range = max(surface_temperature, na.rm = TRUE) 
                   - min(surface_temperature, na.rm = TRUE),
                   mode = getmode(surface_temperature)) %>% 
  arrange(mean)

# CHECK 6.2: summary_sst_year ===============================================
  # mean, median, std, max, min, range, mode of sst per year

summary_sst_year <- ceph_data_clean %>% 
  dplyr::filter(na.rm = TRUE) %>% 
  group_by(year, report_name_scientific) %>% 
  dplyr::summarize(count = sum(number_fish, na.rm = TRUE),
            mean = mean(surface_temperature, na.rm = TRUE), 
            median = median(surface_temperature, na.rm = TRUE),
            standard_deviation = sd(surface_temperature, na.rm = TRUE),
            max_temp = max(surface_temperature, na.rm = TRUE),
            min_temp = min(surface_temperature, na.rm = TRUE),
            range = max(surface_temperature, na.rm = TRUE) 
            - min(surface_temperature, na.rm = TRUE),
            mode = getmode(surface_temperature)) %>% 
  arrange(mean)

# CHECK 7: tot_ceph_year =================================================== 
  # abundance of all fish during each year (all regions)

tot_ceph_year <- ceph_data_clean %>% 
  group_by(year) %>%
  dplyr::summarize(count_all_fish_per_year = sum(number_fish, na.rm = TRUE))

# CHECK 8.1: tot_ceph_region =================================================
  # abundance of all fish in each region (all years)

tot_ceph_region <- ceph_data_clean %>% 
  group_by(region) %>% 
  dplyr::summarize(count_all_fish_per_region = sum(number_fish, na.rm = TRUE))

# CHECK 8.2 tot_ceph_region_without_berry =========================

tot_ceph_region_without_berry <- ceph_data_clean %>% 
  group_by(region) %>% 
  dplyr::filter(report_name_scientific != "Berryteuthis magister") %>% 
  dplyr::summarize(count_all_fish_per_region = sum(number_fish, na.rm = TRUE)) %>% 
  dplyr::mutate(temp = "w/o Berryteuthis magister") %>% 
  tidyr::unite(col = "region",
                c(region, temp),
                sep = "- ")

# CHECK 9: summary_bot_depth_region ================================
  # bottom depth with year, region, and catch to see mean, median, mode, std, 
  # for each species' catch

summary_bot_depth_region <- ceph_data_clean %>% 
  group_by(region, report_name_scientific) %>% 
  dplyr::summarize(count = sum(number_fish, na.rm = TRUE),
            mean = mean(bottom_depth, na.rm = TRUE), 
            median = median(bottom_depth, na.rm = TRUE),
            standard_deviation = sd(bottom_depth, na.rm = TRUE),
            max_depth = max(bottom_depth, na.rm = TRUE),
            min_depth = min(bottom_depth, na.rm = TRUE),
            range = max(bottom_depth, na.rm = TRUE) 
            - min(bottom_depth, na.rm = TRUE),
            mode = getmode(bottom_depth)) %>%  
  arrange(desc(mean))

# 10: tot_ceph_spec ===================================================
  # counts total of each species recorded (all year, all region)

tot_ceph_spec <- count_fish_year %>% 
  group_by(report_name_scientific) %>% 
  dplyr::summarize(total_per_species = sum(tot_num_fish)) %>% 
  arrange(desc(total_per_species))

# 11.1: lat_long_and_depth =================================================
  # mean lat/long per species per mean depth
lat_long_and_depth <- ceph_data_clean %>% 
  group_by(report_name_scientific) %>% 
  dplyr::summarize(count = sum(number_fish, na.rm = TRUE),
            mean_bottom_depth = mean(bottom_depth),
            mean_latitude = mean(start_latitude),
            mean_longitude = mean(start_longitude)) %>% 
  dplyr::filter(count >= 30) %>% 
  arrange(desc(count))

# DEFUNCT? 11.2: BS_lat_long_depth_count ========================================
  # lat_long_and_depth of catches (only Bering Sea) through time
  # CPUE_KGHA = count here in this area

BS_lat_long_depth_count <- ceph_data_clean %>% 
  dplyr::select(year, region, report_name_scientific, start_latitude, 
                start_longitude, bottom_depth) %>% 
  group_by(year) %>% 
  dplyr::filter(region == "BS") %>% 
  dplyr::mutate(LATITUDE = start_latitude,
                LONGITUDE = start_longitude) %>% 
  arrange(year)

# 11.3: all_lat_long_depth_count ======================================
  # lat_long_and_depth of catches (all regions) through time
  
all_lat_long_depth_count <- ceph_data_clean %>% 
  dplyr::select(year, region, report_name_scientific, start_latitude, 
                start_longitude, bottom_depth) %>% 
  group_by(year) %>% 
  dplyr::mutate(LATITUDE = start_latitude,
                LONGITUDE = start_longitude) %>% 
  arrange(year)
  

# CHECK 12.1: summary_bot_temp_region =======================================
  # mean, median, std, max, min, range, mode of gear (bottom) temp per year / species

summary_bot_temp_region <- ceph_data_clean %>% 
  dplyr::filter(na.rm = TRUE) %>% 
  group_by(region, report_name_scientific) %>% 
  dplyr::summarize(count = sum(number_fish, na.rm = TRUE),
            mean = mean(gear_temperature, na.rm = TRUE), 
            median = median(gear_temperature, na.rm = TRUE),
            standard_deviation = sd(gear_temperature, na.rm = TRUE),
            max_temp = max(gear_temperature, na.rm = TRUE),
            min_temp = min(gear_temperature, na.rm = TRUE),
            range = max(gear_temperature, na.rm = TRUE) 
            - min(gear_temperature, na.rm = TRUE),
            mode = getmode(gear_temperature)) %>% 
  arrange(mean)

# CHECK 12.2: summary_bot_temp_year =========================================
  # mean, median, std, max, min, range, mode of gear (bottom) temp per region / species

summary_bot_temp_year <- ceph_data_clean %>% 
  dplyr::filter(na.rm = TRUE) %>% 
  group_by(year, report_name_scientific) %>% 
  dplyr::summarize(count = sum(number_fish, na.rm = TRUE),
            mean = mean(gear_temperature, na.rm = TRUE), 
            median = median(gear_temperature, na.rm = TRUE),
            standard_deviation = sd(gear_temperature, na.rm = TRUE),
            max_temp = max(gear_temperature, na.rm = TRUE),
            min_temp = min(gear_temperature, na.rm = TRUE),
            range = max(gear_temperature, na.rm = TRUE) 
            - min(gear_temperature, na.rm = TRUE),
            mode = getmode(gear_temperature)) %>% 
  arrange(mean)

# CHECK 13.1: tot_count_bot_and_surface_temp_region ========================
  # SST and gear temperature for species total count per region

tot_count_bot_and_surface_temp_region <- ceph_data_clean %>% 
  group_by(region, report_name_scientific) %>% 
  dplyr::summarize(count = sum(number_fish, na.rm = TRUE),
            mean_sea_surface_temperature = mean(surface_temperature, na.rm = TRUE),
            mean_bottom_temperature = mean(gear_temperature, na.rm = TRUE)) %>% 
  arrange(desc(count))

# CHECK 13.2: tot_count_bot_and_surface_temp_year ===========================
  # SST and gear temperature for species total count per year ???

tot_count_bot_and_surface_temp_year <- ceph_data_clean %>% 
  group_by(year, report_name_scientific) %>% 
  dplyr::summarize(count = sum(number_fish, na.rm = TRUE),
            mean_sea_surface_temperature = mean(surface_temperature, na.rm = TRUE),
            mean_bottom_temperature = mean(gear_temperature, na.rm = TRUE)) %>% 
  arrange(desc(count))

# 13.3: tot_count_bot_and_surface_temp_year_region =====================
  # combines 13.1 and 13.2 (kept them for notes)

tot_count_bot_and_surface_temp_year_region <- ceph_data_clean %>% 
  group_by(year, region, report_name_scientific) %>% 
  dplyr::summarize(count = sum(number_fish, na.rm = TRUE),
            mean_sea_surface_temperature = mean(surface_temperature, na.rm = TRUE),
            mean_bottom_temperature = mean(gear_temperature, na.rm = TRUE)) %>% 
  arrange(desc(count))

# 14: tot_ceph_year_region ===============
  # counts cephalopods per year region

tot_ceph_year_region <- ceph_data_clean %>% 
  group_by(year, region) %>% 
  dplyr::summarize(count = sum(number_fish, na.rm = TRUE))

# 15: tot_spec_all_time ================
  #  counts ceph all time per species

# tot_spec_all_time <- ceph_data_region 
  


# data_output -------------------------------------------------------------

write_csv(x = count_fish_per_year_region,
          file = here::here("output_data", "count_fish_per_year_region.csv"))
write_csv(x = count_fish_region,
          file = here::here("output_data", "count_fish_region.csv"))
write_csv(x = count_fish_year,
          file = here::here("output_data", "count_fish_year.csv"))
write_csv(x = count_unique_spec,
          file = here::here("output_data", "count_unique_species.csv"))
write_csv(x = berry_through_time_region,
          file = here::here("output_data", "berry_through_time_region.csv"))
write_csv(x = dory_through_time_region,
          file = here::here("output_data", "dory_through_time_region.csv"))
write_csv(x = tot_ceph_year,
          file = here::here("output_data", "tot_ceph_year.csv"))
write_csv(x = tot_ceph_region,
          file = here::here("output_data", "tot_ceph_region.csv"))
write_csv(x = summary_bot_depth_region,
          file = here::here("output_data", "summary_bot_depth_region.csv"))
write_csv(x = tot_ceph_spec,
          file = here::here("output_data", "tot_ceph_spec.csv"))
write_csv(x = summary_sst_region,
          file = here::here("output_data", "summary_sst_region.csv"))
write_csv(x = summary_sst_year,
          file = here::here("output_data", "summary_sst_year.csv"))
write_csv(x = lat_long_and_depth,
          file = here::here("output_data", "lat_long_and_depth.csv"))
write_csv(x = summary_bot_temp_region,
          file = here::here("output_data", "summary_bot_temp_region.csv"))
write_csv(x = summary_bot_temp_year,
          path = here::here("output_data", "summary_bot_temp_year.csv"))
write_csv(x = tot_count_bot_and_surface_temp_region,
          file = here::here("output_data", "tot_count_bot_and_surface_temp_region.csv"))
write_csv(x = tot_count_bot_and_surface_temp_year,
          file = here::here("output_data", "tot_count_bot_and_surface_temp_year.csv"))
write_csv(x = tot_count_bot_and_surface_temp_year_region,
          file = here::here("output_data", "tot_count_bot_and_surface_temp_year_region.csv"))
write_csv(x = BS_lat_long_depth_count,
          file = here::here("output_data", "BS_lat_long_depth_count.csv"))
write_csv(x = all_lat_long_depth_count,
          file = here::here("output_data", "all_lat_long_depth_count.csv"))
write_csv(x = tot_ceph_year_region,
          file = here::here("output_data", "tot_ceph_year_region.csv"))

