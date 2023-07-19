# Sablefish-Squid-VAST-NOAA-AFSC-EPP-MSI

#Data Source: 
    #https://www.fisheries.noaa.gov/foss/f?p=215:28
    # Raw (for ceph_scavenger_code)
        # raw squid data: b_mag_raw_data.csv
        # raw sablefish data: sablefish_raw_data.csv
        # raw tanner crab data: tanner_crab_raw_data.csv
    # For VAST (for VAST_Formater, vast_sable, vast_sable_squid_cov)
        # clean squid data: b_mag_clean.csv
        # clean sablefish data: sablefish_clean.csv
        # haul data: survey_hauls_good_ak.csv

#ceph_scavenger_code
    # coding file for squid spawning aggregation/scavenger feeding mapping projects. 
    # focal species: Magister Armhook Squid, Sablefish, Tanner Crabs
    
#VAST_Formatter
    # coding file to format B. magister and Sablefish haul data for input into VAST

#vast_sable
    # VAST program for the base sablefish spatio-temporal delta generalized linear mixed model
    # focal species: Sablefish

#vast_sable_squid_cov
    # VAST program for sablefish-squid spatio-temporal delta generalized linear mixed model where a squid density covariate is applied to see effect of squid density on sablefish density.
    # focal species: Sablefish, Magister Armhook Squid
