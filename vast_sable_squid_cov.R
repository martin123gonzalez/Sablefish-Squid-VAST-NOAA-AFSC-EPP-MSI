# This code produces VAST index estimates 
# By: MG/CA
# Contact: caitlin.allen_akselrud@noaa.gov
# VAST Sable with squid COV

# # package installs --------------------------------------------------------
# 
# 2022: Rv4.0.2 or later VAST v3.8.2, FishStatsUtils v2.10.0, cpp VAST_v13_1_0, TMB v1.7.22, Matrix v1.4-0, DHARMa 0.4.5
# ## check to ensure you're running the correct package versions
packageVersion('FishStatsUtils')
packageVersion('VAST')
packageVersion('INLA')
packageVersion('TMB')
packageVersion('TMBhelper')

# ## how to check your session info (see R version and all package versions)
sessionInfo()
# # R version 4.0.2 (2020-06-22)

# install vast ------------------------------------------------------------

# # install.packages("TMB")
# library(TMB)
# # install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# # install.packages("INLA")
# library(INLA)
# av <- available.packages(filters=list())
# av[av[, "Package"] == 'INLA', ]
# 
# # devtools::install_github("james-thorson/FishStatsUtils", INSTALL_opts="--no-staged-install")
# library(FishStatsUtils)
# # install.packages('TMB', type = 'source')
# # devtools::install_github("james-thorson/VAST", INSTALL_opts="--no-staged-install")
# library(VAST)

# libraries ---------------------------------------------------------------

library(here) 
library(tidyverse)
library(sf)
library(scales)
library(renv)
library(lubridate)
library(janitor)
library(VAST)

# Set R environment for consistency ---------------------------------------

# renv::init()
# renv::snapshot()
# renv::restore()

# Set species -------------------------------------------------------------

# species <- 
run_version <-'sable-squid' # 'sable_only' 'sable-squid'
this_year <- lubridate::year(lubridate::today())

speciesName <- paste0("Sablefish_Squid",this_year,"_", run_version)
# max_cells <- Inf            # for Inf runs, need to use Virtual Machine (not enough memory on my laptop)
max_cells <- 5000
# max_cells <- 10000

workDir <- here::here("VAST_results", speciesName,max_cells)
dir.create(workDir, showWarnings = FALSE)

# table(Data_Geostat_sable$Year)
# table(Data_Geostat_squid$Year)

# Read in data ------------------------------------------------------------
# Data_Geostat_squid <- read_csv(here::here("R", "export_to_CA_CO", "GOOD_VAST_b_mag.csv"))          #streamlined here() code for CA_CO
# Data_Geostat_sable <- read_csv(here::here("R", "export_to_CA_CO", "GOOD_VAST_sablefish.csv"))      #streamlined here() code for CA_CO
# Data_Geostat_sable <- read_csv(here::here("R", "epp_ceph_alaska", "output_data", "nonenc_included_VAST_sablefish.csv"))       #nonencounter years included
Data_Geostat_squid <- read_csv(here::here("R", "epp_ceph_alaska", "output_data", "GOOD_VAST_b_mag_BS.csv"))            #normal
Data_Geostat_sable <- read_csv(here::here("R", "epp_ceph_alaska", "output_data", "GOOD_VAST_sablefish_BS.csv"))        #normal
# Data_Geostat_sable <- read_csv(here::here("R", "epp_ceph_alaska", "output_data", "GOOD_VAST_sablefish_BS_2002_2016.csv"))  #small time range,
# Data_Geostat_squid <- read_csv(here::here("R", "epp_ceph_alaska", "output_data", "GOOD_VAST_b_mag_BS_2002_2016.csv"))      #small time range
# Data_Geostat_squid <- read_csv(here::here("R", "epp_ceph_alaska", "output_data", "VAST_b_mag_BS2002_2016BSS.csv"))          #small time range with large catch
# Data_Geostat_sable <- read_csv(here::here("R", "epp_ceph_alaska", "output_data", "VAST_sablefish_BS2002_2016select.csv"))  #small time range, only with overlap
# Data_Geostat_squid <- read_csv(here::here("R", "epp_ceph_alaska", "VAST_ready_data", "Data_Geostat_squid.csv"))
# Data_Geostat_sable <- read_csv(here::here("R", "epp_ceph_alaska", "VAST_ready_data", "Data_Geostat_sable.csv"))
# Format catch data -------------------------------------------------------

Data_Geostat_squid <- Data_Geostat_squid %>% 
  as.data.frame()
Data_Geostat_sable <- Data_Geostat_sable %>% 
  as.data.frame()

# head(Data_Geostat)

table(Data_Geostat_sable$Year)
table(Data_Geostat_squid$Year)

# VAST Settings -----------------------------------------------------------
# Version <- get_latest_version( package="VAST" )
Version <- "VAST_v14_0_1" #2022
# Version <- "VAST_v12_0_0" #2021
#Region <- "User"    
Region <- "eastern_bering_sea" 
# user_region <- readRDS(file = here::here("R", "export_to_CA_CO", "user_region.rds"))            #export_to_CA_CO version
# user_region <- readRDS(file = here::here("R", "epp_ceph_alaska", "data", "user_region.rds"))      #normal version
# user_region <- "eastern_bering_sea" 

strata_names <- "AL_water"
Method <- "Mesh"
knot_method <- "grid"
grid_size_km <- 25
n_x <- 250   # Specify number of stations (a.k.a. "knots") ##CIA: test on 100, final run on 250-500, max 750

# Spatial Run
# FieldConfig <- c("Omega1"="IID", "Epsilon1"= 0, "Omega2"="IID", "Epsilon2"= 0)
# RhoConfig <- c("Beta1"=4, "Beta2"=4, "Epsilon1"=0, "Epsilon2"=0)

FieldConfig <- c("Omega1"="IID", "Epsilon1"= "IID", "Omega2"="IID", "Epsilon2"= "IID")  # correlation across space Omega = spatial effects, Epsilon = spatiotemporal
RhoConfig <- c("Beta1"=0, "Beta2"=0, "Epsilon1"=4, "Epsilon2"=4)#correlation across time
OverdispersionConfig <- c("Eta1"=0, "Eta2"=0)
ObsModel <- c(2,4) #Need (2,4) if there are some years with 100% encounter rate
Options <-  c("Calculate_Range"=FALSE, "Calculate_effective_area"=TRUE, "treat_nonencounter_as_zero"= FALSE) #TRUE )
Aniso <- FALSE
BiasCorr <- TRUE
getJointPrecision <- TRUE
getReportCovariance <- TRUE
fine_scale <- TRUE

strata.limits <- data.frame(STRATA = as.factor('All_areas'))

settings <- make_settings( 
  n_x=n_x,
  Region=Region,
  purpose="index2",
  fine_scale = fine_scale,
  strata.limits=strata.limits,
  ObsModel = ObsModel,
  FieldConfig = FieldConfig,
  RhoConfig = RhoConfig,
  OverdispersionConfig = OverdispersionConfig,
  Options = Options,
  use_anisotropy = Aniso,
  Version = Version,
  max_cells = max_cells,
  bias.correct = FALSE, #turn off for full run
  knot_method = knot_method
)


covariate_data <- Data_Geostat_squid 
# covariate formula
formula <- ~ Catch_KG_km2
Xconfig_zcp <- array(2, dim=c(2,1,1) )
X1config_cp <- as.matrix(2)
X2config_cp <- as.matrix(2)

      #original covariate inputs
      # Xconfig_zcp <- array(1, dim=c(2,1,1) )
      # X1config_cp <- as.matrix(1)
      # X2config_cp <- as.matrix(1)

    # code to match stuff, not actaul math
    # all 3 Xconfig = 1 mean estimate ; change to Xconfig = 2 for centering and scaling biomass

# read saved image (for troubleshooting) ----------------------------------

# load('pollock_index.RData')

# Build the model ---------------------------------------------------------
# quick fit:
# 
# settings_quick <- settings
# settings_quick$n_x <- 100
# settings_quick$bias.correct <- FALSE
# 
# options(max.print = .Machine$integer.max)
# 
# fit <- fit_model( "settings"=settings_quick, 
#                   "Lat_i"=Data_Geostat_sable[,'Lat'], 
#                   "Lon_i"=Data_Geostat_sable[,'Lon'], 
#                   "t_i"=Data_Geostat_sable[,'Year'], 
#                   "c_i"=rep(0,nrow(Data_Geostat_sable)),
#                   "b_i"=as_units(Data_Geostat_sable[,'Catch_KG_km2'], 'kg'),
#                   "a_i"=as_units(Data_Geostat_sable[,'AreaSwept_km2'], 'km^2'),
#                   "v_i"=Data_Geostat_sable[,'Vessel'],
#                   "create_strata_per_region"=TRUE,
#                   "input_grid" = user_region, 
#                   #"getJointPrecision"=getJointPrecision, # turn on for full run
#                   #"getReportCovariance"=getReportCovariance,  # turn on for full run
#                   "X1_formula"=formula,
#                   "X2_formula"=formula,
#                   "X1config_cp" = X1config_cp,
#                   "X2config_cp" = X2config_cp ,
#                   "covariate_data"= covariate_data,
#                   # getsd=TRUE,
#                   getsd=FALSE,            #for testing
#                   # "run_model" = FALSE,    #for testing
#                   # "build_model" = FALSE,  #for testing
#                   # test_fit = FALSE,       #for testing
#                   newtonsteps=0,          #for testing
#                   # CheckForBugs = FALSE,   #for testing
#                   "working_dir" = workDir
# )
# 
# saveRDS(fit, file = paste0(workDir,"/VASTfit.RDS"))
# fit_check <- fit
# fit_check <- readRDS(file = paste0(workDir,"/VASTfit.RDS"))
# fit_check$ParHat

# full model fit:
#fit_check <- readRDS(file = paste0(workDir,"/VASTfit.RDS"))
start.time <- Sys.time() #"2022-09-16 13:10:37 PDT"
start.time

full_fit <- suppressWarnings(fit_model( "settings"=settings, 
                       "Lat_i"=Data_Geostat_sable[,'Lat'], 
                       "Lon_i"=Data_Geostat_sable[,'Lon'], 
                       "t_i"=Data_Geostat_sable[,'Year'], 
                       "c_i"=rep(0,nrow(Data_Geostat_sable)),
                       "b_i"=as_units(Data_Geostat_sable[,'Catch_KG_km2'], 'kg'),
                       "a_i"=as_units(Data_Geostat_sable[,'AreaSwept_km2'], 'km^2'), 
                       "v_i"=Data_Geostat_sable[,'Vessel'],
                       #parameters=fit_check$ParHat,   #use params from quick run as starting point
                       "create_strata_per_region"=TRUE,
                       "getJointPrecision" = getJointPrecision, # turn on for full run
                       "getReportCovariance" = getReportCovariance,  # turn on for full run
                       #"input_grid" = user_region, 
                       "X1_formula"=formula,
                       "X2_formula"=formula,
                       "X1config_cp" = X1config_cp,
                       "X2config_cp" = X2config_cp,
                       "covariate_data"= covariate_data,
                       getsd = TRUE,
                       REML = FALSE,
                       # getsd=FALSE,            #for testing
                       # "run_model" = FALSE,    #for testing -- if an issue, try uncommenting this one
                       # "build_model" = FALSE,  #for testing
                       # test_fit = FALSE,       #for testing
                       # newtonsteps=0,          #for testing
                       # CheckForBugs = FALSE,   #for testing
                       "working_dir" = workDir,
                       "optimize_args" =list("lower"=-Inf,"upper"=Inf),
                       newtonsteps = 2
                       
))

# 2022: setting questions- "input_grid", "refine", optimize_args
stop.time <- Sys.time()
stop.time

# Save results

saveRDS(full_fit, file = paste0(workDir,"/VASTfit_full_",max_cells,".RDS"))
# full_fit <- readRDS(file = paste0(workDir,"/VASTfit_full.RDS"))

fit <- full_fit



# Plots -------------------------------------------------------------------
# 
plot( full_fit)
# 
# plot( full_fit, zrange = c(-3,3), n_cells = 600, strata_names = strata_names )
# 
# 
# ## Save COG (center of gravity) for ESP request
# results = plot( full_fit, n_cells=200^2 )
# write.csv( results$Range$COG_Table, file=paste0(workDir, "/COG.csv"), row.names=FALSE )
# 
# ##save effective area occupied for ESP request
# report = TMB::summary.sdreport(full_fit$parameter_estimates$SD)
# ln_km2 = report[which(rownames(report)=="log_effective_area_ctl"),c('Estimate','Std. Error')]
# Year <- sort(unique(full_fit$year_labels))
# ln_km2 <- as.data.frame(cbind(ln_km2, Year))
# ln_km2 <- ln_km2[which(ln_km2$Year %in% unique(fit$data_frame$t_i)),]
# write.csv( ln_km2, file=paste0(workDir,"/ln_effective_area.csv"), row.names=FALSE )

# v-cov matrix ------------------------------------------------------------
# If you need to load a fit in a new session:
# dyn.load(dynlib("VAST_v13_1_0"))
# full_fit <- readRDS(paste0(workDir,"/VASTfit_full.RDS"))
# 
# options(max.print = .Machine$integer.max)
# # 
# colnames(full_fit$extrapolation_list$a_el) = c("EBS")
# 
# #Naming
Names = paste( rep(colnames(full_fit$extrapolation_list$a_el),each=length(full_fit$year_labels)),
               rep(full_fit$year_labels,ncol(full_fit$extrapolation_list$a_el)), sep="_" )
# 
# Cov for the log-index
Which = which(names(full_fit$parameter_estimates$SD$value)=="ln_Index_ctl")
Cov_ln_Index = full_fit$parameter_estimates$SD$cov[Which,Which]
colnames(Cov_ln_Index) = rownames(Cov_ln_Index) = Names
write.csv( Cov_ln_Index, file=paste0(workDir,"/Cov_ln_Index.csv") )

# Cov for the index
Which = which(names(full_fit$parameter_estimates$SD$value)=="Index_ctl")
Cov_Index = full_fit$parameter_estimates$SD$cov[Which,Which]
colnames(Cov_Index) = rownames(Cov_Index) = Names
write.csv( Cov_Index, file=paste0(workDir,"/Cov_Index.csv") )

# # Everything Cov pull
# full_fit <- VASTfit_full_5000
# Cov_Index_All = full_fit$parameter_estimates$SD$cov
# 
# # dim(Cov_ln_Index)
# # dim(Cov_Index)
# 
# v_cov <- full_fit$parameter_estimates$SD$cov %>% as.data.frame() #variance covariate
# write_csv(v_cov, file = paste0(workDir,"/index_v_cov.csv"))

# Plots 2022 ---------------------------------------------------------------
# 
# plot( full_fit, zrange = c(-3,3), n_cells = 600, strata_names = strata_names )


# # Record package versions
# sink("session_info.txt", type = "output")
# sessionInfo()
# sink()
# 
# # Plot results
# results <- plot_results( full_fit, 
#                          zrange = c(-3,3),
#                          n_cells = 600, 
#                          strata_names = strata_names, 
#                          check_residuals=TRUE,
#                          n_samples=0)

# plot_results( full_fit, 
#               # zrange = c(-3,3), 
#               n_cells = 50^2, 
#               strata_names = strata_names)#, 
# check_residuals=TRUE,
# n_samples=0)

# saveRDS(results, file = "VASTresults.RDS")
# 
# map_list = make_map_info( "Region"=settings$Region, "spatial_list"=full_fit$spatial_list, "Extrapolation_List"=full_fit$extrapolation_list )
# plot_maps(fit = full_fit, Obj=full_fit$tmb_list$Obj, PlotDF=map_list[["PlotDF"]] )
# 
# 
# # ESP products
# write.csv( results$Range$COG_Table, file="COG.csv", row.names=FALSE )
# 
# ##save effective area occupied for ESP request
# report = TMB::summary.sdreport(full_fit$parameter_estimates$SD)
# ln_km2 = report[which(rownames(report)=="log_effective_area_ctl"),c('Estimate','Std. Error')]
# Year <- sort(unique(full_fit$year_labels))
# ln_km2 <- as.data.frame(cbind(ln_km2, Year))
# ln_km2 <- ln_km2[which(ln_km2$Year %in% unique(full_fit$data_frame$t_i)),]
# write.csv( ln_km2, file="ln_effective_area.csv", row.names=FALSE )
