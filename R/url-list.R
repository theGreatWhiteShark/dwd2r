### url-list.R - A function listing all available URLs of the FTP
###   server.
##
##' @title Print all URLs of the FTP server
##' @description Provides a list of all possible download folders for
##'   a manual treatment.
##' @details If you, instead, want to choose the download folder
##'   interactively, please see the \code{\link{get.dwd.ftp.url}}
##'   function.
##'
##' @export
##'
##' @return A list containing all URLs of the DWD FTP server grouped
##'   by their type of measurement. It contains six elements
##'   \itemize{
##'     \item{\emph{observations.germany} - Corresponding to all links
##'             to observation data throughout Germany}
##'     \item{\emph{observations.global} - Corresponding to all links
##'             to observation data throughout the globe}
##'     \item{\emph{grid.germany} - Corresponding to all links
##'             to the grid data spanning Germany}
##'     \item{\emph{grid.europe} - Corresponding to all links
##'             to the grid data spanning Europe}
##'     \item{\emph{derived.germany} - Corresponding to all links
##'             to derived data within Germany}
##'     \item{\emph{regional.averages.germany} - Corresponding to all
##'             links regional averages throughout Germany}
##'   }
##' @family dwd-urls
##' @author Philipp Mueller
cat.dwd.ftp.url <- function(){
  ## Root URL of the FTP server of the DWD
  url.root <- "ftp://ftp-cdc.dwd.de/pub/CDC/"

  ## All URLs corresponding to observation data throughout Germany
  urls.observations.germany <- c(
      "observations_germany/climate/1_minute/precipitation/now/",
      "observations_germany/climate/1_minute/precipitation/historical/",
      "observations_germany/climate/1_minute/precipitation/recent/",
      "observations_germany/climate/1_minute/precipitation/meta/",
      Reduce( c, lapply( paste0(
                     "observations_germany/climate/10_minutes/",
                     c( "air_temperature/", "extreme_temperature/",
                       "extreme_wind/", "precipitation/", "solar/",
                       "wind/" ) ),
                     function( pp )
                       paste0(pp,c( "historical/", "recent/",
                                   "now/", "meta/" ) ) ) ),
      Reduce( c, lapply( paste0(
                     "observations_germany/climate/hourly/",
                     c( "air_temperature/", "soil_temperature/",
                       "pressure/", "precipitation/", "wind/",
                       "cloud_type/", "cloudiness/", "sun/",
                       "visibility/" ) ),
                     function( pp )
                       paste0( pp, c( "historical/", "recent/" ) ) ) ),
      "observations_germany/climate/hourly/solar/",
      "observations_germany/climate/subdaily/",
      Reduce( c, lapply( paste0(
                     "observations_germany/climate/daily/",
                     c( "kl/", "soil_temperature/", "more_precip/",
                       "water_equiv/" ) ),
                     function( pp )
                       paste0( pp, c( "historical/", "recent/" ) ) ) ),
      "observations_germany/climate/daily/solar/",
      Reduce( c, lapply( paste0(
                     "observations_germany/climate/monthly/",
                     c( "kl/", "more_precip/" ) ),
                     function( pp )
                       paste0( pp, c( "historical/", "recent/" ) ) ) ),
      "observations_germany/climate/multi_annual/mean_61-90/",
      "observations_germany/climate/multi_annual/mean_71-00/",
      "observations_germany/climate/multi_annual/mean_81-10/",
      paste0( "observations_germany/climate_urban/hourly/",
             c( "air_temperature/", "soil_temperature/",
               "pressure/", "precipitation/", "wind/", "sun/" ),
             'recent/' ),
      Reduce( c, lapply( paste0(
                     "observations_germany/phenology/annual_reporters/",
                     c( "crops/", "fruit/", "vine/", "wild/" ) ),
                     function( pp )
                       paste0( pp, c( "historical/", "recent/" ) ) ) ),
      "observations_germany/phenology/annual_reporters/farming/historical/",
      "observations_germany/radiosondes/monthly/air_temperature/homogenized/project_PASt/",
      "observations_germany/radiosondes/monthly/air_temperature/raw/project_PASt/" )

  ## All URLs corresponding to the global observation data 
  urls.observations.global <- c(
      Reduce( c, lapply( paste0(
                     "observations_global/CLIMAT/monthly/qc/",
                     c( "air_temperature_absolute_max/",
                       "air_temperature_absolute_min/",
                       "air_temperature_mean/",
                       "air_temperature_mean_of_daily_min/",
                       "air_temperature_mean_of_daily_max/",
                       "mean_sea_level_pressure/",
                       "precipGE1mm_days/", "precipitation_total/",
                       "sunshine_duration/", "vapour_pressure/" ) ),
                     function( pp )
                       paste0( pp, c( "historical/", "recent/" ) ) )),
      "observations_global/CLIMAT/monthly/raw/",
      paste0( "observations_global/CLIMAT/multi_annual/",
             c( "air_temperature_mean/",
               "air_temperature_mean_of_daily_min/",
               "air_temperature_mean_of_daily_max/",
               "mean_sea_level_pressure/",
               "precipitation_total/", "sunshine_duration/") ) )
      
  ## All URLs corresponding to the grid data of Germany
  urls.grid.germany <- c(
      paste0( "grids_germany/hourly/Project_TRY/",
             c( "air_temperature_mean/", "cloud_cover/", "dew_point/",
               "humidity/", "pressure/", "radiation_direct/",
               "radiation_downwelling/", "radiation_global/",
               "radiation_upwelling/", "vapour_pressure/",
               "wind_direction/", "wind_speed/" ) ),
      paste0( "grids_germany/hourly/radolan/",
             c( 'Unterstuetzungsdokumente/',
               'RADOLAN-Koordinatendateien Lambda Phi/1100x900/',
               'RADOLAN-Koordinatendateien Lambda Phi/1500x1400/',
               'RADOLAN-Koordinatendateien Lambda Phi/900x900/',
               'recent/', 'historical/' ) ),
      paste0( "grids_germany/daily/Project_TRY/",
             c( "air_temperature_mean/", "cloud_cover/", "dew_point/",
               "humidity/", "pressure/", "radiation_direct/",
               "radiation_downwelling/", "radiation_global/",
               "radiation_upwelling/", "vapour_pressure/",
               "wind_direction/", "wind_speed/" ) ),
      paste0( "grids_germany/daily/radolan/",
             c( 'Unterstuetzungsdokumente/', 'recent/',
               'historical/' ) ),
      paste0( "grids_germany/daily/",
             c( "evapo_p/", "evapo_r/", "frost_depth/", "regnie/",
               "soil_moist/", "soil_temperature_5cm/" ) ),
      paste0( "grids_germany/monthly/Project_TRY/",
             c( "air_temperature_mean/", "cloud_cover/", "dew_point/",
               "humidity/", "pressure/", "radiation_direct/",
               "radiation_downwelling/", "radiation_global/",
               "radiation_upwelling/", "vapour_pressure/",
               "wind_direction/", "wind_speed/" ) ),
      Reduce( c, lapply( paste0(
                     "grids_germany/monthly/",
                     c( "air_temperature_max/", "drought_index/",
                       "air_temperature_mean/",
                       "air_temperature_min/", "precipitation/",
                       "sunshine_duration/" ) ),
                     function( pp )
                       paste0( pp, c( '01_Jan/', '02_Feb/', '03_Mar/',
                                     '04_Apr/', '05_May/', '06_Jun/',
                                     '07_Jul/', '08_Aug/', '09_Sep/',
                                     '10_Oct/', '11_Nov/',
                                     '12_Dec/' ) ) ) ),
      paste0( "grids_germany/daily/",
             c( "evapo_p/", 'radiation_diffuse/', 'radiation_direct/',
               'radiation_global/', "evapo_r/", "frost_depth/",
               "regnie/", "soil_moist/", "soil_temperature_5cm/" ) ),
      Reduce( c, lapply( paste0(
                     "grids_germany/seasonal/",
                     c( "air_temperature_max/", "drought_index/",
                       "air_temperature_mean/",
                       "air_temperature_min/",
                       "precipitation/", "sunshine_duration/" ) ),
                     function( pp )
                       paste0( pp, c( '13_MAM/', '14_JJA/', '15_SON/',
                                     '16_DJF/' ) ) ) ),
      "grids_germany/halfyear/precipitation/18_NDJFMA/",
      "grids_germany/halfyear/precipitation/19_MJJASO/",
      paste0( "grids_germany/daily/",
             c( "phenology/", "drought_index/", "precipGE10mm_days/",
               "precipGE20mm_days/", "precipGE30mm_days/", "regnie/",
               "snowcover_days/", "summer_days/",
               "air_temperature_max/", "air_temperature_min/",
               "air_temperature_mean/", "precipitation/",
               "radiation_diffuse/", "radiation_direct/",
               "radiation_global/", "sunshine_duration/",
               "hot_days", "ice_days", "frost_days",
               "vegetation_begin", "vegetation_end" ) ),
      paste0( "grids_germany/daily/",
             c("solar/", "drought_index/", "precipGE10mm_days/",
               "precipGE20mm_days/", "precipGE30mm_days/", "regnie/",
               "snowcover_days/", "summer_days/",
               "air_temperature_max/",
               "air_temperature_min/", "air_temperature_mean/",
               "precipitation/", "soil_moist/",
               "soil_temperature_5cm/",
               "radiation_global/", "sunshine_duration/", "hot_days/",
               "ice_days/", "frost_days/", "vegetation_begin/",
               "vegetation_end/", "evapo_p/", "evapo_r/",
               "water_balance/", "wind_parameters/" ) ),
      paste0( "grids_germany/multi_annual/wind_parameters/",
             c( 'resol_1000x1000', 'resol_200x200' ) ),
      Reduce( c, lapply( paste0(
                     "grids_germany/multi_annual/",
                     c( "air_temperature_mean/",
                       "air_temperature_min/", 
                       "precipitation/", "sunshine_duration/" ) ),
                     function( pp )
                       paste0( pp, c( '6190', '7100', '8110' ) ) ) ),
      paste0(
          "grids_germany/return_periods/precipitation/KOSTRA/KOSTRA_DWD_2010R/",
          c( "asc/", "gis/" ) ) )

  ## All URLs corresponding to the grid data throughout Europe.
  urls.grid.europe <- c(
      Reduce( c, lapply( paste0(
                     "grids_europe/daily/",
                     c( "air_temperature_max/MIKLIP_DECREG/v002/",
                       "air_temperature_mean/MIKLIP_DECREG/v002/",
                       "air_temperature_min/MIKLIP_DECREG/v002/",
                       "wind/MIKLIP_DECREG/v002/" ) ),
                     function( pp )
                       paste0( pp, c( '01_Jan/', '02_Feb/', '03_Mar/',
                                     '04_Apr/', '05_May/', '06_Jun/',
                                     '07_Jul/', '08_Aug/', '09_Sep/',
                                     '10_Oct/', '11_Nov/',
                                     '12_Dec/' ) ) ) ),
      paste0( "grids_europe/monthly/",
             c("air_temperature_max/", "air_temperature_min/",
               "air_temperature_mean/", "wind/"),
             'MIKLIP_DECREG/v002/' ),
      "grids_europe/monthly/cloud_cover/SEVIRI/" )

  ## All URLs corresponding to the derived data from Germany
  urls.derived.germany <- c(
      "derived_germany/soil/multi_annual/mean_91-10/",
      Reduce( c, lapply( paste0(
                     "derived_germany/soil/",
                     c( "daily/", "monthly/" ) ),
                     function( pp )
                       paste0( pp, c( "historical/", "recent/" ) ) )),
      paste0(
          "derived_germany/techn/monthly/heating_degreedays/hdd_3807/",
          c( 'recent/', 'historical/' ) ) )

  ## All URLs corresponding to the regional averages over Germany
  urls.regional.averages.germany <- c(
      Reduce( c, lapply( paste0(
                     "regional_averages_DE/",
                     c( "seasonal/", "monthly/", "annual/" ) ),
                     function( pp )
                       paste0( pp, c( "air_temperature_mean/",
                                     "precipitation/",
                                     "sunshine_duration/" ) ) ) ) )

  ## Combining all URLs into one list
  url.list <- list(
      observations.germany = paste0( url.root,
                                    urls.observations.germany ),
      observations.global = paste0( url.root,
                                    urls.observations.global ),
      grid.germany = paste0( url.root,
                                    urls.grid.germany ),
      grid.europe = paste0( url.root,
                                    urls.grid.europe ),
      derived.germany = paste0( url.root,
                                    urls.derived.germany ),
      regional.averages.germany =
        paste0( url.root, urls.regional.averages.germany ) )

  return( url.list )
}
## End of url-list.R
