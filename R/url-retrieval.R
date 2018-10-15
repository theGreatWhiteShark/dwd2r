### url-retrieval.R - All functions concerned with the
### interaction with FTP server of the DWD

##' @title Obtain URLs to the FTP server
##' @description Interactive function to extract the download URLs of the
##'   FTP server of the DWD
##' @details Since there is A LOT of different data provided by
##'   the German weather service (DWD), it makes not sense to specify
##'   them all using input arguments to this function. Instead, the
##'   user will be guided interactively through the different data
##'   sources and specifies the data she want using the inputs at the
##'   prompt. If you prefer to run the function non-interactive, you
##'   can use the \emph{batch.choices} argument and supply a vector of
##'   numbers corresponding to your choices.
##'
##'   As another option, you can use the \code{\link{cat.dwd.ftp.url}}
##'   function, which returns a vector containing all download
##'   URLs. This vector can then be manipulated using e.g. the grep
##'   command.
##'
##'   Since there is a lot of different set of data, it is also not
##'   possible to provide documentation for the individual data
##'   sources. Please see the \url{ftp://ftp-cdc.dwd.de/pub/CDC/} FTP
##'   server for further details.
##'
##'   Before using the data of the DWD, be sure you read its
##'   \url{ftp://ftp-cdc.dwd.de/pub/CDC/Terms_of_use.pdf}!
##'
##' @param batch.choices Numerical vector containing the numbers,
##'   which corresponds to the choices in the interactive mode. If
##'   NULL, the choices will be done interactively. Default = NULL.
##'
##' @export
##'
##' @examples
##' ## Get the URLs pointing to the aggregated collection of the daily
##' ## measured climatic data of the DWD.
##' get.dwd.ftp.url( batch.choices  = c( 1, 1, 5, 1 ) )
##'
##' @return List containing to two elements
##'   \itemize{
##'     \item{ \emph{data} - a character vector containing one or more
##'              URLs pointing to the different data folders. }
##'     \item{ \emph{meta} - character vector pointing to the folder
##'              containing the meta data. Or NULL if there is no
##'              distinct folder and the meta data are contained in
##'              the folders present in the \emph{data} URLs } 
##'   }
##'
##' @family dwd-urls
##' @author Philipp Mueller
get.dwd.ftp.url <- function( batch.choices = NULL ){
  ## The choices for the batch mode have to be provided as a numerical
  ## vector.
  if ( !is.null( batch.choices ) && !is.numeric( batch.choices ) ){
    stop( "Please provide numerical batch choices" )
  }
  
  ## Starting at the top level of the FTP server
  ## Basic URL of the FTP server
  url.root <- "ftp://ftp-cdc.dwd.de/pub/CDC/"

  if ( !is.null( batch.choices ) ){
    selection.top.level <- as.character( batch.choices[ 1 ] )
  } else {
    cat( '\nSelecting the data of the DWD to download.\n\n' )
    cat( '\tGeneral data type:\n\n' )
    cat( '\t\t1) Observation data from Germany\n' )
    cat( '\t\t2) Observation data around the globe\n' )
    cat( '\t\t3) Grid data throughout Germany\n' )
    cat( '\t\t4) Grid data throughout Europe\n' )
    cat( '\t\t5) Derived data from Germany\n' )
    cat( '\t\t6) Regional averages throughout Germany\n' )
    selection.top.level <- readline( 'Selection: ' )
  }

  url.top.level <-
    switch( selection.top.level,
           "1" = "observations_germany/",
           "2" = "observations_global/",
           "3" = "grids_germany/",
           "4" = "grids_europe/",
           "5" = "derived_germany/",
           "6" = "regional_averages_DE/",
           "First batch choice out of range" )
  
  ## Sanity check
  dwd2r.url.check( paste0( url.root, url.top.level ) )
  
  if ( selection.top.level == "1" ){
    ##
    ## Observation data from Germany
    ##
    if ( !is.null( batch.choices ) ){
      selection.second.level <- as.character( batch.choices[ 2 ] )
    } else {
      cat( '\n\n\tSpecific data type:\n' )
      cat( '\t\t1) Climate\n' )
      cat( '\t\t2) Urban climate\n' )
      cat( '\t\t3) Phenology\n' )
      cat( '\t\t4) Radiosondes\n' )
      selection.second.level <- readline( 'Selection: ' )
    }
    
    url.second.level <-
      switch( selection.second.level,
             "1" = "climate/",
             "2" = "climate_urban/",
             "3" = "phenology/",
             "4" = "radiosondes/",
             "Second batch choice out of range" )
    
    ## Sanity check
    dwd2r.url.check( paste0( url.root, url.top.level,
                            url.second.level ) )

    if ( selection.second.level == "1" ){
      ##
      ## Observation data from Germany - climate
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tMeasurement intervals:\n' )
        cat( '\t\t1) 1 minute\n' )
        cat( '\t\t2) 10 minutes\n' )
        cat( '\t\t3) Hourly\n' )
        cat( '\t\t4) Subdaily\n' )
        cat( '\t\t5) Daily\n' )
        cat( '\t\t6) Monthly\n' )
        cat( '\t\t7) Multi annual\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "1_minute/",
                                "2" = "10_minutes/",
                                "3" = "hourly/",
                                "4" = "subdaily/",
                                "5" = "daily/",
                                "6" = "monthly/",
                                "7" = "multi_annual/",
                                "Third batch choice out of range" )
      
      ## Sanity check
      dwd2r.url.check( paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level ) )

      if ( selection.third.level == "1" ){
        ##
        ## Observation data from Germany - climate -
        ## 1 minute
        ##
        if ( is.null( batch.choices ) ){
          cat( '\n\nPrecipitation data found.' )
        }
        list.url.final <-
          list( data = paste0(
                    paste0( url.root, url.top.level,
                           url.second.level, url.third.level,
                           'precipitation/' ),
                    c( "historical/", "now/", "recent/" ) ),
               meta = paste0( url.root, url.top.level,
                             url.second.level, url.third.level,
                             "precipitation/meta/" ) )
      } else if ( selection.third.level == "2" ){
        ##
        ## Observation data from Germany - climate -
        ## 10 minutes
        ##
        if ( !is.null( batch.choices ) ){
          selection.fourth.level <- as.character( batch.choices[ 4 ] )
        } else {
          cat( '\n\n\tMeasurement variable:\n' )
          cat( '\t\t1) Air temperature\n' )
          cat( '\t\t2) Extreme temperature\n' )
          cat( '\t\t3) Extreme wind\n' )
          cat( '\t\t4) Precipitation\n' )
          cat( '\t\t5) Solar\n' )
          cat( '\t\t6) Wind\n' )
          selection.fourth.level <- readline( "Selection: " )
        }

        url.fourth.level <- switch( selection.fourth.level,
                                   "1" = "air_temperature/",
                                   "2" = "extreme_temperature/",
                                   "3" = "extreme_wind/",
                                   "4" = "precipitation/",
                                   "5" = "solar/",
                                   "6" = "wind/",
                                   "Fourth batch choice out of range" )
        ## They all feature the same structure, so no need to handle
        ## them separately.
        list.url.final <-
          list( data = paste0(
                    paste0( url.root, url.top.level,
                           url.second.level, url.third.level,
                           url.fourth.level ),
                    c( "historical/", "now/", "recent/" ) ),
               meta = paste0( url.root, url.top.level,
                             url.second.level, url.third.level,
                             url.fourth.level, "meta/" ) )
      } else if ( selection.third.level == "3" ){
        ##
        ## Observation data from Germany - climate -
        ## hourly
        ##
        if ( !is.null( batch.choices ) ){
          selection.fourth.level <- as.character( batch.choices[ 4 ] )
        } else {
          cat( '\n\n\tMeasurement variable:\n' )
          cat( '\t\t1) Air temperature\n' )
          cat( '\t\t2) Soil temperature\n' )
          cat( '\t\t3) Pressure\n' )
          cat( '\t\t4) Precipitation\n' )
          cat( '\t\t5) Solar\n' )
          cat( '\t\t6) Wind\n' )
          cat( '\t\t7) Cloud type\n' )
          cat( '\t\t8) Cloudiness\n' )
          cat( '\t\t9) Sun\n' )
          cat( '\t\t10) Visibility\n' )
          selection.fourth.level <- readline( "Selection: " )
        }

        url.fourth.level <- switch( selection.fourth.level,
                                   "1" = "air_temperature/",
                                   "2" = "soil_temperature/",
                                   "3" = "pressure/",
                                   "4" = "precipitation/",
                                   "5" = "solar/",
                                   "6" = "wind/",
                                   "7" = "cloud_type/",
                                   "8" = "cloudiness/",
                                   "9" = "sun/",
                                   "10" = "visibility/",
                                   "Fourth batch choice out of range" )
        if ( selection.fourth.level %in% c( "1", "2", "3", "4", "6",
                                           "7", "8", "9", "10" ) ){
          list.url.final <-
            list( data = paste0(
                      paste0( url.root, url.top.level,
                             url.second.level, url.third.level,
                             url.fourth.level ),
                      c( "historical/", "recent/" ) ),
                 meta = NULL )
        } else if ( selection.fourth.level == "5" ) {
          ## The 'solar' data, however, does is not structured in
          ## historical and recent data.
          list.url.final <-
            list( data = paste0( url.root, url.top.level,
                                url.second.level, url.third.level,
                                url.fourth.level ),
                 meta = NULL )
        } else {
          stop( "Fourth batch choice out of range" )
        }
      } else if ( selection.third.level == "4" ){
        ##
        ## Observation data from Germany - climate -
        ## subdaily
        ##
        if ( is.null( batch.choices ) ){
          cat( '\n\nAggregated data found (different measurement variables inside one document.' )
        }
        list.url.final <-
          list( data = paste0( url.root, url.top.level,
                              url.second.level, url.third.level,
                              'standard_format/' ),
               meta = NULL )
      } else if ( selection.third.level == "5" ){
        ##
        ## Observation data from Germany - climate -
        ## daily
        ##
        if ( !is.null( batch.choices ) ){
          selection.fourth.level <- as.character( batch.choices[ 4 ] )
        } else {
          cat( '\n\n\tMeasurement variable:\n' )
          cat( '\t\t1) Classical aggregated data (multiple measurement variables)\n' )
          cat( '\t\t2) Additional precipitation data\n' )
          cat( '\t\t3) Soil temperature\n' )
          cat( '\t\t4) Solar\n' )
          cat( '\t\t5) Water equivalence\n' )
          selection.fourth.level <- readline( "Selection: " )
        }

        url.fourth.level <- switch( selection.fourth.level,
                                   "1" = "kl/",
                                   "2" = "more_precip/",
                                   "3" = "soil_temperature/",
                                   "4" = "solar/",
                                   "5" = "water_equiv/",
                                   "Fourth batch choice out of range" )
        if ( selection.fourth.level %in%  c( "1", "2", "3", "5" ) ){
          list.url.final <-
            list( data = paste0(
                      paste0( url.root, url.top.level,
                             url.second.level, url.third.level,
                             url.fourth.level ),
                      c( "historical/", "recent/" ) ),
                 meta = NULL )
        } else if ( selection.fourth.level == "4" ) {
          ## The 'solar' data, however, does is not structured in
          ## historical and recent data.
          list.url.final <-
            list( data = paste0( url.root, url.top.level,
                                url.second.level, url.third.level,
                                url.fourth.level ),
                 meta = NULL )
        } else {
          stop( "Fourth batch choice out of range" )
        }
      } else if ( selection.third.level == "6" ){
        ##
        ## Observation data from Germany - climate -
        ## monthly
        ##
        if ( !is.null( batch.choices ) ){
          selection.fourth.level <- as.character( batch.choices[ 4 ] )
        } else {
          cat( '\n\n\tMeasurement variable:\n' )
          cat( '\t\t1) Classical aggregated data (multiple measurement variables)\n' )
          cat( '\t\t2) Additional precipitation data\n' )
          selection.fourth.level <- readline( "Selection: " )
        }

        url.fourth.level <- switch( selection.fourth.level,
                                   "1" = "kl/",
                                   "2" = "more_precip/",
                                   "Fourth batch choice out of range" )
        list.url.final <-
          list( data = paste0(
                    paste0( url.root, url.top.level,
                           url.second.level, url.third.level,
                           url.fourth.level ),
                    c( "historical/", "recent/" ) ),
               meta = NULL )
      } else if ( selection.third.level == "7" ){
        ##
        ## Observation data from Germany - climate -
        ## multi annual
        ##
        if ( !is.null( batch.choices ) ){
          selection.fourth.level <- as.character( batch.choices[ 4 ] )
        } else {
          cat( '\n\n\tObservation period:\n' )
          cat( '\t\t1) Mean values from 1961 till 1990\n' )
          cat( '\t\t2) Mean values from 1971 till 2000\n' )
          cat( '\t\t3) Mean values from 1981 till 2010\n' )
          selection.fourth.level <- readline( "Selection: " )
        }

        url.fourth.level <- switch( selection.fourth.level,
                                   "1" = "mean_61-90/",
                                   "1" = "mean_71-00/",
                                   "1" = "mean_81-10/",
                                   "Fourth batch choice out of range" )
        list.url.final <-
          list( data = paste0( url.root, url.top.level,
                              url.second.level, url.third.level,
                              url.fourth.level ),
               meta = NULL )
      } else {
        stop( "Third batch choice out of range" )
      }
    } else if ( selection.second.level == "2" ){
      ##
      ## Observation data from Germany - climate urban
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tHourly available measurement variables:\n' )
        cat( '\t\t1) Air temperature\n' )
        cat( '\t\t2) Soil temperature\n' )
        cat( '\t\t3) Pressure\n' )
        cat( '\t\t4) Precipitation\n' )
        cat( '\t\t5) Wind\n' )
        cat( '\t\t6) Sun\n' )
        selection.third.level <- readline( "Selection: " )
      }

      url.third.level <- switch( selection.third.level,
                                "1" = "air_temperature/",
                                "2" = "soil_temperature/",
                                "3" = "pressure/",
                                "4" = "precipitation/",
                                "5" = "wind/",
                                "6" = "sun/",
                                "Third batch choice out of range" )
      list.url.final <-
        list( data = paste0( url.root, url.top.level,
                            url.second.level, url.third.level,
                            'recent/' ),
             meta = NULL )
    } else if ( selection.second.level == "3" ){
      ##
      ## Observation data from Germany - phenology
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tResponse type:\n' )
        cat( '\t\t1) Annual reporting\n' )
        cat( '\t\t2) Immediate reporting\n' )
        selection.third.level <- readline( "Selection: " )
      }

      url.third.level <- switch( selection.third.level,
                                "1" = "annual_reporters/",
                                "2" = "immediate_reporters/",
                                "Third batch choice out of range" )

      ## Sanity check
      dwd2r.url.check( paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level ) )
      if ( selection.third.level == "1" ){
        ##
        ## Observation data from Germany - phenology -
        ## annual reporters
        ## 
        if ( !is.null( batch.choices ) ){
          selection.fourth.level <- as.character( batch.choices[ 4 ] )
        } else {
          cat( '\n\n\tMeasurement variable:\n' )
          cat( '\t\t1) Crops\n' )
          cat( '\t\t2) Farming\n' )
          cat( '\t\t3) Fruit\n' )
          cat( '\t\t4) Vine\n' )
          cat( '\t\t5) Wild\n' )
          selection.fourth.level <- readline( "Selection: " )
        }

        url.fourth.level <- switch( selection.fourth.level,
                                   "1" = "crops/",
                                   "2" = "farming/",
                                   "3" = "fruit/",
                                   "4" = "vine/",
                                   "5" = "wild/",
                                   "Fourth batch choice out of range" )
        if ( selection.fourth.level %in% c( "1", "3", "4", "5" ) ){
          list.url.final <-
            list( data = paste0(
                      paste0( url.root, url.top.level,
                             url.second.level,
                             url.third.level,
                             url.fourth.level,
                             c( 'recent/', 'historical/' ) ) ),
                 meta = NULL )
        } else if ( selection.fourth.level == "2" ){
          ## It seems there are no recent measurements of the farming.
          list.url.final <-
            list( data = paste0( url.root, url.top.level,
                                url.second.level,
                                url.third.level,
                                url.fourth.level,
                                'historical/' ),
                 meta = NULL )
        } else {
          stop( "Fourth batch choice out of range" )
        }
      } else if ( selection.third.level == "2" ){
        ##
        ## Observation data from Germany - phenology -
        ## immediate reporters
        ## 
        if ( !is.null( batch.choices ) ){
          selection.fourth.level <- as.character( batch.choices[ 4 ] )
        } else {
          cat( '\n\n\tMeasurement variable:\n' )
          cat( '\t\t1) Crops\n' )
          cat( '\t\t2) Fruit\n' )
          cat( '\t\t3) Vine\n' )
          cat( '\t\t4) Wild\n' )
          selection.fourth.level <- readline( "Selection: " )
        }

        url.fourth.level <- switch( selection.fourth.level,
                                   "1" = "crops/",
                                   "2" = "fruit/",
                                   "3" = "vine/",
                                   "4" = "wild/",
                                   "Fourth batch choice out of range" )
        list.url.final <-
          list( data = paste0(
                    paste0( url.root, url.top.level,
                           url.second.level,
                           url.third.level,
                           url.fourth.level,
                           c( 'recent/', 'historical/' ) ) ),
               meta = NULL )
      } else {
        stop( "Third batch choice out of range" )
      }
    } else if ( selection.second.level == "4" ){
      ##
      ## Observation data from Germany - radiosondes
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tAvailable kinds of monthly air temperatures:\n' )
        cat( '\t\t1) Homogenized\n' )
        cat( '\t\t2) Raw\n' )
        selection.third.level <- readline( "Selection: " )
      }

      url.third.level <- switch( selection.third.level,
                                "1" = "homogenized/",
                                "2" = "raw/",
                                "Third batch choice out of range" )
      list.url.final <-
        list( data = paste0( url.root, url.top.level,
                            url.second.level,
                            'project_PASt/' ),
             meta = NULL )
      
      if ( is.null( batch.choices ) ){
        cat( '\n\n\tProject PASt data found' )
      }
    } else {
      stop( "Second batch choice out of range" )
    }

  } else if ( selection.top.level == "2" ){
    ##
    ## Observation data from around the globe
    ##
    if ( !is.null( batch.choices ) ){
      selection.second.level <- as.character( batch.choices[ 2 ] )
    } else {
      cat( '\n\n\tMeasurement interval of the CLIMAT data:\n' )
      cat( '\t\t1) Monthly\n' )
      cat( '\t\t2) Multi annual\n' )
      selection.second.level <- readline( "Selection: " )
    }

    url.second.level <- switch( selection.second.level,
                               "1" = "CLIMAT/monthly/",
                               "2" = "CLIMAT/multi_annual/",
                               "Second batch choice out of range" )

    ## Sanity check
    dwd2r.url.check( paste0( url.root, url.top.level,
                            url.second.level ) )
    if ( selection.second.level == "1" ){
      ##
      ## Observation data from around the globe - monthly
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tFormatting of the data:\n' )
        cat( '\t\t1) Qc\n' )
        cat( '\t\t2) Raw\n' )
        selection.third.level <- readline( "Selection: " )
      }

      url.third.level <- switch( selection.third.level,
                                "1" = "qc/",
                                "2" = "raw/",
                                "Third batch choice out of range" )
      ## Sanity check
      dwd2r.url.check( paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level ) )
      if ( selection.third.level == "1" ){
        ##
        ## Observation data from around the globe - monthly
        ## qc
        ##
        if ( !is.null( batch.choices ) ){
          selection.fourth.level <- as.character( batch.choices[ 4 ] )
        } else {
          cat( '\n\n\tMeasurement variable:\n' )
          cat( '\t\t1) Maximal absolute air temperature\n' )
          cat( '\t\t2) Minimal absolute air temperature\n' )
          cat( '\t\t3) Mean air temperature\n' )
          cat( '\t\t4) Mean of daily maximal air temperature\n' )
          cat( '\t\t5) Mean of daily minimal air temperature\n' )
          cat( '\t\t6) Mean sea level pressure\n' )
          cat( '\t\t7) Monthly days with more than 1mm precipitation\n' )
          cat( '\t\t8) Total precipitation\n' )
          cat( '\t\t9) Sunshine duration\n' )
          cat( '\t\t10) Vapour pressure\n' )
          selection.fourth.level <- readline( "Selection: " )
        }

        url.fourth.level <- switch(
            selection.fourth.level,
            "1" = "air_temperature_absolute_max/",
            "2" = "air_temperature_absolute_min/",
            "3" = "air_temperature_mean/",
            "4" = "air_temperature_mean_of_daily_min/",
            "5" = "air_temperature_mean_of_daily_max/",
            "6" = "mean_sea_level_pressure/",
            "7" = "precipGE1mm_days/",
            "8" = "precipitation_total/",
            "9" = "sunshine_duration/",
            "10" = "vapour_pressure/",
            "Fourth batch choice out of range" )
        
        list.url.final <-
          list( data = paste0(
                    paste0( url.root, url.top.level,
                           url.second.level,
                           url.third.level,
                           url.fourth.level,
                           c( 'recent/', 'historical/' ) ) ),
               meta = NULL )
      } else if ( selection.third.level == "2" ){
        ##
        ## Observation data from around the globe - monthly
        ## raw
        ##
        list.url.final <- 
          list( data = paste0( url.root, url.top.level,
                              url.second.level, url.third.level ),
               meta = NULL )
      } else {
        stop( "Third batch choice out of range" )
      }
    } else if ( selection.second.level == "2" ){
      ##
      ## Observation data from around the globe - multi annual
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tMeasurement variable:\n' )
        cat( '\t\t1) Mean air temperature\n' )
        cat( '\t\t2) Mean of daily maximal air temperature\n' )
        cat( '\t\t3) Mean of daily minimal air temperature\n' )
        cat( '\t\t4) Mean sea level pressure\n' )
        cat( '\t\t5) Total precipitation\n' )
        cat( '\t\t6) Sunshine duration\n' )
        selection.third.level <- readline( "Selection: " )
      }

      url.third.level <- switch(
          selection.third.level,
          "1" = "air_temperature_mean/",
          "2" = "air_temperature_mean_of_daily_min/",
          "3" = "air_temperature_mean_of_daily_max/",
          "4" = "mean_sea_level_pressure/",
          "5" = "precipitation_total/",
          "6" = "sunshine_duration/",
          "Third batch choice out of range" )
      
      list.url.final <-
        list( data = paste0( url.root, url.top.level,
                            url.second.level,
                            url.third.level ),
             meta = NULL )
    } else {
      stop( "Second batch choice out of range" )
    }
  } else if ( selection.top.level == "3" ){
    ##
    ## Grid data throughout Germany
    ##
    if ( !is.null( batch.choices ) ){
      selection.second.level <- as.character( batch.choices[ 2 ] )
    } else {
      cat( '\n\n\tMeasurement intervals:\n' )
      cat( '\t\t1) Hourly\n' )
      cat( '\t\t2) Daily\n' )
      cat( '\t\t3) Monthly\n' )
      cat( '\t\t4) Seasonal\n' )
      cat( '\t\t5) Half a year\n' )
      cat( '\t\t6) Annual\n' )
      cat( '\t\t7) Multi annual\n' )
      cat( '\t\t8) Return periods\n' )
      selection.second.level <- readline( "Selection: " )
    }
    url.second.level <- switch( selection.second.level,
                               "1" = "hourly/",
                               "2" = "daily/",
                               "3" = "monthly/",
                               "4" = "seasonal/",
                               "5" = "halfyear/",
                               "6" = "annual/",
                               "7" = "multi_annual/",
                               "8" = "return_periods/",
                               "Second batch choice out of range" )
    
    ## Sanity check
    dwd2r.url.check( paste0( url.root, url.top.level,
                            url.second.level ) )

    if ( selection.second.level == "1" ){
      ##
      ## Grid data throughout Germany - hourly
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tData source:\n' )
        cat( '\t\t1) Project TRY\n' )
        cat( '\t\t2) Radolan\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "Project_TRY/",
                                "2" = "radolan/",
                                "Third batch choice out of range" )
      
      ## Sanity check
      dwd2r.url.check( paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level) )
      if ( selection.third.level == "1" ){
        ##
        ## Grid data throughout Germany - hourly
        ## Project TRY
        ##
        if ( !is.null( batch.choices ) ){
          selection.fourth.level <- as.character( batch.choices[ 4 ] )
        } else {
          cat( '\n\n\tMeasurement variable:\n' )
          cat( '\t\t1) Mean air temperature\n' )
          cat( '\t\t2) Cloud coverage\n' )
          cat( '\t\t3) Dew point\n' )
          cat( '\t\t4) Humidity\n' )
          cat( '\t\t5) Pressure\n' )
          cat( '\t\t6) Radiation (direct)\n' )
          cat( '\t\t7) Radiation (downwelling)\n' )
          cat( '\t\t8) Radiation (global)\n' )
          cat( '\t\t9) Radiation (upwelling)\n' )
          cat( '\t\t10) Vapour pressure\n' )
          cat( '\t\t11) Wind direction\n' )
          cat( '\t\t12) Wind speed\n' )
          selection.fourth.level <- readline( "Selection: " )
        }

        url.fourth.level <- switch(
            selection.fourth.level,
            "1" = "air_temperature_mean/",
            "2" = "cloud_cover/",
            "3" = "dew_point/",
            "4" = "humidity/",
            "5" = "pressure/",
            "6" = "radiation_direct/",
            "7" = "radiation_downwelling/",
            "8" = "radiation_global/",
            "9" = "radiation_upwelling/",
            "10" = "vapour_pressure/",
            "11" = "wind_direction/",
            "12" = "wind_speed/",
            "Fourth batch choice out of range" )
        
        list.url.final <-
          list( data = paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level,
                              url.fourth.level ),
               meta = NULL )
      } else if ( selection.third.level == "2" ){
        ##
        ## Grid data throughout Germany - hourly
        ## Radolan
        ##
        ## This folder is somewhat special. 
        
        list.url.final <-
          list(
              data = paste0(
                  paste0( url.root, url.top.level,
                         url.second.level,
                         url.third.level ),
                  c( 'recent/', 'historical/' ) ),
              meta = paste0(
                  paste0( url.root, url.top.level,
                         url.second.level, url.third.level ),
                  c( 'Unterstuetzungsdokumente/',
                    'RADOLAN-Koordinatendateien Lambda Phi/1100x900/',
                    'RADOLAN-Koordinatendateien Lambda Phi/1500x1400/',
                    'RADOLAN-Koordinatendateien Lambda Phi/900x900/'
                    ) ) )
      } else {
        stop( "Third batch choice out of range" )
      }
    } else if ( selection.second.level == "2" ){
      ##
      ## Grid data throughout Germany - daily
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tData source:\n' )
        cat( '\t\t1) Project TRY\n' )
        cat( '\t\t2) Radolan\n' )
        cat( '\t\t3) Potential evapotranspiration over gras\n' )
        cat( '\t\t4) Actual evapotranspiration over gras and sandy loam\n' )
        cat( '\t\t5) Frost depth of uncovered soil at midday\n' )
        cat( '\t\t6) Precipitation (REGNIE grid)\n' )
        cat( '\t\t7) Soil moisture at 60cm depth under gras and sandy loam\n' )
        cat( '\t\t8) Soil temperature at 5cm depth\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "Project_TRY/",
                                "2" = "radolan/",
                                "3" = "evapo_p/",
                                "4" = "evapo_r/",
                                "5" = "frost_depth/",
                                "6" = "regnie/",
                                "7" = "soil_moist/",
                                "8" = "soil_temperature_5cm/",
                                "Third batch choice out of range" )
      
      ## Sanity check
      dwd2r.url.check( paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level) )
      if ( selection.third.level == "1" ){
        ##
        ## Grid data throughout Germany - daily
        ## Project TRY
        ##
        if ( !is.null( batch.choices ) ){
          selection.fourth.level <- as.character( batch.choices[ 4 ] )
        } else {
          cat( '\n\n\tMeasurement variable:\n' )
          cat( '\t\t1) Mean air temperature\n' )
          cat( '\t\t2) Cloud coverage\n' )
          cat( '\t\t3) Dew point\n' )
          cat( '\t\t4) Humidity\n' )
          cat( '\t\t5) Pressure\n' )
          cat( '\t\t6) Radiation (direct)\n' )
          cat( '\t\t7) Radiation (downwelling)\n' )
          cat( '\t\t8) Radiation (global)\n' )
          cat( '\t\t9) Radiation (upwelling)\n' )
          cat( '\t\t10) Vapour pressure\n' )
          cat( '\t\t11) Wind direction\n' )
          cat( '\t\t12) Wind speed\n' )
          selection.fourth.level <- readline( "Selection: " )
        }

        url.fourth.level <- switch(
            selection.fourth.level,
            "1" = "air_temperature_mean/",
            "2" = "cloud_cover/",
            "3" = "dew_point/",
            "4" = "humidity/",
            "5" = "pressure/",
            "6" = "radiation_direct/",
            "7" = "radiation_downwelling/",
            "8" = "radiation_global/",
            "9" = "radiation_upwelling/",
            "10" = "vapour_pressure/",
            "11" = "wind_direction/",
            "12" = "wind_speed/",
            "Fourth batch choice out of range" )
        
        list.url.final <-
          list( data = paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level,
                              url.fourth.level ),
               meta = NULL )
      } else if ( selection.third.level == "2" ){
        ##
        ## Grid data throughout Germany - hourly
        ## Radolan
        ##
        ## This folder is somewhat special. 
        
        list.url.final <-
          list(
              data = paste0(
                  paste0( url.root, url.top.level,
                         url.second.level,
                         url.third.level ),
                  c( 'recent/', 'historical/' ) ),
              meta = paste0( url.root, url.top.level,
                            url.second.level, url.third.level,
                            'Unterstuetzungsdokumente/' ) )
      } else if ( selection.third.level %in% c( "3", "4", "5", "6",
                                               "7", "8" ) ) {
        ##
        ## Grid data throughout Germany - hourly
        ## other quantities
        list.url.final <-
          list( data = paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level ),
               meta = NULL )
      } else {
        stop( "Third batch choice out of range" )
      }
    } else if ( selection.second.level == "3" ){
      ##
      ## Grid data throughout Germany - monthly
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tData source:\n' )
        cat( '\t\t1) Project TRY\n' )
        cat( '\t\t2) Drought index\n' )
        cat( '\t\t3) Potential evapotranspiration over gras\n' )
        cat( '\t\t4) Actual evapotranspiration over gras and sandy loam\n' )
        cat( '\t\t5) Frost depth of uncovered soil at midday\n' )
        cat( '\t\t6) Precipitation (REGNIE grid)\n' )
        cat( '\t\t7) Soil moisture at 60cm depth under gras and sandy loam\n' )
        cat( '\t\t8) Soil temperature at 5cm depth\n' )
        cat( '\t\t9) Maximum air temperature\n' )
        cat( '\t\t10) Minimal air temperature\n' )
        cat( '\t\t11) Mean air temperature\n' )
        cat( '\t\t12) Precipitation\n' )
        cat( '\t\t13) Radiation (diffuse)\n' )
        cat( '\t\t14) Radiation (direct)\n' )
        cat( '\t\t15) Radiation (global)\n' )
        cat( '\t\t16) Sunshine duration\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "Project_TRY/",
                                "2" = "drought_index/",
                                "3" = "evapo_p/",
                                "4" = "evapo_r/",
                                "5" = "frost_depth/",
                                "6" = "regnie/",
                                "7" = "soil_moist/",
                                "8" = "soil_temperature_5cm/",
                                "9" = "air_temperature_max/",
                                "10" = "air_temperature_min/",
                                "11" = "air_temperature_mean/",
                                "12" = "precipitation/",
                                "13" = "radiation_diffuse/",
                                "14" = "radiation_direct/",
                                "15" = "radiation_global/",
                                "16" = "sunshine_duration/",
                                "Third batch choice out of range" )
      
      ## Sanity check
      dwd2r.url.check( paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level) )
      if ( selection.third.level == "1" ){
        ##
        ## Grid data throughout Germany - monthly
        ## Project TRY
        ##
        if ( !is.null( batch.choices ) ){
          selection.fourth.level <- as.character( batch.choices[ 4 ] )
        } else {
          cat( '\n\n\tMeasurement variable:\n' )
          cat( '\t\t1) Mean air temperature\n' )
          cat( '\t\t2) Cloud coverage\n' )
          cat( '\t\t3) Dew point\n' )
          cat( '\t\t4) Humidity\n' )
          cat( '\t\t5) Pressure\n' )
          cat( '\t\t6) Radiation (direct)\n' )
          cat( '\t\t7) Radiation (downwelling)\n' )
          cat( '\t\t8) Radiation (global)\n' )
          cat( '\t\t9) Radiation (upwelling)\n' )
          cat( '\t\t10) Vapour pressure\n' )
          cat( '\t\t11) Wind direction\n' )
          cat( '\t\t12) Wind speed\n' )
          selection.fourth.level <- readline( "Selection: " )
        }

        url.fourth.level <- switch(
            selection.fourth.level,
            "1" = "air_temperature_mean/",
            "2" = "cloud_cover/",
            "3" = "dew_point/",
            "4" = "humidity/",
            "5" = "pressure/",
            "6" = "radiation_direct/",
            "7" = "radiation_downwelling/",
            "8" = "radiation_global/",
            "9" = "radiation_upwelling/",
            "10" = "vapour_pressure/",
            "11" = "wind_direction/",
            "12" = "wind_speed/",
            "Fourth batch choice out of range" )
        
        list.url.final <-
          list( data = paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level,
                              url.fourth.level ),
               meta = NULL )
      } else if ( selection.third.level %in%
                 c( "2", "9", "10", "11", "12", "16" ) ){
        ##
        ## Grid data throughout Germany - monthly
        ## maximal/minimal/mean air temperature, precipitation,
        ## sunshine duration, drought index
        ##
        ## These folder contain different directories for each month,
        ## while the top level directory contains the description
        ## files.
        list.url.final <-
          list(
              data = paste0(
                  paste0( url.root, url.top.level,
                         url.second.level,
                         url.third.level ),
                  c( '', '01_Jan/', '02_Feb/', '03_Mar/',
                    '04_Apr/', '05_May/', '06_Jun/',
                    '07_Jul/', '08_Aug/', '09_Sep/',
                    '10_Oct/', '11_Nov/', '12_Dec/' ) ),
              meta = paste0( url.root, url.top.level,
                            url.second.level, url.third.level ) )
      } else if ( selection.third.level %in%
                 c( "3","4", "5", "6", "7", "8", "13", "14",
                   "15" ) ) {
        ##
        ## Grid data throughout Germany - hourly
        ## other quantities
        list.url.final <-
          list( data = paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level ),
               meta = NULL )
      } else {
        stop( "Third batch choice out of range" )
      }
    } else if ( selection.second.level == "4" ){
      ##
      ## Grid data throughout Germany - seasonal
      ##
      ## These folder contain different directories for each month,
      ## while the top level directory contains the description
      ## files.
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tData source:\n' )
        cat( '\t\t1) Drought index\n' )
        cat( '\t\t2) Maximum air temperature\n' )
        cat( '\t\t3) Minimal air temperature\n' )
        cat( '\t\t4) Mean air temperature\n' )
        cat( '\t\t5) Precipitation\n' )
        cat( '\t\t6) Sunshine duration\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "drought_index/",
                                "2" = "air_temperature_max/",
                                "3" = "air_temperature_min/",
                                "4" = "air_temperature_mean/",
                                "5" = "precipitation/",
                                "6" = "sunshine_duration/",
                                "Third batch choice out of range" )
      list.url.final <-
        list(
            data = paste0(
                paste0( url.root, url.top.level,
                       url.second.level,
                       url.third.level ),
                c( '', '13_MAM/', '14_JJA/', '15_SON/',
                  '16_DJF/' ) ),
            meta = paste0( url.root, url.top.level,
                          url.second.level, url.third.level ) )
    } else if ( selection.second.level == "5" ){
      ##
      ## Grid data throughout Germany - half of a year
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\nPrecipitation data found.' )
        cat( '\n\n\tChoose measurement period:\n' )
        cat( '\t\t1) November till April\n' )
        cat( '\t\t2) May till October\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "18_NDJFMA/",
                                "2" = "19_MJJASO/",
                                "Third batch choice out of range" )
      list.url.final <-
        list(
            data = paste0( url.root, url.top.level,
                          url.second.level,
                          url.third.level ),
            meta = NULL )
    } else if ( selection.second.level == "6" ){
      ##
      ## Grid data throughout Germany - annual
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tData source:\n' )
        cat( '\t\t1) Phenology\n' )
        cat( '\t\t2) Drought index\n' )
        cat( '\t\t3) Number of days with at least 10mm of precipitation\n' )
        cat( '\t\t4) Number of days with at least 20mm of precipitation\n' )
        cat( '\t\t5) Number of days with at least 30mm of precipitation\n' )
        cat( '\t\t6) Precipitation (REGNIE grid)\n' )
        cat( '\t\t7) Number of days with snow cover (Snow depth >=1cm at 7UTC)\n' )
        cat( '\t\t8) Number of days with summer (max. air temperature >=25\u00B0C)\n' )
        cat( '\t\t9) Maximum air temperature\n' )
        cat( '\t\t10) Minimal air temperature\n' )
        cat( '\t\t11) Mean air temperature\n' )
        cat( '\t\t12) Precipitation\n' )
        cat( '\t\t13) Radiation (diffuse)\n' )
        cat( '\t\t14) Radiation (direct)\n' )
        cat( '\t\t15) Radiation (global)\n' )
        cat( '\t\t16) Sunshine duration\n' )
        cat( '\t\t17) Number of days with hot (max. air temperature >=30\u00B0C)\n' )
        cat( '\t\t18) Number of days with ice (max. air temperature < 0\u00B0C)\n' )
        cat( '\t\t19) Number of days with frost (min. air temperature < 0\u00B0C)\n' )
        cat( '\t\t20) Beginning of the vegetation\n' )
        cat( '\t\t21) End of the vegetation\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "phenology/",
                                "2" = "drought_index/",
                                "3" = "precipGE10mm_days/",
                                "4" = "precipGE20mm_days/",
                                "5" = "precipGE30mm_days/",
                                "6" = "regnie/",
                                "7" = "snowcover_days/",
                                "8" = "summer_days/",
                                "9" = "air_temperature_max/",
                                "10" = "air_temperature_min/",
                                "11" = "air_temperature_mean/",
                                "12" = "precipitation/",
                                "13" = "radiation_diffuse/",
                                "14" = "radiation_direct/",
                                "15" = "radiation_global/",
                                "16" = "sunshine_duration/",
                                "17" = "hot_days",
                                "18" = "ice_days",
                                "19" = "frost_days",
                                "20" = "vegetation_begin",
                                "21" = "vegetation_end",
                                "Third batch choice out of range" )
      
      ## Sanity check
      dwd2r.url.check( paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level ) )
      if ( selection.third.level == "1" ){
        warning( paste(
            "The import the annual phenology is not supported yet. It consists of data for numerous fruits and stuff. If you wish to use this data, check out the FTP server",
            paste0( url.root, url.top.level,
                   url.second.level,
                   url.third.level, ',' ),
            "implement it yourself (preferred) or feel free to contact the package's maintainer." ) )
      } else if ( selection.third.level %in%
                 as.character( seq( 2, 21 ) ) ){
        list.url.final <-
          list(
              data = paste0( url.root, url.top.level,
                            url.second.level,
                            url.third.level ),
              meta = NULL )
      } else {
        stop( "Third batch choice out of range" )
      }
    } else if ( selection.second.level == "7" ){
      ##
      ## Grid data throughout Germany - multi annual
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tData source:\n' )
        cat( '\t\t1) Annual sum of incoming shortwave radiation\n' )
        cat( '\t\t2) Drought index\n' )
        cat( '\t\t3) Number of days with at least 10mm of precipitation\n' )
        cat( '\t\t4) Number of days with at least 20mm of precipitation\n' )
        cat( '\t\t5) Number of days with at least 30mm of precipitation\n' )
        cat( '\t\t6) Precipitation (REGNIE grid)\n' )
        cat( '\t\t7) Number of days with snow cover (Snow depth >=1cm at 7UTC)\n' )
        cat( '\t\t8) Number of days with summer (max. air temperature >=25\u00B0C)\n' )
        cat( '\t\t9) Maximum air temperature\n' )
        cat( '\t\t10) Minimal air temperature\n' )
        cat( '\t\t11) Mean air temperature\n' )
        cat( '\t\t12) Precipitation\n' )
        cat( '\t\t13) Soil moisture at 60cm depth under gras and sandy loam\n' )
        cat( '\t\t14) Soil temperature at 5cm depth\n' )
        cat( '\t\t15) Radiation (global)\n' )
        cat( '\t\t16) Sunshine duration\n' )
        cat( '\t\t17) Number of days with hot (max. air temperature >=30\u00B0C)\n' )
        cat( '\t\t18) Number of days with ice (max. air temperature < 0\u00B0C)\n' )
        cat( '\t\t19) Number of days with frost (min. air temperature < 0\u00B0C)\n' )
        cat( '\t\t20) Beginning of the vegetation\n' )
        cat( '\t\t21) End of the vegetation\n' )
        cat( '\t\t22) Potential evapotranspiration over gras\n' )
        cat( '\t\t23) Actual evapotranspiration over gras and sandy loam\n' )
        cat( '\t\t24) Water balance\n' )
        cat( '\t\t25) Mean annual wind speeds from 10m to 100m and Weibull parameters\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "solar/",
                                "2" = "drought_index/",
                                "3" = "precipGE10mm_days/",
                                "4" = "precipGE20mm_days/",
                                "5" = "precipGE30mm_days/",
                                "6" = "regnie/",
                                "7" = "snowcover_days/",
                                "8" = "summer_days/",
                                "9" = "air_temperature_max/",
                                "10" = "air_temperature_min/",
                                "11" = "air_temperature_mean/",
                                "12" = "precipitation/",
                                "13" = "soil_moist/",
                                "14" = "soil_temperature_5cm/",
                                "15" = "radiation_global/",
                                "16" = "sunshine_duration/",
                                "17" = "hot_days/",
                                "18" = "ice_days/",
                                "19" = "frost_days/",
                                "20" = "vegetation_begin/",
                                "21" = "vegetation_end/",
                                "22" = "evapo_p/",
                                "23" = "evapo_r/",
                                "24" = "water_balance/",
                                "25" = "wind_parameters/",
                                "Third batch choice out of range" )
      
      ## Sanity check
      dwd2r.url.check( paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level ) )
      if ( selection.third.level %in% c( "11", "12", "16" ) ){
        ##
        ## Grid data throughout Germany - multi annual
        ## mean air temperature, precipitation, sunshine duration
        ##
        list.url.final <-
          list(
              data = paste0( paste0( url.root, url.top.level,
                                    url.second.level,
                                    url.third.level ),
                            c( '6190', '7100', '8110' ) ),
              meta = NULL )
      } else if ( selection.third.level == "25" ){
        ##
        ## Grid data throughout Germany - multi annual
        ## wind parameters
        ##
        list.url.final <-
          list(
              data = paste0( paste0( url.root, url.top.level,
                                    url.second.level,
                                    url.third.level ),
                            c( 'resol_1000x1000',
                              'resol_200x200' ) ),
              meta = NULL )
      } else if ( selection.third.level %in%
                 c( "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                   "13", "14", "15", "17", "18", "19", "20", "21",
                   "22", "23", "24" ) ){
        ##
        ## Grid data throughout Germany - multi annual
        ## other parameters
        ##
        list.url.final <-
          list(
              data = paste0( url.root, url.top.level,
                            url.second.level,
                            url.third.level ),
              meta = NULL )
      } else {
        stop( "Third batch choice out of range" )
      }
    } else if ( selection.second.level == "8" ){
      ##
      ## Grid data throughout Germany - return periods
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\nKOSTRA precipitation data found.' )
        cat( '\n\n\tChoose format:\n' )
        cat( '\t\t1) ASCII\n' )
        cat( '\t\t2) GIS\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "asc/",
                                "2" = "gis/",
                                "Third batch choice out of range" )
      list.url.final <-
        list(
            data = paste0( url.root, url.top.level,
                          url.second.level,
                          'precipitation/KOSTRA/KOSTRA_DWD_2010R/',
                          url.third.level ),
            meta = NULL )
    } else {
      stop( "Second batch choice out of range" )
    }
  } else if ( selection.top.level == "4" ){
    ##
    ## Grid data throughout Europe
    ##
    if ( !is.null( batch.choices ) ){
      selection.second.level <- as.character( batch.choices[ 2 ] )
    } else {
      cat( '\n\n\tMeasurement intervals:\n' )
      cat( '\t\t1) Daily\n' )
      cat( '\t\t2) Monthly\n' )
      selection.second.level <- readline( "Selection: " )
    }
    url.second.level <- switch( selection.second.level,
                               "1" = "daily/",
                               "2" = "monthly/",
                               "Second batch choice out of range" )
    
    ## Sanity check
    dwd2r.url.check( paste0( url.root, url.top.level,
                            url.second.level ) )
    if ( selection.second.level == "1" ){
      ##
      ## Grid data throughout Europe - daily
      ##
      ## For each data set there is a version v001 and v002. But since
      ## the description states there is a bug in the netCDF header of
      ## version1, I will just provide version v002.
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tMeasurement variable:\n' )
        cat( '\t\t1) Maximum air temperature\n' )
        cat( '\t\t2) Minimal air temperature\n' )
        cat( '\t\t3) Mean air temperature\n' )
        cat( '\t\t4) Daily mean near-surface (10m) wind speed\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "air_temperature_max/",
                                "2" = "air_temperature_min/",
                                "3" = "air_temperature_mean/",
                                "4" = "wind/",
                                "Third batch choice out of range" )
      list.url.final <-
        list(
            data = paste0(
                paste0( url.root, url.top.level,
                       url.second.level,
                       url.third.level,
                       'MIKLIP_DECREG/v002/' ),
                c( '', '01_Jan/', '02_Feb/', '03_Mar/',
                  '04_Apr/', '05_May/', '06_Jun/',
                  '07_Jul/', '08_Aug/', '09_Sep/',
                  '10_Oct/', '11_Nov/', '12_Dec/' ) ),
            meta = paste0( url.root, url.top.level,
                          url.second.level, url.third.level,
                          'MIKLIP_DECREG/v002/' ) )
    } else if ( selection.second.level == "2" ){
      ##
      ## Grid data throughout Europe - monthly
      ##
      ## For each data set there is a version v001 and v002. But since
      ## the description states there is a bug in the netCDF header of
      ## version1, I will just provide version v002.
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tData source:\n' )
        cat( '\t\t1) Maximum air temperature\n' )
        cat( '\t\t2) Minimal air temperature\n' )
        cat( '\t\t3) Mean air temperature\n' )
        cat( '\t\t4) Daily mean near-surface (10m) wind speed\n' )
        cat( '\t\t5) Mean cloud cover\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "air_temperature_max/",
                                "2" = "air_temperature_min/",
                                "3" = "air_temperature_mean/",
                                "4" = "wind/",
                                "5" = "cloud_cover/",
                                "Third batch choice out of range" )
      if ( selection.third.level %in% c( "1", "2", "3", "4" ) ){ 
        ##
        ## Grid data throughout Europe - monthly
        ## max/min/mean air temperature, wind speed
        ##
        list.url.final <-
          list(
              data = paste0( url.root, url.top.level,
                            url.second.level,
                            url.third.level,
                            'MIKLIP_DECREG/v002/' ),
              meta = NULL )
      } else if ( selection.third.level == "5" ){
        ##
        ## Grid data throughout Europe - monthly
        ## cloud cover
        ##
        list.url.final <-
          list(
              data = paste0( url.root, url.top.level,
                            url.second.level,
                            url.third.level, 'SEVIRI/' ),
              meta = NULL )
      } else {
        stop( "Third batch choice out of range" )
      }
    } else {
      stop( "Second batch choice out of range" )
    }
  } else if ( selection.top.level == "5" ){
    ##
    ## Derived data from Germany
    ##
    if ( !is.null( batch.choices ) ){
      selection.second.level <- as.character( batch.choices[ 2 ] )
    } else {
      cat( '\n\n\tMeasurement variable:\n' )
      cat( '\t\t1) Different characteristic elements of soil and crops\n' )
      cat( '\t\t2) Number of degree days\n' )
      selection.second.level <- readline( "Selection: " )
    }
    url.second.level <- switch( selection.second.level,
                               "1" = "soil/",
                               "2" = "techn/",
                               "Second batch choice out of range" )
    ## Sanity check
    dwd2r.url.check( paste0( url.root, url.top.level,
                            url.second.level ) )
    if ( selection.second.level == "1" ){
      ##
      ## Derived data from Germany - soil
      ##
      if ( !is.null( batch.choices ) ){
        selection.third.level <- as.character( batch.choices[ 3 ] )
      } else {
        cat( '\n\n\tMeasurement intervals:\n' )
        cat( '\t\t1) Daily\n' )
        cat( '\t\t2) Monthly\n' )
        cat( '\t\t3) Multi annual\n' )
        selection.third.level <- readline( "Selection: " )
      }
      url.third.level <- switch( selection.third.level,
                                "1" = "daily/",
                                "2" = "monthly/",
                                "3" = "multi_annual/",
                                "Third batch choice out of range" )
      if ( selection.third.level %in% c( "1", "2" ) ){
        ##
        ## Derived data from Germany - soil - daily & monthly
        ##
        list.url.final <-
          list( data = paste0(
                    paste0( url.root, url.top.level,
                           url.second.level,
                           url.third.level,
                           c( 'recent/', 'historical/' ) ) ),
               meta = NULL )
      } else if ( selection.third.level == "3" ){
        ##
        ## Derived data from Germany - soil - multi annual
        ##
        list.url.final <-
          list( data = paste0( url.root, url.top.level,
                              url.second.level,
                              url.third.level, 'mean_91-10/' ),
               meta = NULL )
      } else {
        stop( "Third batch choice out of range" )
      }
    } else if ( selection.second.level == "2" ){
      ##
      ## Derived data from Germany - degree days
      ##
      list.url.final <-
        list( data = paste0(
                  paste0( url.root, url.top.level,
                         url.second.level,
                         'monthly/heating_degreedays/hdd_3807/',
                         c( 'recent/', 'historical/' ) ) ),
             meta = NULL )
    } else {
      stop( "Second batch choice out of range" )
    }
  } else if ( selection.top.level == "6" ){
    ##
    ## Regional average throughout Germany
    ##
    if ( !is.null( batch.choices ) ){
      selection.second.level <- as.character( batch.choices[ 2 ] )
    } else {
      cat( '\n\n\tMeasurement intervals:\n' )
      cat( '\t\t1) Monthly\n' )
      cat( '\t\t2) Seasonal\n' )
      cat( '\t\t3) Annual\n' )
      selection.second.level <- readline( "Selection: " )
    }
    url.second.level <- switch( selection.second.level,
                               "1" = "monthly/",
                               "2" = "seasonal/",
                               "3" = "annual/",
                               "Second batch choice out of range" )
    
    ## Sanity check
    dwd2r.url.check( paste0( url.root, url.top.level,
                            url.second.level ) )

    ## Fortunately they all feature the same folder structure.
    if ( !is.null( batch.choices ) ){
      selection.third.level <- as.character( batch.choices[ 4 ] )
    } else {
      cat( '\n\n\tMeasurement variable:\n' )
      cat( '\t\t1) Mean air temperature\n' )
      cat( '\t\t2) Precipitation\n' )
      cat( '\t\t3) Sunshine duration\n' )
      selection.third.level <- readline( "Selection: " )
    }
    url.third.level <- switch( selection.third.level,
                              "1" = "air_temperature_mean/",
                              "2" = "precipitation/",
                              "3" = "sunshine_duration/",
                              "Third batch choice out of range" )
    list.url.final <-
      list(
          data = paste0( url.root, url.top.level,
                        url.second.level,
                        url.third.level ),
          meta = NULL )
  } else {
    stop( "First batch choice out of range" )
  }

  ## A last sanity check on the extracted links.
  if ( !is.null( list.url.final$data ) ){
    dwd2r.url.check( list.url.final$data )
  }
  if ( !is.null( list.url.final$meta ) ){
    dwd2r.url.check( list.url.final$meta )
  }
  return( list.url.final )
}

##' @title Checking the validity of URLs
##' @description This helper functions tests whether URLs are
##'   reachable and throws an error if this is not the case.
##'
##' @param url Either a simple string or a character vector of several
##'   URLs.
##'
##' @importFrom RCurl url.exists
##'
##' @return invisible( TRUE ) if all URLs exist. Throws an error if
##'   not. 
##' @author Philipp Mueller
dwd2r.url.check <- function( url ){
  ## Test the URL collected so far.
  for ( uu in 1 : length( url ) ){
    if ( !RCurl::url.exists( url[ uu ] ) ){
      stop( paste( "The URL", url[ uu ],
                  "does not exist on the FTP server of the DWD." ) )
    }
    ## Avoid being detect as a bot
    Sys.sleep( .01 )
  }
  invisible( TRUE )
}
## End of url-retrieval.R
