##' @title Downloads data of the DWD
##' @description Downloads daily weather data from observation
##'   stations in Germany and extracts minimum and maximum temperature
##'   as well as precipitation data.
##'
##' @details The download will be done using 'wget'. Per default the
##'   dwd2r.download.path variable from the \code{getOption(
##'   "dwd2r.download.path" )} will be used to set the download
##'   path. Since this function will check the files already present
##'   it's strongly recommended to use the save.downloads
##'   options. Whenever this function is invoked again only updated
##'   files will be downloaded which saves a lot of traffic and
##'   time. The csv.export option can be used to export the time
##'   series into a data type file making it available outside of R
##'   too. In addition, the geographic positions of the individual
##'   stations will be extracted and saved as well. They are needed
##'   for the leaflet module of the climex shiny app. CAUTION: since
##'   this procedure takes a while its run in parallel on all cores of
##'   your machine!
##' @param save.downloads If TRUE the downloaded .zip files are stored
##'   in download.folder/downloads_dwd. Else they will be deleted
##'   after the extracting. Default = TRUE.
##' @param url If the user wants to avoid the guided selection.
##' @param csv.export If TRUE creates an additional folder containing
##'   .csv files with the individual station data. Using this the data
##'   can be used outside of R too. Default = FALSE.
##' @param data.type Specifies which kind of information from the
##'   downloaded files should be extracted. This input can be a
##'   character vector The options are: temp.max, temp.min, prec,
##'   default (for both the daily maximum and minimum temperature and
##'   the precipitation), temp.mean, vapor.pressure, cloud.amount,
##'   air.pressure, quality (-999 = faulty or suspicious, 1 =
##'   superficially checked, 2 = checked according to individual
##'   criterion, 3 = old automated check and correction, 5 =
##'   historical and subjective check, 7 = additional second check but
##'   without correction, 8 = quality check outside of the routine (?,
##'   just quoting the manual), 9 = not all parameters are corrected,
##'   10 = fully checked and corrected), relative.humidity,
##'   wind.speed, temp.min.at.ground, wind.speed.peak, prec.type (0 =
##'   no precipitation, 1 = only rain (before 1979), 2 = unknown, 4 =
##'   only rain (after 1979), 7 = only snow, 8 = snow or rain),
##'   sunshine.duration, snow.height. Default = default.
##' @param download.folder Specifies the data will be stored and
##'   downloaded too. It is advised to store it in the path stored in
##'   the \code{options( "dwd2r.download.path" )}, which is also used
##'   for importing the saved data. You can overwrite its default
##'   value of "~/R/dwd_data/" by adding \code{options(
##'   dwd2r.download.path = "PATH" )} to your .Rprofile path in your
##'   home.
##' @param batch.choices Numerical vector containing the numbers,
##'   which corresponds to the choices in the interactive mode. If
##'   NULL, the choices will be done interactively. Default = NULL.
##' @param quiet Whether or not to display the output generated when
##'   downloading the content. Default = FALSE.
##' 
##' @export
##'
##' @examples
##' ## Downloading the aggregated collection of the daily measured
##' ## climatic data of the DWD into a local folder, create .csv files
##' ## from all the time series, and store the files (to be able to do
##' ## a diff at a latter point in time).
##' download.data.dwd( save.downloads = TRUE, csv.export = TRUE,
##'                    download.folder = "./local/folder",
##'                    batch.choices  = c( 1, 1, 5, 1 ) )
##' 
##' @importFrom xts xts
##' 
##' @return invisible setwd()
##' 
##' @author Philipp Mueller 
download.data.dwd <- function( save.downloads = TRUE,
                              csv.export = FALSE,
                              url = NULL,
                              data.type = c( "default", "temp.max",
                                            "temp.min", "prec",
                                            "temp.mean",
                                            "vapor.pressure",
                                            "cloud.amount",
                                            "air.pressure",
                                            "relative.humidity",
                                            "wind.speed",
                                            "temp.min.at.ground",
                                            "wind.speed.peak",
                                            "prec.type", "quality",
                                            "sunshine.duration",
                                            "snow.height" ),
                              download.folder = NULL,
                              batch.choices = NULL,
                              quiet = FALSE ){
  ## The folder to put all the temporary files of the dwd2r package in
  ## is set in the options(). To modify it, overwrite the options(
  ## dwd2r.download.path ) in the .Rprofile file in your home
  ## directory
  if ( is.null( download.folder ) ){
    download.folder <- getOption( "dwd2r.download.path" )
  }
  old.dir <- getwd()
  ## If the folder does not exists yet, create it.
  if ( !dir.exists( download.folder ) ){
    dir.create( download.folder, recursive = TRUE )
  }
  setwd( download.folder )

  ## Retrieve the download URLs, extract the URLs to the individual
  ## files, and download them.
  ## paths on the DWD servers
  ## url.recent <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/"
  ## url.historical <-
  ## "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/"
  if ( is.null( url ) ){
    url.full.content <- get.dwd.ftp.url( batch.choices = batch.choices )
  }
  if ( !is.null( url.full.content$meta ) ){
    warning(
        "Data sources containing meta information are not supported yet!" )
  }
  ## URLs pointing to the actual data.
  url <- url.full.content$data
  ## Download the files. The subfolders variable will contain the
  ## subfolders of download.folder, which contain the downloaded
  ## data.
  cat( 'Download data from\n' )
  lapply( url, function( uu ) cat( paste0( '- ',uu, '\n' ) ) )
  subfolders <- download.content(
      url = url, download.folder = download.folder, quiet = quiet )

  ## Get a list of all files, which were part of the download.
  ## In the classical, aggregated way of providing the data the DWD
  ## splits the time series and provides all data more recent than
  ## approximately one year in the *recent* folder and everything else
  ## in the *historical* folder. These ones are treated separately.
  if ( length( grep( "recent", subfolders ) ) != 0 ){
    indices.recent <- grep( "recent", subfolders )
    files.recent <- list.files( paste0(
        download.folder, subfolders[ indices.recent ] ) )
  } else {
    files.recent <- indices.recent <- NULL
  }
  if ( length( grep( "historical", subfolders ) ) != 0 ){
    indices.historical <- grep( "historical", subfolders )
    files.historical <- list.files( paste0(
        download.folder, subfolders[ indices.historical ] ) )
  } else {
    files.historical <- indices.historical <- NULL
  }
  ## Get all other files
  if ( length( subfolders ) > ( length( indices.recent ) +
                                length( indices.historical ) ) ){
    ## The recent and historical ones do not cover all choices
    message( "not just recent and historical" )
    files.diverse <- list.files( paste0(
        download.folder, subfolders[ -c( indices.recent,
                                        indices.historical ) ] ) )
  } else {
    files.diverse <- NULL
  }

  
  ## setting the column numbers for extraction based on the data.type
  ## input
  if ( missing( data.type ) )
    data.type <- "default"
  data.type <- match.arg( data.type )
  if ( data.type == "default" )
    data.type <- c( "temp.max", "temp.min", "prec" )
  data.columns <- numeric()
  if ( "quality" %in% data.type )
    data.columns <- c( data.columns, 6 )
  if ( "temp.mean" %in% data.type )
    data.columns <- c( data.columns, 14 )
  if ( "vapor.pressure" %in% data.type )
    data.columns <- c( data.columns, 12 )
  if ( "cloud.coverage" %in% data.type )
    data.columns <- c( data.columns, 11 )
  if ( "air.pressure" %in% data.type )
    data.columns <- c( data.columns, 13 )
  if ( "relative.humidity" %in% data.type )
    data.columns <- c( data.columns, 15 )
  if ( "wind.speed" %in% data.type )
    data.columns <- c( data.columns, 5 )
  if ( "temp.max" %in% data.type )
    data.columns <- c( data.columns, 16 )
  if ( "temp.min" %in% data.type )
    data.columns <- c( data.columns, 17 )
  if ( "temp.min.at.ground" %in% data.type )
    data.columns <- c( data.columns, 18 )
  if ( "wind.speed.peak" %in% data.type )
    data.columns <- c( data.columns, 4 )
  if ( "prec" %in% data.type )
    data.columns <- c( data.columns, 7 )
  if ( "prec.type" %in% data.type )
    data.columns <- c( data.columns, 8 )
  if ( "sunshine.duration" %in% data.type )
    data.columns <- c( data.columns, 9 )
  if ( "snow.height" %in% data.type )
    data.columns <- c( data.columns, 10 )

  ## Always download the latest description file. Else the algorithm
  ## would fail importing recently added stations.
  if ( !is.null( files.recent ) ){
    file.description.recent <-
      grep( "Beschreibung", files.recent, value = TRUE )
    if ( length( file.description.recent ) > 1 ){
      warning(
          "More than one recent 'Beschreibung' file found. Algorithm will misbehave!" )
    }
    if ( length( indices.recent ) ){
      warning(
          "More than one folder containing the recent measurements. The algorithm will not handle this properly!" )
    }
    utils::download.file(
               url = paste0( url[ indices.recent ],
                            file.description.recent ),
               destfile = paste0( subfolders[ indices.recent ],
                                 file.description.recent ),
               method = "wget", quiet = quiet )
  } else if ( !is.null( files.diverse ) ){
    file.description.diverse <-
      grep( "Beschreibung", files.diverse, value = TRUE )
    if ( length( file.description.diverse ) > 1 ){
      warning(
          "More than one diverse 'Beschreibung' file found. Algorithm will misbehave!" )
    }
    if ( length( indices.recent ) ){
      warning(
          "More than one folder containing the diverse measurements. The algorithm will not handle this properly!" )
    }
    utils::download.file(
               url = paste0( url[ indices.diverse ],
                            file.description.diverse ),
               destfile = paste0( subfolder[ indices.diverse ],
                                 file.description.diverse ),
               method = "wget", quiet = quiet )
  } else {
    warning(
        "There are no recent or other files. Something probably went wrong" )
  }

  ## This will download quite a number of .zip files
  download.content( url.recent, files.recent )
  setwd( "../historical" )
  ## Always download the latest description file. Else the algorithm
  ## would fail importing recently added stations.
  file.description.historical <- grep( "Beschreibung",
                                      files.historical[[ 1 ]],
                                      value = TRUE )
  utils::download.file( url = paste0( url.historical,
                                     file.description.historical ),
                       destfile = file.description.historical,
                       method = "wget" )
  download.content( url.historical, files.historical )
  setwd( "../" )
  ## file containing the discription of the station data
  file.description.recent.raw <- utils::read.table( (
    paste0( "./recent/",
           list.files( "./recent/")[ grep( "Beschreibung",
                                          list.files( "./recent/" ) )
                                    ] ) ),
    header = FALSE, sep = "\t", stringsAsFactors = FALSE,
    encoding = "UTF-8", skip = 2 )
  ## split it into a list to make efficient use of the grep command
  file.description.recent <-
    split( file.description.recent.raw,
          seq( nrow( file.description.recent.raw ) ) )
  file.description.historical.raw <- utils::read.table( (
    paste0( "./historical/",
           list.files( "./historical/")[
               grep( "Beschreibung", list.files( "./historical/" ) ) ]
           ) ),
    header = FALSE, sep = "\t", stringsAsFactors = FALSE,
    encoding = "UTF-8", skip = 2 )
  ## split it into a list to make efficient use of the grep command
  file.description.historical <- split(
      file.description.historical.raw,
      seq( nrow( file.description.historical.raw ) ) )
  file.description.aux <- c( file.description.recent,
                            file.description.historical[
                                !( file.description.historical %in%
                                   file.description.recent ) ] )
  ## since its not possible to have ensure the correct encoding while
  ## importing the artifacts have to be replaced by hand
  file.d.1 <- lapply( file.description.aux, function( x )
    gsub( "\xfc", "\uFC", x ) )
  file.d.2 <- lapply( file.d.1, function( x )
    gsub( "\xf6", "\uF6", x ) ) 
  file.d.3 <- lapply( file.d.2, function( x )
    gsub( "\xe4", "\uE4", x ) )  
  file.d.4 <- lapply( file.d.3, function( x )
    gsub( "\xdf", "\uDF", x ) )  
  file.d.5 <- lapply( file.d.4, function( x )
    gsub( "\U3e63643c", "\uDC", x ) )  
  file.description <- lapply( file.d.5, function( x )
    gsub( "\U3e36643c", "\uD6", x ) )      

  browser()
  ## extract a vector of all unique station IDs seen in the .zip files
  list.station.ids <- as.list( unique( c(
      Reduce( c, lapply( list.files( "./recent/" )[
                     grep( ".zip", list.files( "./recent/" ) ) ],
                     function( x ) strsplit( x, "_" )[[ 1 ]][ 3 ] ) ),
      Reduce( c, lapply( list.files( "./historical/" )[
                     grep( ".zip", list.files( "./historical/" ) ) ],
                     function( x ) strsplit( x, "_" )[[ 1 ]][ 3 ] ) )
  ) ) )
  ## this lists will contain all the final station data
  ## I will only extract the maximum, minimum temperature and the
  ## precipitation but it is easily extendable to all other columns
  stations.temp.max.xts <- stations.temp.min.xts <-
    stations.prec.xts <- list()
  extract.content <- function( station.id, data.column ){
    ## handle each station ID separately and extract all corresponding
    ## .zip files into auxiliary folders 
    flag.recent <- ifelse( length(
        grep( station.id, list.files( "./recent/" ) ) ) != 0,
        TRUE, FALSE )
    flag.historical <- ifelse( length(
        grep( station.id, list.files( "./historical/" ) ) ) != 0,
        TRUE, FALSE )
    ## If the zip file can not be extracted properly the unzip function
    ## will just raise a warning. It has to be converted to an error
    unlink( "./TMPrecent/", recursive = TRUE )
    unlink( "./TMPhistorical/", recursive = TRUE )
    options( warn = 2 )
    if ( flag.recent ){
      try.unzip <- try( utils::unzip( (
        paste0( "./recent/",
               grep( station.id, list.files( "./recent/" ),
                    value = TRUE ) ) ),
        exdir = "./TMPrecent" ), silent = TRUE )
      ## if something goes wrong here just return a placeholder of the
      ## same format
      if ( class( try.unzip ) == "try-error" && !flag.historical )
        return( xts( NA, order.by = lubridate::today() ) )
      if ( class( try.unzip ) == "try-error" )
        flag.recent <- FALSE
    }
    if ( flag.historical ){
      try.unzip <- try( utils::unzip( (
        paste0( "./historical/",
               grep( station.id, list.files( "./historical/" ),
                    value = TRUE ) ) ),
        exdir = "./TMPhistorical/" ), silent = TRUE )
      if ( class( try.unzip ) == "try-error" )
        return( xts( NA, order.by = lubridate::today() ) )
    }
    ## but we don't want potential warning in other packages break our
    ## code
    options( warn = 0 )
    ## get the path to the .txt files containing the information
    if ( flag.recent ){
      recent.file <- paste0( "./TMPrecent/",
                            grep( "produkt",
                                 list.files("TMPrecent/" ),
                                 value = TRUE ) )
      if ( length( grep( "produkt",
                        list.files("TMPrecent/" ),
                        value = TRUE ) ) == 0 )
        return( xts( NA, order.by = lubridate::today() ) )
      ## sometimes an older version is present due to an error
      ## occurring beforehand those have to be removed (at least from
      ## the recent.file variable)
      if ( length( recent.file ) > 1 )
        recent.file <- recent.file[ grep( station.id, recent.file ) ]
    }
    if ( flag.historical ){
      historical.file <- paste0( "./TMPhistorical/",
                                grep( "produkt",
                                     list.files("TMPhistorical/" ),
                                     value = TRUE ) )
      if ( length( grep( "produkt",
                        list.files("TMPhistorical/" ) ) ) == 0 )
        return( xts( NA, order.by = lubridate::today() ) )
      ## sometimes an older version is present due to an error
      ## occurring beforehand those have to be removed (at least from
      ## the historical.file variable)
      if ( length( historical.file ) > 1 )
        historical.file <- historical.file[ grep( station.id,
                                                 historical.file ) ]
    }
    ## data.ii will be a data.frame containing all the recent and
    ## historical data of one station
    if ( flag.recent ){
      ## sometimes there is a single delimiter symbol in the last line.
      ## This causes the read.table function to throw a warning and, if
      ## the file to read just consists of one line, to
      ## fail. Therefore it has to be avoided by checking how many
      ## characters are present in the last line.
      recent.file.read.lines <- readLines( recent.file )
      contains.delimiter.recent <-
        nchar( recent.file.read.lines[
            length( recent.file.read.lines ) ] ) < 10
      if ( contains.delimiter.recent ){
        ## only the last line with the potential delimiter will be
        ## omitted minus two because of the omitted header
        data.ii <-
          utils::read.table(
                     recent.file, header = TRUE, sep = ";",
                     nrows = ( length( recent.file.read.lines ) - 2 ) )
      } else {
        data.ii <- utils::read.table( recent.file, header = TRUE,
                                     sep = ";" )
      }
      if ( flag.historical ){
        hist.file.read.lines <- readLines( historical.file )
        contains.delimiter.historical <-
          nchar( hist.file.read.lines[
              length( hist.file.read.lines ) ] ) < 10
        if ( contains.delimiter.historical ){
          data.hist <-
            utils::read.table(
                       historical.file, header = TRUE, sep = ";",
                       nrows = ( length( hist.file.read.lines ) - 2 ) )
        } else {
          data.hist <- utils::read.table( historical.file,
                                         header = TRUE, sep = ";" )
        }
        ## in most cases some data of the recent observations are also
        ## included in the historical ones. But we of course don't want
        ## any duplicates
        suppressWarnings( data.ii <- rbind( data.hist[
                              -which( data.hist[ , 2 ] %in%
                                      data.ii[ , 2 ] ), ],
                              data.ii ) )
      }
    } else {
      hist.file.read.lines <- readLines( historical.file )
      contains.delimiter.historical <-
        nchar( hist.file.read.lines[
            length( hist.file.read.lines ) ] ) < 10
      if ( contains.delimiter.historical ){
        data.ii <-
          utils::read.table(
                     historical.file, header = TRUE, sep = ";",
                     nrows = ( length( hist.file.read.lines ) - 2 ) )
      } else {
        data.ii <- utils::read.table( historical.file, header = TRUE,
                                     sep = ";" )
      }
    }
    ## delete the auxiliary folders
    unlink( "./TMPrecent/", recursive = TRUE )
    unlink( "./TMPhistorical/", recursive = TRUE )
    ## writing the data into the lists using the xts class
    results.tmp <- xts( data.ii[ , data.column ],
                       order.by = convert.date.integer(
                           data.ii[ , 2 ] ) )
    ## artifacts in the DWD data base are stored as -999
    ## these are converted to NA
    results.tmp[ results.tmp == -999 ] <- NA
    return( results.tmp )
  }
  ## For each data type a separate list containing the corresponding
  ## data of all stations will be generated
  ## This takes some time. But unfortunately it can't be parallelized
  ## easily since all thread read and write to the same folders. So no
  ## "progress bar" either
  for ( dd in 1 : length( data.type ) ){
    print( paste( "parsing", data.type[ dd ], "data..." ) )
    assign( paste0( "stations.", data.type[ dd ] ),
           lapply( list.station.ids, function( x )
             extract.content( x, data.columns[ dd ] ) ) )
  }
  ## assigning the stations names
  extract.station.names <- function( station.id, file.description ){
    ## The station ID is placed in the first column and the date of
    ## the beginning of the observation period is placed in the second
    ## column. With just one line between the first and second column
    ## and the necessity of the second to start with either a 1 or a 2
    ## the station ID can be extracted uniquely.
    ## The conversion to numeric and back is necessary to delete zeros
    line.raw <- grep( paste0( station.id, " [1,2]" ),
                     file.description, value = TRUE )
    ## The extraction of the name fails, because the DWD has not yet
    ## added the station to the Description file holding of the
    ## stations meta data. This can happen (already happened to me).
    if ( length( line.raw ) == 0 ){
      ## Show a warning and recommend downloading the newest data.
      warning( "Some of the station data are not contained in the overall description file. Please make sure you have the most recent data!" )
      ## Worst case, just delete the corresponding file and download
      ## the new one.
      return( list( "none", c( 0, 0, 0 ) ) )
    }
    ## The name can consist of multiple words. In the previous column
    ## there is a digit and in the next the county consisting of one
    ## word (including a minus)
    line.full <- unlist( strsplit( line.raw, " " ) )
    line <- as.character( line.full[ line.full != "" ] )
    line.last.digit <- utils::tail( grep( "[[:digit:]]", line ), 1 )
    line.last.word <- utils::tail( grep( "[[:alpha:]]", line ), 1 )
    station.name <- line[ line.last.digit + 1 ]
    if ( line.last.word - 1 > line.last.digit + 1 ){
      ## the stations name consists of more than one word
      for ( ww in ( line.last.digit + 2 ) : ( line.last.word - 1 ) )
        station.name <- paste( station.name, line[ ww ] )
    }                             
    ## in the 9th, 8th, 7th entries the stations coordinates are
    ## residing
    return( list( station.name,
                 c( as.numeric( line[ line.last.digit ] ),
                   as.numeric( line[ line.last.digit - 1 ] ),
                   as.numeric( line[ line.last.digit - 2 ] ) ) ) )
  }
  station.extracts <- parallel::mclapply( list.station.ids, function( x )
    extract.station.names( x, file.description ),
    mc.cores = parallel::detectCores( logical = FALSE ) )
  station.names <- Reduce( c, lapply( station.extracts,
                                     function( x ) x[[ 1 ]] ) )
  station.positions.aux <- Reduce( rbind, lapply( station.extracts,
                                                 function( x )
                                                   x[[ 2 ]] ) )
  station.positions <- data.frame(
      longitude = station.positions.aux[ , 1 ],
      latitude = station.positions.aux[ , 2 ],
      altitude = station.positions.aux[ , 3 ],
      name = station.names )
  ## Ordering the stations according to their names in alphabetical
  ## order
  station.positions <- station.positions[ order( station.names ), ]
  ## assigning the names of the stations.
  ## this new assignment take in the order of 25ms in total
  for ( ss in paste0( "stations.", data.type ) ){
    ## Get the variable of the string ss, do a operation on it and
    ## reassign it to the same name
    tmp <- get( ss )
    names( tmp ) <- station.names
    ## Ordering the stations according to their names in alphabetical
    ## order
    tmp <- tmp[ order( station.names ) ]
    assign( ss, tmp )
  }
  ## writing the data to dat files
  if ( csv.export ){
    ## creating a distinct folder for all the different data types
    for ( dd in data.type ){
      if ( !dir.exists( paste0( "data_dwd/", dd ) ) )
        dir.create( paste0( "data_dwd/", dd ) )
      data.temp <- get( paste0( "stations.", dd ) )
      for ( ll in 1 : length( data.temp ) )
        utils::write.table( data.frame(
                   date = index( data.temp[[ ll ]] ),
                   value = data.temp[[ ll ]], row.names = NULL ),
                   file = paste0( "data_dwd/", dd, "/",
                                 gsub( "/", "-",
                                      names( data.temp )[ ll ],
                                      fixed = TRUE ), ".csv" ),
                   sep = ",", row.names = FALSE )
    }
  }   
  ## delete all folders
  if ( !save.downloads ){
    unlink( "./recent/", recursive = TRUE )
    unlink( "./historical/", recursive = TRUE )
  }
  data.name <- data.type
  ## restore the input value of the selected data types
  if ( all( data.name == c( "temp.max", "temp.min", "prec" ) ) )
    data.name <- "default"
  ## save the extracted data
  save( list = c( paste0( "stations.", data.type ),
                  "station.positions" ),
       file = paste0( "./dwd_",
                     gsub( ".", "-", paste( data.name, 
                                           collapse = "_" ),
                          fixed = TRUE ),
                     ".RData" ) )
  setwd( old.dir )
  invisible()
}

##' @title Load a data file into R
##' @description Searches the \emph{~/R/dwd_data/} directory or a
##'   specified folder for .RData files recursively and displays the
##'   user its findings for her to choose one of them.
##' @details In order to use the data with the \pkg{climex} package,
##'   it should be of class \pkg{xts} or of lists of class \pkg{xts}
##'   objects.
##'
##'   You can use the \pkg{dwd2r} package to download and use the
##'   daily station data provided by the German weather service.
##'
##' @param download.folder Specifies the folder in which the function
##'   will look for .RData files recursively. Per default the
##'   \emph{R/dwd_data/} directory in your home folder will be
##'   used. You can overwrite this behavior by setting \code{options(
##'   dwd2r.download.path = "PATH" )} in your \emph{.Rprofile} path in
##'   your home.
##' @param envir Environment the data will be attached to. If not
##'   specified, the data will be loaded to the environment the
##'   function is called from. Default = NULL.
##' @family import
##'  
##' @export
##' @return Returns invisible but attaches the chosen .RData file to
##'   the specified R environment.
##' @author Philipp Mueller
source.data <- function( download.folder = NULL, envir = NULL ){
  ## The folder to put all the temporary files of the dwd2r
  ## package in is set in the options(). To modify it,
  ## overwrite the options( dwd2r.download.path ) in the .Rprofile
  ## file in your home directory
  if ( is.null( download.folder ) ){
    download.folder <- getOption( "dwd2r.download.path" )
  }

  ## Extract all .RData objects contained in the download path.
  data.path <- list.files( download.folder, pattern = ".RData",
                          recursive = TRUE )
  ## Obtain the size of the file in MB
  data.size <- rep( NA, length( data.path ) )
  for ( dd in 1 : length( data.path ) ){
    data.size[ dd ] <- file.size(
        paste0( download.folder, data.path[ dd ] ) )/ 1024^2
  }

  ## Print the user a compilation of all found objects.
  cat( '\nImporting data into your R session.\n\n' )
  cat( paste0( '\tData files found the folder ', download.folder,
              ':\n\n' ) )
  cat( '   size:\tpath:\n' )
  for ( dd in 1 : length( data.path ) ){
    if ( data.size[ dd ] < 100 ){
      cat( paste0( dd, '. ', round( data.size[ dd ], digits = 2 ),
                  '\t\t', data.path[ dd ], '\n' ) )
    } else {
      cat( paste0( dd, '. ', round( data.size[ dd ], digits = 2 ),
                  '\t', data.path[ dd ], '\n' ) )
    }
  }
  cat( '\n\n' )
  cat(
      'Please select one file by entering the corresponding number.\n' )
  data.selection <- readline( 'Selection: ' )

  print( paste( "Loading file",
               data.path[ as.numeric( data.selection ) ], "..." ) )

  if ( is.null( envir ) ) {
    load( file = paste0( download.folder,
                        data.path[ as.numeric( data.selection ) ] ),
         envir = parent.frame() )
  } else {
    load( file = paste0( download.folder,
                        data.path[ as.numeric( data.selection ) ] ),
         envir = envir )
  } 
  invisible( )
}
## End of import.R
