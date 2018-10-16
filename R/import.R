##' @title Downloads data of the DWD
##' @description Downloads daily weather data from observation
##'   stations in Germany and extracts minimum and maximum temperature
##'   as well as precipitation data.
##'
##' @description Since this function is agnostic of the type of data
##'   set picked for download and extraction, a prefix to the file
##'   names must be provided using \strong{prefix.file.name}. Else,
##'   the temperatures of e.g. both the hourly and daily data will be
##'   saved in a file called dwd.temperatures and one overrides the
##'   other. Instead, it will be saved into
##'   \emph{dwd.[prefix.file.name].temperatures}.
##'
##'   The .csv files will be stored in
##'   \emph{[download.folder]/csv/[prefix.file.name]} and a separate
##'   folder will be created for each climatological quantity.
##'
##' @param files.list Named list of character vectors containing
##'   absolute paths pointing towards the .zip files downloaded from
##'   the DWD FTP server. The names correspond to \emph{recent},
##'   \emph{historical}, and \emph{diverse}.
##' @param files.description.list Named list of the corresponding
##'   description files. One for each element in \strong{files.list}.
##' @param csv.export If TRUE creates an additional folder containing
##'   .csv files with the individual station data. Using this the data
##'   can be used outside of R too. Default = FALSE.
##' @param download.folder This folder will be used to unpack and
##'   extract the .zip archives in. It does not have to be the same as
##'   the one containing the downloaded content.
##' @param prefix.file.name String, which will prepend to all save
##'   files.
##' @param quiet Whether or not to display the output generated when
##'   downloading the content. Default = FALSE.
##' 
##' @importFrom xts xts
##' 
##' @return invisible( TRUE )
##' 
##' @author Philipp Mueller
conversion.climate <- function( files.list, files.description.list,
                               csv.export = FALSE, download.folder,
                               prefix.file.name, quiet = FALSE ){
  ## Ensure the prefix of the file name is a string and contains only
  ## one slash, which is located at the very end.
  if ( !is.character( prefix.file.name ) ){
    stop( "prefix.file.name has to be a string" )
  }
  if ( length( grep( "/", prefix.file.name ) ) > 2 ||
       ( length( grep( "/", prefix.file.name ) ) == 1 &&
         substr( prefix.file.name, nchar( prefix.file.name ),
                nchar( prefix.file.name ) ) != "/" ) ){
    stop(
        "No '/' delimiter allowed in prefix.file.name except as the very last char" )
  } else if ( substr( prefix.file.name, nchar( prefix.file.name ),
                nchar( prefix.file.name ) ) == "/" ){
    ## If there isn't already a / char present at the end of the
    ## prefix, add one.
    prefix.file.name <- gsub( '/', '', prefix.file.name )
  }
  ## Reading the content of the file containing the
  ## description of the station data.
  if ( !is.null( files.description.list$recent ) ){
    file.description.recent <-
      utils::read.table( files.description.list$recent,
                        header = FALSE, sep = "\t",
                        stringsAsFactors = FALSE,
                        encoding = "UTF-8", skip = 2 )
    ## Split it into a list to make efficient use of the grep command
    file.description.recent <-
      split( file.description.recent,
            seq( nrow( file.description.recent ) ) )
  } else {
    file.description.recent <- NULL
  }
  
  if ( !is.null( files.description.list$historical ) ){
    file.description.historical <-
      utils::read.table( files.description.list$historical,
                        header = FALSE, sep = "\t",
                        stringsAsFactors = FALSE,
                        encoding = "UTF-8", skip = 2 )
    ## Split it into a list to make efficient use of the grep command
    file.description.historical <- split(
        file.description.historical,
        seq( nrow( file.description.historical ) ) )
  } else {
    file.description.historical <- NULL
  }

  if ( !is.null( files.description.list$diverse ) ){
    warning(
        "The other folder structures apart from recent and historical are not supported yet." )
  }

  ## Merge the content of the two description files into one object
  ## and ensure proper encoding.
  file.description <- c( file.description.recent,
                        file.description.historical[
                            !( file.description.historical %in%
                              file.description.recent ) ] )
  file.description <- lapply( file.description, function( x )
    gsub( "\xfc", "\uFC", x ) )
  file.description <- lapply( file.description, function( x )
    gsub( "\xf6", "\uF6", x ) ) 
  file.description <- lapply( file.description, function( x )
    gsub( "\xe4", "\uE4", x ) )  
  file.description <- lapply( file.description, function( x )
    gsub( "\xdf", "\uDF", x ) )  
  file.description <- lapply( file.description, function( x )
    gsub( "\U3e63643c", "\uDC", x ) )  
  file.description <- lapply( file.description, function( x )
    gsub( "\U3e36643c", "\uD6", x ) )      
  
  ## Extract a vector of all unique station IDs seen in the .zip files
  list.station.ids <- as.list( unique( c(
      Reduce( c, lapply( files.list$recent[
                                        grep( ".zip", files.list$recent ) ],
                        function( x )
                          strsplit( x, "_" )[[ 1 ]][
                              length( strsplit( x, "_" )[[ 1 ]] ) - 1 ]
                        ) ),
      Reduce( c, lapply( files.list$historical[
                                        grep( ".zip", files.list$historical ) ],
                        function( x )
                          strsplit( x, "_" )[[ 1 ]][
                              length( strsplit( x, "_" )[[ 1 ]] ) - 3 ]
                        ) ) ) ) )

  ## Ensure the auxiliary folders for the extraction of the content do
  ## exist.
  download.folder.recent <- paste0( download.folder, "tmpRecent/" )
  if ( !dir.exists( download.folder.recent ) ){
    dir.create( download.folder.recent, recursive = TRUE )
  }
  download.folder.historical <- paste0( download.folder,
                                       "tmpHistorical/" )
  if ( !dir.exists( download.folder.historical ) ){
    dir.create( download.folder.historical, recursive = TRUE )
  }

  ## Extract the content of all files. The content is a list of
  ## xts-class time series and the content of each file (corresponding
  ## to a station) will be an element of the stations.content list.
  if ( !quiet ){
    cat( 'Starting the extraction of the content...\n' )
  }
  stations.content <-
    lapply( seq( 1 , length( list.station.ids ) ), function( ll ){
      if ( !quiet && ( ll %% 10 ) == 0 ){
        cat( paste( "Extracting file", ll, "of",
                   length( list.station.ids ), "\n" ) )
      }
      stations.content[[ ll ]] <- 
        extract.content.climate(
            station.id = list.station.ids[[ ll ]],
            files.list = list( recent = files.list$recent,
                              historical = files.list$historical ),
            download.folder.recent = download.folder.recent,
            download.folder.historical = download.folder.historical )
    } )

  ## Clean up and delete the auxiliary folders used in extraction.
  unlink( download.folder.recent, recursive = TRUE )
  unlink( download.folder.historical, recursive = TRUE )

  ## Now, we restructure the data to have a list representing a
  ## climatological quantity and its elements to correspond to the
  ## individual stations. Beforehand, each list represented a station
  ## and its elements corresponded to the climatological
  ## quantities. Restructuring it this way eases a massive parallel
  ## application in the analysis of e.g. the temperature. The
  ## following object will be a list of all quantities. It will get
  ## split into different entities at a latter point.
  quantities.content <-
    lapply( 1 : length( stations.content[[ 1 ]] ), function( qq )
      lapply( stations.content, function( ss ) ss[[ qq ]] ) )
  
  ## Create a distinct objects for each climatological quantity.
  for ( qq in 1 : length( quantities.content ) ){
    assign( paste0( "dwd.", names( stations.content[[ 1 ]] )[ qq ] ),
           quantities.content[[ qq ]] )
  }
  
  ## assigning the stations names
  if ( !quiet ){
    cat( "Extract the name of the stations...\n" )
  }
  quantities.info <- lapply( list.station.ids, function( x )
    extract.station.name.and.location( x, file.description ) )
  station.names <- Reduce( c, lapply( quantities.info,
                                     function( ss ) ss$name ) )
  station.positions <-
    Reduce( rbind, lapply( quantities.info, function( ss )
      ss$location ) )
  station.positions <- data.frame(
      longitude = station.positions[ , 1 ],
      latitude = station.positions[ , 2 ],
      altitude = station.positions[ , 3 ],
      name = station.names )
  ## Ordering the stations according to their names in alphabetical
  ## order.
  station.positions <- station.positions[ order( station.names ), ]
  
  ## Assigning the names of the stations to the lists of the
  ## climatological quantities.
  for ( ss in paste0( "dwd.", names( stations.content[[ 1 ]] ) ) ){
    ## Get the object of the name ss, names its elements according to
    ## the corresponding stations, and order its content
    ## alphabetically.
    tmp <- get( ss )
    names( tmp ) <- station.names
    tmp <- tmp[ order( station.names ) ]
    assign( ss, tmp )
  }

  ## Writing the data to .csv files
  if ( csv.export ){
    ## Create a distinct folder for each quantity.
    for ( qq in names( stations.content[[ 1 ]] ) ){
      if ( !dir.exists( paste0( download.folder, "csv/",
                               prefix.file.name, '/', qq ) ) ){
        dir.create( paste0( download.folder, "csv/",
                           prefix.file.name, '/', qq ),
                   recursive = TRUE )
      }
      tmp <- get( paste0( "dwd.", qq ) )
      for ( ss in 1 : length( tmp ) )
        utils::write.table( data.frame(
                   date = index( tmp[[ ss ]] ),
                   value = tmp[[ ss ]], row.names = NULL ),
                   file = paste0( download.folder, 'csv/',
                                 prefix.file.name, '/', qq, '/',
                                 gsub( "/", "-",
                                      names( tmp )[ ss ],
                                      fixed = TRUE ), ".csv" ),
                   sep = ",", row.names = FALSE )
    }
  }  
  ## save the extracted data
  save( list = c( paste0( "dwd.", data.type ),
                 "station.positions" ),
       file = paste0( download.folder, "./dwd_",
                     prefix.file.name, '_',
                     gsub( ".", "-", paste( data.name, 
                                           collapse = "_" ),
                          fixed = TRUE ),
                     ".RData" ) )
  invisible( TRUE )
}

##' @title Unzips files and extracts the content of DWD source files
##'   for a single station.
##' @description Reads in the content of both the recent and
##'   historical files corresponding to a single station, combines
##'   its data, and converts it into a format R can handle.
##' @details The station is specified by its \emph{station.id}. The
##'   extraction will take place in either
##'   \strong{download.folder.recent} or
##'   \strong{download.folder.historical}. Before extraction all its
##'   content will be removed.
##'
##' @param station.id String specifying a station.
##' @param files.list Named list of character vectors containing
##'   absolute paths pointing towards the .zip files downloaded from
##'   the DWD FTP server. The names correspond to \emph{recent} and
##'   \emph{historical}.
##' @param download.folder.recent Folder, which will contain the
##'   extracted recent archive.
##' @param download.folder.historical Folder, which will contain the
##'   extracted historical archive.
##'
##' @return Named list of \pkg{xts}-class objects where each
##'   element of the list corresponds to one data column in the data
##'   files.
##' @author Philipp Mueller
extract.content.climate <- function( station.id, files.list,
                                    download.folder.recent,
                                    download.folder.historical ){
  ## Compatibility
  if ( length( station.id ) > 1 ){
    stop( "Please provide only one station ID!" )
  }
  ## Delete the content of the auxiliary folders
  unlink( paste0( download.folder.recent,
                 list.files( download.folder.recent ) ),
         recursive = TRUE )
  unlink( paste0( download.folder.historical,
                 list.files( download.folder.historical ) ),
         recursive = TRUE )
  ## Obtain the index corresponding to the station ID.
  index.recent <- grep( paste0( "_", station.id, "_" ),
                       files.list$recent )
  if ( length( index.recent ) > 1 ){
    stop( "More than one recent station matches the station ID!" )
  }
  index.historical <- grep( paste0( "_", station.id, "_" ),
                           files.list$historical )
  if ( length( index.historical ) > 1 ){
    stop( "More than one historical station matches the station ID!" )
  }
  if ( length( index.recent ) == 0 &&
       length( index.historical ) == 0 ){
    message( paste( "No data at all could be found for station ID",
                   station.id ) )
    return( NULL )
  }
  
  ## If the zip file can not be extracted properly the unzip function
  ## will just raise a warning. It has to be converted to an error
  old.warning.level <- getOption( "warn" )
  options( warn = 2 )
  ## Extract the data
  if ( length( index.recent ) > 0 ){
    try.unzip <- try( utils::unzip( 
                                 files.list$recent[ index.recent ],
                                 exdir = download.folder.recent ), silent = TRUE )
    if ( class( try.unzip ) == "try-error" ){
      stop( paste( "Unable to extract the recent content of station",
                  station.id ) )
    }
  }
  if ( length( index.historical ) > 0 ){
    try.unzip <- try( utils::unzip(
                                 files.list$historical[ index.historical ],
                                 exdir = download.folder.historical ), silent = TRUE )
    if ( class( try.unzip ) == "try-error" ){
      stop( paste( "Unable to extract the historical content of station",
                  station.id ) )
    }
  }
  ## Revert the altering of the warning level
  options( warn = old.warning.level )

  ## Will contain lists of xts-class objects representing the time
  ## series extracted from the files.
  content.list <- list()
  ## Extract the content from all "produkt_klima_*" files.
  if ( length( index.recent ) > 0 ){
    content.files <- grep(
        "produkt", paste0( download.folder.recent,
                          list.files( download.folder.recent ) ),
        value = TRUE )
    if ( length( content.files ) == 0 ){
      warning( paste( "No recent produkt_* files found for station",
                     station.id ) )
    } else {
      ## Append the content of the individual files to the content
      ## list.
      content.list <-
        c( content.list,
          lapply( content.files, function( ff )
            import.file.content.climate( ff ) ) )
    }
  }
  if ( length( index.historical ) > 0 ){
    content.files <- grep(
        "produkt", paste0( download.folder.historical,
                          list.files( download.folder.historical ) ),
        value = TRUE )
    if ( length( content.files ) == 0 ){
      warning( paste( "No historical produkt_* files found for station",
                     station.id ) )
    } else {
      ## Append the content of the individual files to the content
      ## list.
      content.list <-
        c( content.list,
          lapply( content.files, function( ff )
            import.file.content.climate( ff ) ) )
    }
  }

  ## Check whether all lists are compatible and feature the same
  ## amount of climatological quantities
  if ( !all( Reduce( c, lapply( 1 : length( content.list ),
                               function( ll )
                                 names( content.list[[ 1 ]] ) ==
                                 names( content.list[[ ll ]] ) )
                    ) ) ){
    warning( paste( "extracted content does not match for station",
                   station.id ) )
  }
  ## Merge all time series concerning one climatological variable into
  ## one and provide them as a list of class xts elements.
  ## Start with the first list of results and add all data of the
  ## latter, which was not part of the former one.
  result.list <- content.list[[ 1 ]]
  if ( length( content.list ) > 1 ){
    for ( ll in 2 : length( content.list ) ){
      result.list <- lapply( 1 : length( result.list ), function( rr ){
        ## Add all time stamps, which are not present yet.
        if ( all( is.na( content.list[[ ll ]][[ rr ]] ) ) ||
             all( is.na( result.list[[ rr ]] ) ) ){
          ## The c.xts function will complain if one of the objects
          ## solely consists of NAs.
          suppressWarnings({
            res <- c( result.list[[ rr ]],
                     content.list[[ ll ]][[ rr ]][
                         !index( content.list[[ ll ]][[ rr ]] ) %in%
                         index( result.list[[ rr ]] ) ] ) })
        } else {
          res <- c( result.list[[ rr ]],
                   content.list[[ ll ]][[ rr ]][
                       !index( content.list[[ ll ]][[ rr ]] ) %in%
                       index( result.list[[ rr ]] ) ] )
        }
        return( res ) } )
    }
  }
  ## The name got lost during the merging of the content.
  names( result.list ) <- names( content.list[[ 1 ]] )
  return( result.list )
}

##' @title Extract the names and the position of the individual
##'   stations from the description file.
##' @description The DWD has a peculiar format for their description
##'   files. This function will serve as a wrapper to look up the name
##'   and the location of a station, specified using its
##'   \strong{station.id}, inside a description file.
##' 
##' @param station.id String specifying a station.
##' @param file.description Path to a description file matching the
##'   station IDs and their actual names.
##'
##' @return A list containing a string with the name of the station
##'   as one element and a numerical vector of length three (
##'   longitude, latitude, altitude ) as the second element.
##' @author Philipp Mueller
extract.station.name.and.location <- function( station.id,
                                              file.description ){
  ## The station ID is placed in the first column and the date of
  ## the beginning of the observation period is placed in the second
  ## column. With just one line between the first and second column
  ## and the necessity of the second to start with either a 1 or a 2,
  ## the station ID can be extracted uniquely.
  ## The conversion to numeric and back is necessary to delete zeros
  line.raw <- grep( paste0( station.id, " [1,2]" ),
                   file.description, value = TRUE )
  if ( length( line.raw ) == 0 ){
    ## The extraction of the name fails, because the DWD has not yet
    ## added the station to the Description file holding of the
    ## stations meta data. Or it removed the corresponding line in the
    ## description file but kept the data.
    if ( station.id == "00738" ){
      ## For this station the meta data were removed.
      return( list( name = "Br\uFCckenau, Bad (A)",
                   location = c( 9.79, 50.31, 314 ) ) )
    }
    warning( "Some of the station data are not contained in the overall description file. Please make sure you have the most recent data!" )
    ## Worst case, just delete the corresponding file and download
    ## the new one.
    return( list( name = "unknown",
                 location = c( NA, NA, NA ) ) )
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
  return( list( name = station.name,
               location = c( as.numeric( line[ 6 ] ),
                            as.numeric( line[ 5 ] ),
                            as.numeric( line[ 4 ] )
                            ) ) )
}

##' @title Imports the content of a single produkt file into R
##' @description It does the actual conversion of the format used by
##'   the DWD to the \pkg{xts} class. This function is intend to be
##'   used for aggregated stations data.
##'
##' @param file.path Path to a single produkt_* file contained in the
##'   zip archives of the DWD.
##'
##' @return A time series of class \pkg{xts}.
##' @author Philipp Mueller
import.file.content.climate <- function( file.path ){
  ## Sometimes there is a single delimiter symbol in the last line.
  ## This causes the read.table function to throw a warning and, if
  ## the file to read just consists of one line, to fail. Therefore,
  ## it has to be avoided by checking how many characters are present
  ## in the last line.
  file.content <- readLines( file.path )
  if ( nchar( file.content[ length( file.content ) ] ) < 10 ){
    ## only the last line with the potential delimiter will be
    ## omitted. Minus two, because we have to omit the header as well.
    file.data <-
      utils::read.table(
                 file.path, header = TRUE, sep = ";",
                 nrows = ( length( file.content ) - 2 ) )
  } else {
    file.data <- utils::read.table( file.path, header = TRUE,
                                   sep = ";" )
  }
  ## Converting the data into a list of xts-class objects. Each data
  ## column will constitute an element in the list. Since the last
  ## column only contains the "eor" (end of row) delimiters, it will
  ## be skipped.
  results.list <- lapply( seq( 3, ncol( file.data ) - 1 ),
                         function( rr ){
                           xts( file.data[ , rr ],
                               order.by = convert.date.integer(
                                   file.data[ , 2 ] ) ) } )
  ## Artifacts in the DWD data base are stored as -999. These will be
  ## converted to NA (not available).
  results.list <- lapply( results.list, function( ll ){
    ll[ ll == -999 ] <- NA
    return( ll ) } )

  ## Use the header of the content file to name the list containing
  ## the results.
  if ( all( names( file.data ) ==
            c( "STATIONS_ID", "MESS_DATUM", "QN_3", "FX", "FM",
              "QN_4", "RSK", "RSKF", "SDK", "SHK_TAG", "NM", "VPM",
              "PM", "TMK", "UPM", "TXK", "TNK", "TGK", "eor" ) ) ){
    names( results.list ) <-
      c( "wind.gust.quality", "wind.gust.max", "wind.gust.mean",
        "quality.general", "precipitation.height",
        "precipitation.form", "sunshine.duration", "snow.depth",
        "cloud.cover.mean", "vapour.pressure.mean", "pressure.mean",
        "temperature.mean", "relative.humidity.mean",
        "temperature.2m.max", "temperature.2m.min",
        "temperature.5cm.min" )

  } else {
    warning( "Unknown column names in the extracted files" )
    names( results.list ) <- names( file.data )[
        seq( 3, ncol( file.data ) - 1 ) ]
  }
  
  return( results.list )
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
